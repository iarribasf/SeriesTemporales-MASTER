#---------------------------------------------------------------
# Codigo Tema 4: junio 2020
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)
library(tseries)
library(aod)

#- Analisis de Libros
libros <- read.csv2("libros.csv", header = TRUE)
libros <- ts(libros["libros"], start = 1993, frequency = 1)

autoplot(libros, xlab = "", ylab = "", main = "Libros")
autoplot(diff(libros), xlab = "", ylab = "", main = "Diferencia libros")

ggAcf(libros, xlab = "", ylab = "FAC", main = "")
ggAcf(diff(libros), xlab = "", ylab = "FAC", main = "")

ggtsdisplay(diff(libros), main = "Libros (primera diferencia)")

arima010 <- Arima(libros, 
                 order=c(0, 1, 0), 
                 include.constant = TRUE)
arima010

wald.test(b = coef(arima010), 
          Sigma = vcov(arima010), 
          Terms = 1)

arima010 <- Arima(libros, 
                 order=c(0, 1, 0), 
                 include.constant = FALSE)
arima010

accuracy(arima010)

error <- residuals(arima010)
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
shapiro.test(error)
jarque.bera.test(error)

sderror <- sd(error)
autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1993, 2019, 2)) 

parima010 <- forecast(arima010, h = 5, level = 95)
parima010
autoplot(parima010, 
         xlab = "", 
         ylab = "Títulos",
         main = "Libros (1993-2018) y predicción (2019-2023)") +
  scale_x_continuous(breaks= seq(1993, 2023, 2)) 

auto.arima(libros, trace = TRUE)

#- Analisis Aforo
aforo <- read.csv2("aforo_oropesa.csv", header = TRUE)
aforo <- ts(aforo, start = 1960, freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos (000)",
         main = "Aforo de vehículos en N-340, Oropesa")


autoplot(log(aforo),xlab = "log(Aforo)", ylab = "", main = "")
autoplot(diff(log(aforo)), xlab = "Una diferencia de log(Aforo)", ylab = "", main = "")
autoplot(diff(log(aforo), differences = 2), xlab = "Dos diferencias de log(Aforo)", ylab = "", main = "")

ggAcf(log(aforo))
ggAcf(diff(log(aforo)))
ggAcf(diff(log(aforo), differences = 2))

ggtsdisplay(diff(log(aforo), differences = 2), 
            main = "Aforo (log y dos diferencias)")

auto.arima(aforo, lambda = 0)

arima022 <- Arima(aforo, 
                  order = c(0, 2, 2),
                  lambda = 0)

error <- residuals(arima022)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2014, 4)) 

d1979 <- 1*(time(error) == 1979)
d1981 <- 1*(time(error) == 1981)
d1984 <- 1*(time(error) == 1984)
d2011 <- 1*(time(error) == 2011)

auto.arima(aforo, lambda = 0, xreg = cbind(d1979, d1981, d1984, d2011))

arima120 <- Arima(aforo, 
                  order = c(1, 2, 0), 
                  lambda = 0,  
                  xreg = cbind(d1979, d1981, d1984, d2011))
arima120

wald.test(b = coef(arima120), Sigma = vcov(arima120), Terms = 1)
wald.test(b = coef(arima120), Sigma = vcov(arima120), Terms = 2)
wald.test(b = coef(arima120), Sigma = vcov(arima120), Terms = 3)
wald.test(b = coef(arima120), Sigma = vcov(arima120), Terms = 4)
wald.test(b = coef(arima120), Sigma = vcov(arima120), Terms = 5)

error <- residuals(arima120)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2014, 4)) 

accuracy(arima120)
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
jarque.bera.test(error)

parima120 <- forecast(arima120, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 5), d1981=rep(0, 5), 
                                   d1984=rep(0, 5), d2011=rep(0, 5)))
parima120

autoplot(parima120, 
     ylab = 'Vehículos (000)',
     main = 'Aforo (1960-2018) y predicción (2019-2023)') +
  scale_x_continuous(breaks= seq(1960, 2023, 4)) 

#- Analisis Consumo de alimentos per cápita
alimentospc <- read.csv2("alimentacionpc.csv", header = TRUE)
alimentospc <- ts(alimentospc, start = 1987, freq = 1)
    
autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "Consumo alimentario en hogar")

autoplot(alimentospc, xlab = "", ylab = "", main = "Alimentos")
autoplot(diff(alimentospc), xlab = "", ylab = "", main = "Diferencia alimentos")

ggAcf(alimentospc)
ggAcf(diff(alimentospc))

ggtsdisplay(alimentospc, main = "Consumo de alimentos per cápita")

auto.arima(alimentospc)

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0))

error <- residuals(arima100)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1987, 2018, 3)) 

wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 1)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 2)

accuracy(arima100)
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
shapiro.test(error)

parima100 <- forecast(arima100, h = 5, level = 95)
parima100
autoplot(parima100, 
     ylab = "Kilos per cápita",
     main = "Consumo de alimentos y predicción") +
  scale_x_continuous(breaks= seq(1987, 2023, 4)) 

