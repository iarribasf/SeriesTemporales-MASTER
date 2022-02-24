#---------------------------------------------------------------
# Codigo Tema 4
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)
library(tseries)
library(aod)

#- Analisis de Libros
libros <- read.csv2("series/libros.csv", header = TRUE)
libros <- ts(libros[, 2], start = 1993, frequency = 1)

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
aforo <- read.csv2("series/aforo_oropesa.csv", header = TRUE)
aforo <- ts(aforo, start = 1960, freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos (000)",
         main = "Aforo de vehículos en N-340, Oropesa")


autoplot(aforo,xlab = "log(Aforo)", ylab = "", main = "")
autoplot(diff(aforo), xlab = "Una diferencia de log(Aforo)", ylab = "", main = "")
autoplot(diff(aforo, differences = 2), xlab = "Dos diferencias de log(Aforo)", ylab = "", main = "")

ggAcf(aforo)
ggAcf(diff(aforo))
ggAcf(diff(aforo, differences = 2))

ndiffs(aforo)

ggtsdisplay(diff(aforo, differences = 1), 
            main = "Aforo (una diferencia)")

auto.arima(aforo)

arima212 <- Arima(aforo, 
                  order = c(2, 1, 2))

error <- residuals(arima212)
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

time(error)[abs(error) > 2.5*sderror]

d1979 <- 1*(time(error) == 1979)
d2011 <- 1*(time(error) == 2011)

auto.arima(aforo,
           xreg = cbind(d1979,  d2011))

arima210 <- Arima(aforo, 
                  order = c(2, 1, 0), 
                  xreg = cbind(d1979,  d2011))
arima210

wald.test(b = coef(arima210), Sigma = vcov(arima210), Terms = 1)
wald.test(b = coef(arima210), Sigma = vcov(arima210), Terms = 2)
wald.test(b = coef(arima210), Sigma = vcov(arima210), Terms = 3)
wald.test(b = coef(arima210), Sigma = vcov(arima210), Terms = 4)

error <- residuals(arima210)
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

accuracy(arima210)
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
jarque.bera.test(error)

parima210 <- forecast(arima210, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 5), d1981=rep(0, 5)))
parima210

autoplot(parima210, 
         ylab = 'Vehículos (000)',
         main = 'Aforo (1960-2018) y predicción (2019-2023)') +
  scale_x_continuous(breaks= seq(1960, 2023, 4)) 

k <- 30                  
h <- 5                    
T <- length(aforo)     
s <- T - k - h    

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(d1979, d2011))

for (i in 0:s) {
  train.set <- subset(aforo, start = i + 1, end = i + k)
  test.set <-  subset(aforo, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     order = c(2, 1, 0),
                     xreg=as.matrix(X.train)))
  } else {
    fit <- try(Arima(train.set, 
                     order = c(2, 1, 0)))
  }
  
  if (!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) 
      fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
        fcast <- forecast(fit, h = h)
      mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

mapeArima <- colMeans(mapeArima, na.rm = TRUE)
mapeArima

#- Analisis Consumo de alimentos per cápita
alimentospc <- read.csv2("series/alimentacionpc.csv", header = TRUE)
alimentospc <- ts(alimentospc, start = 1987, freq = 1)
    
autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "Consumo alimentario en hogar")

autoplot(alimentospc, xlab = "", ylab = "", main = "Alimentos")
autoplot(diff(alimentospc), xlab = "", ylab = "", main = "Diferencia alimentos")

ggAcf(alimentospc)
ggAcf(diff(alimentospc))

ndiffs(alimentospc)

ggtsdisplay(alimentospc, main = "Consumo de alimentos per cápita")

auto.arima(alimentospc)

arima100 <- Arima(alimentospc, 
                  include.constant = TRUE,
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

d1993 <- 1* (time(alimentospc) == 1993)
d1995 <- 1* (time(alimentospc) == 1995)
d2009 <- 1* (time(alimentospc) == 2009)

arima100 <- Arima(alimentospc, 
                  include.constant = TRUE,
                  order = c(1, 0, 0),
                  xreg = cbind(d1993, d1995, d2009))
arima100

wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 1)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 2)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 3)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 4)
wald.test(b = coef(arima100), Sigma = vcov(arima100), Terms = 5)

accuracy(arima100)

error <- residuals(arima100)
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
jarque.bera.test(error)

parima100 <- forecast(arima100, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(rep(0, 5), rep(0, 5), rep(0, 5)))
parima100
autoplot(parima100, 
     ylab = "Kilos per cápita",
     main = "Consumo de alimentos y predicción") +
  scale_x_continuous(breaks= seq(1987, 2023, 4)) 

