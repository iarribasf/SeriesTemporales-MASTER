#----------------------------------------------------------
# CODIGO TEMA 4
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(lmtest)
library(tseries)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Residuos
residuos <- read.csv2("./series/Residuos.csv", 
                      header = TRUE)

residuos <- ts(residuos[, 2], 
               start = 1995, 
               frequency  = 1)

autoplot(residuos,
         xlab = "", 
         ylab = "Kg per cápita", 
         main = "")

# Aforo vehículos
aforo <- read.csv2("./series/Aforo_oropesa.csv", 
                   header = TRUE)
aforo <- ts(aforo, 
            start = 1960, 
            freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos diarios",
         main = "")

# Consumo de alimentos per capita
alimentospc <- read.csv2("./series/Alimentacionpc.csv",
                         header = TRUE)

alimentospc <- ts(alimentospc,
                  start = 1990, 
                  freq = 1)

autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "",
         ylim = c(0, 700))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Residuos
#----------------------------------------------------------
# Transformacion
autoplot(residuos, xlab = "", ylab = "", main = "")
autoplot(diff(residuos), xlab = "", ylab = "", main = "")

ggAcf(residuos, xlab = "", ylab = "", main = "")
ggAcf(diff(residuos), xlab = "", ylab = "", main = "")

ndiffs(residuos)

# Identificacion
ggtsdisplay(diff(residuos), main = "")

auto.arima(residuos, 
           d = 1,
           trace = TRUE)

# Estimacion + Intervencion
arima110 <- Arima(residuos, 
                  order = c(1, 1, 0), 
                  include.constant = FALSE)
arima110

error <- residuals(arima110)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1995, 2023, 2)) 

# Validacion: coeficientes significativos
coeftest(arima110)

# Validacion: medidas de error
accuracy(arima110)

# Validacion: hipotesis sobre el residuo
error <- residuals(arima110)
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
shapiro.test(error)

# Prediccion
parima110 <- forecast(arima110, 
                      h = 5, 
                      level = 95)
parima110

autoplot(parima110, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "") +
  scale_x_continuous(breaks= seq(1999, 2027, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Aforo
#----------------------------------------------------------
# Transformacion
autoplot(aforo, xlab = "", ylab = "", main = "")
autoplot(diff(aforo), xlab = "", ylab = "", main = "")

ggAcf(aforo, xlab = "", ylab = "", main = "")
ggAcf(diff(aforo), xlab = "", ylab = "", main = "")

ndiffs(aforo)

# Identificacion
ggtsdisplay(diff(aforo), 
            main = "Aforo (una diferencia)")

auto.arima(aforo,
           d = 1)

# Estimacion + Intervencion
arima010 <- Arima(aforo, 
                  order = c(0, 1, 0),
                  include.constant = FALSE)

error <- residuals(arima010)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2024, 4)) 

fechas <- format(seq(as.Date("1960-01-01"), as.Date("2023-01-01"), "year"), "%Y")
fechas[abs(error) > 2.5 * sderror]

d1979 <- 1*(time(error) == 1979)
d2011 <- 1*(time(error) == 2011)
d2020 <- 1*(time(error) == 2020)

auto.arima(aforo,
           d = 1,
           xreg = cbind(d1979,  d2011, d2020))

arima210 <- Arima(aforo, 
                  order = c(2, 1, 0), 
                  xreg = cbind(d1979,  d2011, d2020))
arima210

error <- residuals(arima210)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1960, 2024, 4)) 

# Validacion: coeficientes significativos
coeftest(arima210)

# Validacion: medidas de error
accuracy(arima210)

# Validacion: hipotesis sobre el residuo
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
jarque.bera.test(error)

# Prediccion
parima210 <- forecast(arima210, 
                      h = 4, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 4), d2011=rep(0, 4),
                                   d2020=rep(0, 4)))
parima210

autoplot(parima210, 
         xlab = "",
         ylab = "Vehículos diarios",
         main = "") +
  scale_x_continuous(breaks= seq(1960, 2028, 4)) 

# Validación con origen de predicción móvil
k <- 30                  
h <- 4                  
T <- length(aforo)     
s <- T - k - h    

mapeArima <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(aforo, start = i + 1, end = i + k)
  test.set <-  subset(aforo, start = i + k + 1, end = i + k + h) 
  
  fit <- Arima(train.set, 
               include.constant = FALSE,
               order = c(2, 1, 0))
  
  fcast <- forecast(fit, h = h)
  
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeArima <- apply(mapeArima, MARGIN = 2, FUN = median)
mapeArima
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Consumo de alimentos per cápita
#----------------------------------------------------------
# Transformacion
autoplot(alimentospc, xlab = "", ylab = "", main = "")
autoplot(diff(alimentospc), xlab = "", ylab = "", main = "")

ggAcf(alimentospc, xlab = "", ylab = "", main = "")
ggAcf(diff(alimentospc), xlab = "", ylab = "", main = "")

ndiffs(alimentospc)

# Identificacion 
ggtsdisplay(alimentospc)

auto.arima(alimentospc,
           d = 0)

# Estimacion + Intervencion
arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0),
                  include.constant = TRUE)

error <- residuals(arima100)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1990, 2024, 2)) 

fechas <- format(seq(as.Date("1990-01-01"), as.Date("2023-01-01"), by = "year"), "%Y")
fechas[abs(error) > 2.5 * sderror]

d2020 <- 1* (time(alimentospc) == 2020)
d2022 <- 1* (time(alimentospc) == 2022)

#auto.arima(alimentospc, d = 0, xreg = cbind(d2020, d2022))

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0),
                  include.constant = TRUE,
                  xreg = cbind(d2020, d2022))
arima100

error <- residuals(arima100)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1990, 2024, 4)) 

fechas[abs(error) > 2.5 * sderror]

d2023 <- 1* (time(alimentospc) == 2023)

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0),
                  include.constant = TRUE,
                  xreg = cbind(d2020, d2022, d2023))
arima100

error <- residuals(arima100)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1990, 2024, 4)) 

# Validacion: coeficientes significativos
coeftest(arima100)

# Validacion: medidas de error
accuracy(arima100)

# Validacion: hipotesis sobre el residuo
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
jarque.bera.test(error)

# Prediccion
parima100 <- forecast(arima100, 
                      h = 5, 
                      level = 95,
                      xreg = cbind(rep(0, 5), rep(0, 5), rep(0, 5)))
parima100

parima100b <- forecast(arima100, 
                       h = 5, 
                       level = 95,
                       xreg = cbind(rep(0, 5), rep(0, 5), rep(1, 5)))
parima100b

autoplot(alimentospc, 
         series = "Alimentos",
         xlab = "",
         ylab = "Kg per cápita",
         main = "",
         PI = FALSE,
         ylim = c(300, 700)) +
  autolayer(parima100,  PI = FALSE, series = "Aumento ocio desaparece") +
  autolayer(parima100b, PI = FALSE, series = "Aumento ocio se mantiene") +
  scale_x_continuous(breaks= seq(1990, 2028, 4)) +
  scale_colour_discrete(limits=c("Alimentos", "Aumento ocio desaparece", 
                                 "Aumento ocio se mantiene")) +
  labs(colour="Predicciones") + 
  theme(legend.position=c(0.2,0.2))

