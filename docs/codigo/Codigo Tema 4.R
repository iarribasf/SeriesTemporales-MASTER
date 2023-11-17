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
# Libros
libros <- read.csv2("./series/libros.csv", 
                    header = TRUE)

libros <- ts(libros[, 2], 
             start = 1993, 
             frequency = 1)

autoplot(libros,
         xlab = "", 
         ylab = "", 
         main = "")

# Aforo vehículos
aforo <- read.csv2("./series/aforo_oropesa.csv", 
                   header = TRUE)

aforo <- ts(aforo, 
            start = 1960, 
            freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos (000)",
         main = "")

# Consumo de alimnetos per capita
alimentospc <- read.csv2("./series/alimentacionpc.csv", 
                         header = TRUE)

alimentospc <- ts(alimentospc, 
                  start = 1990, 
                  freq = 1)

autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Libros
#----------------------------------------------------------
# Transformacion
autoplot(libros, xlab = "", ylab = "", main = "")
autoplot(diff(libros), xlab = "", ylab = "", main = "")

ggAcf(libros, xlab = "", ylab = "", main = "")
ggAcf(diff(libros), xlab = "", ylab = "", main = "")

ggtsdisplay(diff(libros), main = "")

ndiffs(libros)

# Estimacion
arima010 <- Arima(libros, 
                  order=c(0, 1, 0), 
                  include.constant = TRUE)
arima010

coeftest(arima010)

arima010 <- Arima(libros, 
                  order=c(0, 1, 0), 
                  include.constant = FALSE)
arima010

# Error de ajuste
accuracy(arima010)

# Hipotesis sobre el residuo
error <- residuals(arima010)
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
shapiro.test(error)
jarque.bera.test(error)

# Intervencion
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
  scale_x_continuous(breaks= seq(1993, 2019, 2)) 

# Prediccion
parima010 <- forecast(arima010, 
                      h = 5, 
                      level = 95)
parima010

autoplot(parima010, 
         xlab = "", 
         ylab = "Títulos",
         main = "") +
  scale_x_continuous(breaks= seq(1993, 2024, 2)) 

# Identificacion
auto.arima(libros, 
           d = 1, 
           trace = TRUE)
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
ggtsdisplay(diff(aforo, differences = 1), 
            main = "Aforo (una diferencia)")

auto.arima(aforo,
           d = 1)

# estimacion + Intervencion
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
  scale_x_continuous(breaks= seq(1960, 2020, 4)) 

time(error)[abs(error) > 2.5*sderror]

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
  scale_x_continuous(breaks= seq(1960, 2020, 4)) 

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
                      h = 5, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 5), d2011=rep(0, 5),
                                  d2020=rep(0, 5)))

parima210

autoplot(parima210, 
         xlab = "",
         ylab = "Vehículos (000)",
         main = "") +
  scale_x_continuous(breaks= seq(1960, 2024, 4)) 

# Validación con origen de predicción móvil
k <- 30                  
h <- 5                    
T <- length(aforo)     
s <- T - k - h    

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(d1979, d2011, d2020))

for (i in 0:s) {
  train.set <- subset(aforo, start = i + 1, end = i + k)
  test.set <-  subset(aforo, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  
  hay <- colSums(X.train)  
  
  if(sum(hay) == 0) {
    X.train <- NULL
    X.test <- NULL
  } else {
    X.train <- as.matrix(X.train[, hay>0])
    X.test <- as.matrix(X.test[, hay>0])
  }
  
  fit <- Arima(train.set, 
               include.constant = FALSE,
               order = c(2, 1, 0),
               xreg = X.train)
  
  fcast <- forecast(fit, h = h, xreg = X.test)
  
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeArima <- colMeans(mapeArima)
mapeArima
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Consumo de alimentos per cápita
#----------------------------------------------------------
# Transformacion
autoplot(alimentospc, xlab = "", ylab = "", main = "Alimentos")
autoplot(diff(alimentospc), xlab = "", ylab = "", main = "Diferencia alimentos")

ggAcf(alimentospc)
ggAcf(diff(alimentospc))

ndiffs(alimentospc)

# Identificacion + Intervencion
ggtsdisplay(alimentospc, main = "Consumo de alimentos per cápita")

auto.arima(alimentospc,
           d = 0)

arima001 <- Arima(alimentospc, 
                  order = c(0, 0, 1),
                  include.constant = TRUE)

error <- residuals(arima001)
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
  scale_x_continuous(breaks= seq(1990, 2022, 3)) 

time(alimentospc)[abs(error) > 2.5 * sderror]

d2020 <- 1* (time(alimentospc) == 2020)
auto.arima(alimentospc, d = 0, xreg = d2020)

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0),
                  include.constant = TRUE,
                  xreg = cbind(d2020))

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
  scale_x_continuous(breaks= seq(1990, 2022, 3)) 

time(alimentospc)[abs(error) > 2.5 * sderror]

d1993 <- 1* (time(alimentospc) == 1993)
d1995 <- 1* (time(alimentospc) == 1995)
d1999 <- 1* (time(alimentospc) == 1999)
d2022 <- 1* (time(alimentospc) == 2022)

arima100 <- Arima(alimentospc, 
                  order = c(1, 0, 0),
                  include.constant = TRUE,
                  xreg = cbind(d1993, d1995, d1999, d2020, d2022))

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
  scale_x_continuous(breaks= seq(1990, 2022, 3)) 

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
                      xreg = cbind(rep(0, 5), rep(0, 5), rep(0, 5), 
                                   rep(0, 5), rep(0, 5)))
parima100

autoplot(parima100, 
         xlab = "",
         ylab = "Kilos per cápita",
         main = "") +
  scale_x_continuous(breaks= seq(1990, 2026, 4)) 

