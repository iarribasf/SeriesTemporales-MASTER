#----------------------------------------------------------
# CODIGO TEMA 5
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(lmtest)
library(seasonal)
library(tseries)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  freq = 12)

nacimientos <- window(nacimientos, start = 2000)
autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Exportaciones
exportaciones <- read.csv2("./series/Exportaciones.csv", 
                           header = TRUE)

exportaciones <- ts(exportaciones,
                    start = c(1999, 1),
                    freq = 12)

autoplot(exportaciones,
         xlab = "",
         ylab = "Millones de €",
         main = "")

# Chocolate
chocolate <- read.csv2("./series/Chocolate.csv", 
                       header = TRUE)

chocolate <- ts(chocolate, 
                start = 1958, 
                freq = 12)

autoplot(chocolate, 
         xlab = "", 
         ylab = "Toneladas",
         main = "Producción de Chocolate (Australia)")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Nacimientos
#----------------------------------------------------------
# Estacionariedad y ergodicidad
ggAcf(log(nacimientos), lag = 48, ylim = c(-1, 1), 
      xlab = "", ylab = "", main = "")
ggAcf(diff(log(nacimientos)), lag = 48, ylim = c(-1, 1), 
      xlab = "", ylab = "", main = "")
ggAcf(diff(log(nacimientos), lag = 12), lag = 48, ylim = c(-1, 1), 
      xlab = "", ylab = "", main = "")
ggAcf(diff(diff(log(nacimientos), lag = 12)), lag = 48, ylim = c(-1, 1), 
      xlab = "", ylab = "", main = "")

# Identificación
ggtsdisplay(diff(diff(log(nacimientos), lag = 12)), lag = 48)

monthdays(nacimientos)
easter(nacimientos)

DiasMes <- monthdays(nacimientos)
SemanaSanta <- easter(nacimientos)
d0111 <- 1*(cycle(nacimientos) == 1  & trunc(time(nacimientos)) == 2011)
d1220 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2020)
d0221 <- 1*(cycle(nacimientos) == 2  & trunc(time(nacimientos)) == 2021)
d0321 <- 1*(cycle(nacimientos) == 3  & trunc(time(nacimientos)) == 2021)

auto.arima(nacimientos, 
           d = 1, 
           D = 1, 
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta, d0111, d1220, d0221, d0321))

summary(seas(nacimientos))
summary(seas(nacimientos, transform.function = "log"))

# Estimación e intervencion
d1210 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2010)

l1220 <- 1*(trunc(time(nacimientos)) > 2020) + 
  1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2020)

l0221 <- 1*(trunc(time(nacimientos)) > 2021) + 
  1*(cycle(nacimientos) >= 2 & trunc(time(nacimientos)) == 2021)

nac.ar1 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, d1210,  d0111, l1220, l0221))
nac.ar1

error <- residuals(nac.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 

d1206 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2006)
d0416 <- 1*(cycle(nacimientos) == 4 & trunc(time(nacimientos)) == 2016)
d1120 <- 1*(cycle(nacimientos) == 11 & trunc(time(nacimientos)) == 2020)
d1122 <- 1*(cycle(nacimientos) == 11 & trunc(time(nacimientos)) == 2022)

nac.ar2 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d1210, d0111, d0416, d1120, d1122,
                              l1220, l0221))
nac.ar2

error <- residuals(nac.ar2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 

# Compensacion
d12100111 <- d1210 - d0111
l12200221 <- l1220 - l0221

nac.ar3 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d12100111, d0416, d1120, d1122,
                              l12200221))
nac.ar3

# Significatividad
coeftest(nac.ar3)

# Error de ajuste
accuracy(nac.ar3)

# Hipotesis sobre el residuo
error <- residuals(nac.ar3)

Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

jarque.bera.test(error) 

ggAcf(error, lag = 36, ylim = c(-0.3, 0.3), main = "")

# Predicción
tmp <- ts(rep(0, 48), start = 2023, freq = 12)
pdm <- monthdays(tmp)
pss <- easter(tmp)
pnac.ar3 <- forecast(nac.ar3, 
                     h = 48,
                     xreg = cbind(pdm, pss, 
                                  rep(0,48), rep(0,48), 
                                  rep(0,48), rep(0,48),
                                  rep(0,48), rep(0,48)), 
                     level = 95)
pnac.ar3

autoplot(pnac.ar3, 
         ylab = "Nacimientos",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2026, 4)) 

# Error de predicción extra-muestral origen de prediccion movil
k <- 120                   
h <- 12                    
T <- length(nacimientos)   
s<-T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasMes, SemanaSanta, d1206, d12100111, 
                      d0416, d1120, d1122, l12200221))

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h) 
  
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
  
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Exportaciones
#----------------------------------------------------------
# Estacionariedad y ergodicidad
ggAcf(log(exportaciones), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(exportaciones)), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(exportaciones), lag = 12), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(diff(log(exportaciones), lag = 12)), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ndiffs(log(exportaciones))
nsdiffs(log(exportaciones))

# Identificación
auto.arima(exportaciones,
           d = 1,
           D = 1,
           lambda = 0,
           xreg = cbind(monthdays(exportaciones), easter(exportaciones)))

summary(seas(exportaciones))

# Estimación e intervencion
DiasLaborables <- bizdays(exportaciones, FinCenter = "London")
SemanaSanta <- easter(exportaciones)

l1208 <- 1*(trunc(time(exportaciones)) > 2008) + 
  1*(cycle(exportaciones) == 12 & trunc(time(exportaciones)) == 2008)

l0320 <- 1*(trunc(time(exportaciones)) > 2020) + 
  1*(cycle(exportaciones)   > 2 & trunc(time(exportaciones)) == 2020)

d0420 <- 1*(cycle(exportaciones) ==  4 & trunc(time(exportaciones)) == 2020)
d0520 <- 1*(cycle(exportaciones) ==  5 & trunc(time(exportaciones)) == 2020)

exp.ar1 <- Arima(exportaciones, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasLaborables, SemanaSanta, 
                              l1208, l0320, d0420, d0520))
exp.ar1

error <- residuals(exp.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1998, 2022, 2)) 

# Validacion
coeftest(exp.ar1)

# Error de ajuste
accuracy(exp.ar1)

# Hipotesis sobre el residuo
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

jarque.bera.test(error) 

# Error de predicción extra-muestral origen de prediccion movil
k <- 120                   
h <- 12                    
T <- length(exportaciones)   
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasLaborables, SemanaSanta, 
                      l1208, l0320, d0420, d0520))

for (i in 0:s) {
  train.set <- subset(exportaciones, start = i + 1, end = i + k)
  test.set <-  subset(exportaciones, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  
  hay <- colSums(X.train)  
  
  tmp <- which(hay == k | hay == 0)
  if(length(tmp) > 0) {
    X.train <- X.train[, -tmp]  
    X.test <- X.test[, -tmp]  
    hay <- hay[-tmp]
  }
  
  if(length(hay) == 0) {
    X.train <- NULL
    X.test <- NULL
  } else {
    X.train <- as.matrix(X.train)
    X.test <- as.matrix(X.test)
  }
  
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

# Predicción
tmp <- ts(rep(0, 48), start = 2023, freq = 12)
pdl <- bizdays(tmp, FinCenter = "London")
pss <- easter(tmp)
pexp.ar1 <- forecast(exp.ar1, 
                     h = 48,
                     xreg = cbind(pdl, pss, 
                                  rep(1,48), rep(1,48), 
                                  rep(0,48), rep(0,48)), 
                     level = 95)
pexp.ar1

autoplot(pexp.ar1, 
         xlab = "",
         ylab = "Millones de euros",
         main = "",
         PI = FALSE) +
  scale_x_continuous(breaks= seq(1998, 2026, 4))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Chocolate
#----------------------------------------------------------
# Transformación
ggAcf(log(chocolate), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(chocolate)), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(chocolate), lag = 12), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(diff(log(chocolate), lag = 12)), lag = 48, xlab = "", ylab = "", main = "")

ndiffs(chocolate)
nsdiffs(chocolate)

# Identificación, Intervencion y Estimacion
DiasLaborables <- bizdays(chocolate, FinCenter = "London")
DiasLaborables

auto.arima(chocolate, d = 1, D = 1, 
           lambda = 0,
           xreg = cbind(DiasLaborables))

summary(seas(diff(diff(log(chocolate), lag = 12))))

choco.ar1 <- Arima(chocolate, order=c(1, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = cbind(DiasLaborables))
choco.ar1

error <- residuals(choco.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1958, 1994, 2)) 

d0975 <- 1*(cycle(chocolate) == 9 & trunc(time(chocolate)) == 1975)
d0186 <- 1*(cycle(chocolate) == 1 & trunc(time(chocolate)) == 1986)
d0191 <- 1*(cycle(chocolate) == 1 & trunc(time(chocolate)) == 1991)
d0194 <- 1*(cycle(chocolate) == 1 & trunc(time(chocolate)) == 1994)

choco.ar2 <- Arima(chocolate, order=c(1, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = cbind(DiasLaborables, d0975, d0186, d0191, d0194))
choco.ar2

error <- residuals(choco.ar2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) +
  scale_x_continuous(breaks= seq(1958, 1994, 2)) 

# Significatividad
coeftest(choco.ar2)

# Error de ajuste
accuracy(choco.ar2)

# Hipotesis residuo
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

jarque.bera.test(error) 

# Prediccion
tmp <- ts(rep(0, 48), start = 1995, frequency = 12)
pdl <- bizdays(tmp, FinCenter = "London")

pchoco.ar2 <- forecast(choco.ar2, 
                       h = 48,
                       xreg = cbind(pdl, 
                                    rep(0,48), rep(0,48), rep(0,48), rep(0,48)), 
                       level = 95)
autoplot(pchoco.ar2, 
         xlab = "",
         ylab = "Toneladas",
         main = "") +
  scale_x_continuous(breaks= seq(1958, 1998, 4)) 

# Error de predicción extra-muestral origen de prediccion movil
k <- 240                
h <- 12                 
T <- length(chocolate)  
s <- T - k - h            

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasLaborables, d0975, d0186, d0191, d0194))

for (i in 0:s) {
  train.set <- subset(chocolate, start = i + 1, end = i + k)
  test.set <-  subset(chocolate, start = i + k + 1, end = i + k + h) 
  
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
  
  fit <- try(Arima(train.set, 
                   order = c(1, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

