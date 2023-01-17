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
library(aod)
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
ggAcf(log(nacimientos), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(nacimientos)), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(nacimientos), lag = 12), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(diff(log(nacimientos), lag = 12)), lag = 48, xlab = "", ylab = "", main = "")

# Identificación
ggtsdisplay(diff(diff(log(nacimientos), lag = 12)), lag = 48)

monthdays(nacimientos)
easter(nacimientos)

DiasMes <- monthdays(nacimientos)
SemanaSanta <- easter(nacimientos)
d0111 <- 1*(cycle(nacimientos) == 1 & trunc(time(nacimientos)) == 2011)

auto.arima(nacimientos, 
           d = 1, 
           D = 1, 
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta, d0111))

summary(seas(nacimientos))

# Estimación e intervencion
d1210 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2010)

nac.ar1 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 2),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, d1210,  d0111))
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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

d1206 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2006)
d0416 <- 1*(cycle(nacimientos) == 4 & trunc(time(nacimientos)) == 2016)
d0616 <- 1*(cycle(nacimientos) == 6 & trunc(time(nacimientos)) == 2016)

nac.ar2 <- Arima(nacimientos, 
                 order = c(0,1,1),
                 seasonal = c(0, 1, 2),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d1210, d0111, d0416, d0616))
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
  scale_x_continuous(breaks= seq(2000, 2020, 2)) 

# Compensacion
d12100111 <- d1210 - d0111

nac.ar3 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 2),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d12100111, d0416, d0616))
nac.ar3

# Significatividad
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 1)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 2)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 3)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 4)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 5)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 6)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 7)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 8)
wald.test(b = coef(nac.ar3), Sigma = vcov(nac.ar3), Terms = 9)

# Estimación
nac.ar4 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal =  c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d12100111, d0416, d0616))
nac.ar4

# Error de ajuste
accuracy(nac.ar4)

# Hipotesis sobre el residuo
error <- residuals(nac.ar4)

Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

jarque.bera.test(error) 

ggAcf(error, lag = 36, ylim = c(-0.3, 0.3), main = "")

# Predicción
tmp <- ts(rep(0, 48), start = 2020, freq = 12)
pdm <- monthdays(tmp)
pss <- easter(tmp)
pnac.ar4 <- forecast(nac.ar4, 
                     h = 48,
                     xreg = cbind(pdm, pss, rep(0,48), rep(0,48), 
                                  rep(0,48), rep(0,48)), 
                     level = 95)
pnac.ar4

autoplot(pnac.ar4, 
         ylab = "Nacimientos",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2024, 4)) 

# Error de predicción extra-muestral origen de prediccion movil
k <- 120                   
h <- 12                    
T <- length(nacimientos)   
s<-T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasMes, SemanaSanta, d1206, d1210, d0111, d0416, d0616))

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     order = c(0, 1, 1),
                     seasonal = c(0, 1, 1),
                     lambda = 0,
                     xreg=as.matrix(X.train)))
  } else {
    fit <- try(Arima(train.set, 
                     order = c(0, 1, 1),
                     seasonal = c(0, 1, 1),
                     lambda = 0))
  }
  
  if(!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) 
      fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
        fcast <- forecast(fit, h = h)
      
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
ggAcf(log(exportaciones), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(exportaciones)), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(log(exportaciones), lag = 12), lag = 48, xlab = "", ylab = "", main = "")
ggAcf(diff(diff(log(exportaciones), lag = 12)), lag = 48, xlab = "", ylab = "", main = "")

ndiffs(log(exportaciones))
nsdiffs(log(exportaciones))

# Identificación
auto.arima(exportaciones,
           d = 1,
           D = 1,
           lambda = 0,
           xreg = cbind(monthdays(exportaciones), easter(exportaciones)))

summary(seas(exportaciones))

# Estimación + intervencion
DiasLaborables <- bizdays(exportaciones, FinCenter = "London")
SemanaSanta <- easter(exportaciones)

l1208 <- 1*(trunc(time(exportaciones)) > 2008) + 
  1*(cycle(exportaciones) == 12 & trunc(time(exportaciones)) == 2008)

l0320 <- 1*(trunc(time(exportaciones)) > 2020) + 
  1*(cycle(exportaciones)   > 2 & trunc(time(exportaciones)) == 2020)

d1207 <- 1*(cycle(exportaciones) == 12 & trunc(time(exportaciones)) == 2007)
d0420 <- 1*(cycle(exportaciones) ==  4 & trunc(time(exportaciones)) == 2020)
d0520 <- 1*(cycle(exportaciones) ==  5 & trunc(time(exportaciones)) == 2020)

exp.ar1 <- Arima(exportaciones, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasLaborables, SemanaSanta, 
                              l1208, l0320, d1207, d0420, d0520))
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
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 1)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 2)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 3)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 4)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 5)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 6)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 7)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 8)
wald.test(b = coef(exp.ar1), Sigma = vcov(exp.ar1), Terms = 9)

# Error de ajuste
accuracy(nac.ar2)

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
s<-T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasLaborables, SemanaSanta, 
                      l1208, l0320, d1207, d0420, d0520))

for (i in 0:s) {
  train.set <- subset(exportaciones, start = i + 1, end = i + k)
  test.set <-  subset(exportaciones, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     order = c(0, 1, 1),
                     seasonal = c(0, 1, 1),
                     lambda = 0,
                     xreg=as.matrix(X.train),
                     optim.method = "BFGS"))
  } else {
    fit <- try(Arima(train.set, 
                     order = c(0, 1, 1),
                     seasonal = c(0, 1, 1),
                     lambda = 0))
  }
  
  if(!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) 
      fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
        fcast <- forecast(fit, h = h)
      
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
tmp <- ts(rep(0, 48), start = 2022, freq = 12)
pdl <- bizdays(tmp, FinCenter = "London")
pss <- easter(tmp)
pexp.ar1 <- forecast(exp.ar1, 
                     h = 48,
                     xreg = cbind(pdl, pss, 
                                  rep(1,48), rep(1,48), 
                                  rep(0,48), rep(0,48), rep(0,48)), 
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

ggtsdisplay(diff(diff(log(chocolate), lag = 12)), lag = 48)

# Identificación
DiasLaborables <- bizdays(chocolate, FinCenter = "London")
DiasLaborables

auto.arima(chocolate, d = 1, D = 1, 
           lambda = 0,
           xreg = cbind(DiasLaborables))

summary(seas(diff(diff(log(chocolate), lag = 12))))

# Estimacion + Intervencion
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
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 1)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 2)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 3)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 4)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 5)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 6)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 7)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 8)

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
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     order = c(1, 1, 1),
                     seasonal = c(0, 1, 1),
                     lambda = 0,
                     xreg=as.matrix(X.train)))
  } else {
    fit <- try(Arima(train.set, 
                     order = c(1, 1, 1),
                     seasonal = c(0, 1, 1),
                     lambda = 0))
  }
  
  if(!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) 
      fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
        fcast <- forecast(fit, h = h)
      
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

