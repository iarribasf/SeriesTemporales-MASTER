#---------------------------------------------------------------
# Codigo Tema 5: junio 2020
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)
library(tseries)
library(aod)
library(seasonal)

#- Serie Nacimientos
nacimientos <- read.csv2("nacimientos.csv", header = TRUE)
nacimientos <- ts(nacimientos["nacimientos"],
                  start = c(1975, 1),
                  freq = 12)
nacimientos <- window(nacimientos, start = 2000)
autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos en España")

ggtsdisplay(diff(diff(log(nacimientos), lag = 12)), lag = 48,
            main = "FAC y FACP para Nacimientos")

monthdays(nacimientos)

#- Ajuste por modelos Arima de Nacimientos
Bisiestos <- 1*(monthdays(nacimientos) == 29)
d0408 <- 1*(cycle(nacimientos) == 4 & trunc(time(nacimientos)) == 2008)
d0111 <- 1*(cycle(nacimientos) == 1 & trunc(time(nacimientos)) == 2011)

auto.arima(nacimientos, 
           d = 1, 
           D = 1, 
           lambda = 0,
           xreg = cbind(Bisiestos, d0408, d0111))

summary(seas(nacimientos))

nac.ar1 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = list(order = c(0, 1, 2), period = 12),
                 lambda = 0,
                 xreg = cbind(Bisiestos, d0408, d0111))
nac.ar1

error <- residuals(nac.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2018, 2)) 

d0616 <- 1*(cycle(nacimientos) == 6 & trunc(time(nacimientos)) == 2016)
d1206 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2006)

nac.ar2 <- Arima(nacimientos, 
                 order = c(0,1,1),
                 seasonal = list(order = c(0, 1, 2), period = 12),
                 lambda = 0,
                 xreg = cbind(Bisiestos, d1206, d0408, d0111, d0616))
nac.ar2


error <- residuals(nac.ar2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2018, 2)) 

wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 1)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 2)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 3)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 4)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 5)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 6)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 7)
wald.test(b = coef(nac.ar2), Sigma = vcov(nac.ar2), Terms = 8)

accuracy(nac.ar2)

ggAcf(error, lag = 36, ylim = c(-0.3, 0.3), 
      main = "FAC del error del modelo")

Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")
jarque.bera.test(error) 

pbi <- 1*(monthdays(ts(0, start = 2019, end = c(2022, 12), freq = 12)) == 29)
pnac.ar2 <- forecast(nac.ar2, 
                     h = 48,
                     xreg = cbind(pbi, rep(0,48), rep(0,48), rep(0,48), rep(0,48)), 
                     level = 95)
pnac.ar2
forecast(nac.ar2, 
         h = 12,
         xreg = cbind(rep(0,12), rep(0,12), rep(0,12), rep(0,12), rep(0,12)), 
         level = 95)

autoplot(pnac.ar2, 
     ylab = 'Nacimientos',
     main = 'Nacimientos (2000-2018) y predicción (2019-2022)') +
  scale_x_continuous(breaks= seq(2000, 2022, 4)) 


#- Ajuste por modelos Arima de Nacimientos por dia
nac.ar3 <- Arima(nacimientos/monthdays(nacimientos), 
                 order = c(0,1,1),
                 seasonal = list(order = c(0, 1, 2), period = 12),
                 lambda = 0,
                 xreg = cbind(d1206, d0408, d0111, d0616))
nac.ar3

accuracy(nac.ar3)

#- Ajuste por modelos Arima de Chocolate
chocolate <- read.csv2("Chocolate.csv", header = TRUE)
chocolate <- ts(chocolate, start = 1958, freq = 12)

autoplot(chocolate, 
         xlab = "", 
         ylab = "Toneladas",
         main = "Producción de Chocolate (Australia)")

ggAcf(log(chocolate), lag = 48)
ggAcf(diff(log(chocolate)), lag = 48)
ggAcf(diff(log(chocolate), lag = 12), lag = 48)
ggAcf(diff(diff(log(chocolate), lag = 48, lag = 12)))

ggtsdisplay(diff(diff(log(chocolate), lag = 12)), lag = 48, main = "FAC y FACP para Chocolate (log)")

bizdays(chocolate, FinCenter = "London")
DiasLaborables <- bizdays(chocolate, FinCenter = "London")

auto.arima(chocolate, d = 1, D = 1, 
           lambda = 0,
           xreg = DiasLaborables)


summary(seas(diff(diff(log(chocolate), lag = 48))))

choco.ar1 <- Arima(chocolate, order=c(1, 1, 1),
                   seasonal = list(order = c(0, 1, 1), period = 12),
                   lambda = 0,
                   xreg = DiasLaborables)
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
                   seasonal = list(order = c(0, 1, 1), period = 12),
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

wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 1)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 2)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 3)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 4)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 5)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 6)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 7)
wald.test(b = coef(choco.ar2), Sigma = vcov(choco.ar2), Terms = 8)

accuracy(choco.ar2)

ggAcf(error, lag = 36)
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")
jarque.bera.test(error) 

dummy <- ts(rep(0, 48), start = 1995, frequency = 12)
pdl <- bizdays(dummy, FinCenter = "London")

pchoco.ar2 <- forecast(choco.ar2, 
                     h = 48,
                     xreg = cbind(pdl, rep(0,48), rep(0,48), rep(0,48), rep(0,48)), 
                     level = 95)
autoplot(pchoco.ar2, 
         xlab = "",
         ylab = 'Toneladas',
         main = 'Chocolate (1958-1994) y predicción (1995-1998)') +
  scale_x_continuous(breaks= seq(1958, 1998, 4)) 

#- Cross-validation para nacimientos 
Bisiestos <- monthdays(nacimientos) == 29
d0408 <- 1*(cycle(nacimientos) == 4 & trunc(time(nacimientos)) == 2008)
d0616 <- 1*(cycle(nacimientos) == 6 & trunc(time(nacimientos)) == 2016)
d1206 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2006)
d0111 <- 1*(cycle(nacimientos) == 1 & trunc(time(nacimientos)) == 2011)

k <- 120                
h <- 12                 
T <- length(nacimientos)
s<-T - k - h            

mapeArima <- matrix(NA, s, h)
mapeAlisado <- matrix(NA, s, h)

X <- cbind(Bisiestos, d1206, d0408, d0111, d0616)

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h) 
  
  X.train <- X[(i + 1):(i + k),]
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- X[(i + k + 1):(i + k + h),]
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- Arima(train.set, 
                 order = c(0, 1, 1),
                 seasonal = list(order = c(0, 1, 2), period = 12),
                 lambda = 0,
                 xreg=X.train)} else {
                   fit <- Arima(train.set, 
                                order = c(0, 1, 1),
                                seasonal = list(order = c(0, 1, 2), period = 12),
                                lambda = 0)
                 }
  
  if (length(X.train) > 0) fcast <- forecast(fit, h = h, xreg = X.test) else
    fcast <- forecast(fit, h = h)
  
  mapeArima[i,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, lambda = 0, model = "AAA", damped = TRUE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i,] <- 100*abs(test.set - fcast$mean)/test.set
}
  
errorArima <- colMeans(mapeArima)
errorArima

errorAlisado <- colMeans(mapeAlisado)
errorAlisado

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  geom_line(aes(x = 1:12, y = errorAlisado), colour = "Red") +
  ggtitle("Error de predicción (MAPE) según horizonte temporal") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

