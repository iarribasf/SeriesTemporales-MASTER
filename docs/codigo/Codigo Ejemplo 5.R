#---------------------------------------------------------------
# Codigo ejemplo tema 5
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)
library(tseries)
library(aod)
library(seasonal)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("series/Enfermedades cerebrovasculares.csv", header = TRUE)
DefEnfCer <- ts(DefEnfCer[,2], start = 1980, freq = 12)
DefEnfCer <- window(DefEnfCer, start = 1988)

#- Identificacion
ggtsdisplay(diff(diff(log(DefEnfCer), lag = 12)), lag = 48,
            main = "FAC y FACP para Defunciones. (log)")

DiasMes <- monthdays(DefEnfCer)

d0299 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 1999)
d0105 <- 1*(cycle(DefEnfCer) == 1 & trunc(time(DefEnfCer)) == 2005)
d0212 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2012)

auto.arima(DefEnfCer, d = 1, D = 1,
           lambda = 0,
           xreg = cbind(DiasMes, d0299, d0105, d0212))

summary(seas(DefEnfCer))

#- Estimacion
DefEnfCerArima1 <- Arima(DefEnfCer, 
                         order = c(1, 1, 1),  
                         seasonal = c(1, 1, 1),
                         lambda = 0,
                         xreg = cbind(d0299, d0105, d0212))
DefEnfCerArima1

#- Valores extremos
error <- residuals(DefEnfCerArima1)
sderror <- sd(error)
autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1988, 2018, 2)) 

d0501 <- 1*(cycle(DefEnfCer) == 5 & trunc(time(DefEnfCer)) == 2001)
d0603 <- 1*(cycle(DefEnfCer) == 6 & trunc(time(DefEnfCer)) == 2003)
d0115 <- 1*(cycle(DefEnfCer) == 1 & trunc(time(DefEnfCer)) == 2015)
d0215 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2015)
d0389 <- 1*(cycle(DefEnfCer) == 3 & trunc(time(DefEnfCer)) == 1989)
d0803 <- 1*(cycle(DefEnfCer) == 8 & trunc(time(DefEnfCer)) == 2003)

#- Segunda estimacion
DefEnfCerArima2 <- Arima(DefEnfCer, 
                         order = c(1, 1, 1),  
                         seasonal = c(1, 1, 1),
                         lambda = 0,
                         xreg = cbind(d0299, d0105, d0215, d0212, d0501, 
                                      d0603, d0115, d0389, d0803))
DefEnfCerArima2

#- Valores extremos
error <- residuals(DefEnfCerArima2)
sderror <- sd(error)
autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1988, 2018, 2)) 

#- Agrupamos meses atípicos
d01aa <- d0105 + d0115
d02aa <- d0299 + d0215 + d0212

DefEnfCerArima3 <- Arima(DefEnfCer, 
                         order = c(1, 1, 1),  
                         seasonal = c(1, 1, 1),
                         lambda = 0,
                         xreg = cbind(d01aa, d02aa, d0501, 
                                      d0603, d0389, d0803))
DefEnfCerArima3

#- Coeficientes significativos
for(i in 1:length(coef(DefEnfCerArima3))) {
  wt <- wald.test(b = coef(DefEnfCerArima3), 
                  Sigma = vcov(DefEnfCerArima3), 
                  Terms = i)
  cat("\nCoeficiente: ", names(coef(DefEnfCerArima3))[i], "\tvalor de p: ", 
      formatC(wt$result$chi2[3], digits = 4, format = "f"))
}

accuracy(DefEnfCerArima3)
ggAcf(error, lag = 36)
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")
jarque.bera.test(error) 

#- Prediccion
pDefEnfCerArima3 <- forecast(DefEnfCerArima3, 
                             h = 60,
                             xreg = cbind(rep(0, 60), rep(0, 60), rep(0 ,60), 
                                          rep(0, 60), rep(0, 60), rep(0, 60)), 
                             level = 95)
autoplot(pDefEnfCerArima3, 
         xlab = "",
         ylab = 'Defunciones',
         main = 'Defunciones (1988-2018) y predicción (2019-2024)') +
  scale_x_continuous(breaks= seq(1988, 2024, 4)) 


#- Comparacion con alisado
summary(ets(DefEnfCer, lambda = 0))

#- Validacion cruzada
k <- 120                   
h <- 12                    
T <- length(DefEnfCer)     
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)
mapeAlisado <- matrix(NA, s + 1, h)

X <- data.frame(cbind(d01aa, d02aa, d0501, d0603, d0389, d0803))

for (i in 0:s) {
  train.set <- subset(DefEnfCer, start = i + 1, end = i + k)
  test.set <-  subset(DefEnfCer, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1), period = 12),
                     lambda = 0,
                     xreg = as.matrix(X.train)), silent = TRUE)
  } else {
    fit <- try(Arima(train.set, 
                     order = c(1, 1, 1),
                     seasonal = list(order = c(1, 1, 1), period = 12),
                     lambda = 0), silent = TRUE)
  }
  
  if (!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) fcast <- forecast(fit, h = h, xreg = as.matrix(X.test)) else
      fcast <- forecast(fit, h = h)
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
  
  fit <- ets(train.set, lambda = 0, model = "AAA", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
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





