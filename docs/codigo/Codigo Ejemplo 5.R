#---------------------------------------------------------------
# Codigo ejemplo tema 5
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)
library(tseries)
library(lmtest)
library(seasonal)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("series/Enfermedades cerebrovasculares.csv", 
                       header = TRUE)

DefEnfCer <- ts(DefEnfCer[,2], 
                start = 1980, 
                freq = 12)

DefEnfCer <- window(DefEnfCer, 
                    start = 1990)

#- Identificacion
ggtsdisplay(diff(diff(log(DefEnfCer), lag = 12)), 
            lag = 48,
            main = "FAC y FACP para Defunciones. (log)")

d0299 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 1999)
d0603 <- 1*(cycle(DefEnfCer) == 6 & trunc(time(DefEnfCer)) == 2003)
d0803 <- 1*(cycle(DefEnfCer) == 8 & trunc(time(DefEnfCer)) == 2003)
d0212 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2012)

auto.arima(DefEnfCer, 
           d = 1, 
           D = 1,
           lambda = 0,
           xreg = cbind(d0299, d0603, d0803, d0212))

summary(seas(DefEnfCer,
             arima.model = c(1, 1, 1, 1, 1, 1)))

#- Estimacion
DefEnfCerArima1 <- Arima(DefEnfCer, 
                         order = c(1, 1, 1),  
                         seasonal = c(1, 1, 1),
                         lambda = 0,
                         cbind(d0299, d0603, d0803, d0212))
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
  scale_x_continuous(breaks= seq(1990, 2022, 2)) 

d1291 <- 1*(cycle(DefEnfCer) ==12 & trunc(time(DefEnfCer)) == 1991)
d0793 <- 1*(cycle(DefEnfCer) == 7 & trunc(time(DefEnfCer)) == 1993)
d0501 <- 1*(cycle(DefEnfCer) == 5 & trunc(time(DefEnfCer)) == 2001)
d0105 <- 1*(cycle(DefEnfCer) == 1 & trunc(time(DefEnfCer)) == 2005)
d0115 <- 1*(cycle(DefEnfCer) == 1 & trunc(time(DefEnfCer)) == 2015)
d0215 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2015)
d0221 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2021)


#- Segunda estimacion
DefEnfCerArima2 <- Arima(DefEnfCer, 
                         order = c(1, 1, 1),  
                         seasonal = c(1, 1, 1),
                         lambda = 0,
                         xreg = cbind(d1291, d0793, d0299, d0501, d0603, 
                                      d0803, d0105, d0212, d0115, d0215,
                                      d0221))
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
  scale_x_continuous(breaks= seq(1990, 2022, 2)) 

#- Agrupamos meses atípicos
d01aa <- d0105 + d0115
d02aa <- d0299 + d0212 + d0215

DefEnfCerArima3 <- Arima(DefEnfCer, 
                         order = c(1, 1, 1),  
                         seasonal = c(1, 1, 1),
                         lambda = 0,
                         xreg = cbind(d01aa, d02aa, 
                                      d0793, d0501, d0603, d0803,
                                      d0221))
DefEnfCerArima3

#- Coeficientes significativos
coeftest(DefEnfCerArima3)

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
                                          rep(0 ,60), rep(0, 60), rep(0, 60), 
                                          rep(0, 60)), 
                             level = 95)

autoplot(pDefEnfCerArima3, 
         xlab = "",
         ylab = 'Defunciones',
         main = 'Defunciones (1990-2021) y predicción (2022-2026)') +
  scale_x_continuous(breaks= seq(1990, 2026, 4)) 


#- Comparacion con alisado
summary(ets(DefEnfCer, lambda = 0))

#- Validacion cruzada
k <- 120                   
h <- 12                    
T <- length(DefEnfCer)     
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)
mapeAlisado <- matrix(NA, s + 1, h)

X <- data.frame(cbind(d01aa, d02aa, d0793, d0501, d0603, d0803, d0221))

for (i in 0:s) {
  train.set <- subset(DefEnfCer, start = i + 1, end = i + k)
  test.set <-  subset(DefEnfCer, start = i + k + 1, end = i + k + h) 
  
  X.train <- data.frame(X[(i + 1):(i + k),])
  X.test <- data.frame(X[(i + k + 1):(i + k + h),])
  
  hay <- colSums(X.train)
  
  if(sum(hay) == 0){
    X.train <- NULL
    X.test <- NULL
  } else {
    X.train <- as.matrix(X.train[, hay>0])
    X.test <- as.matrix(X.test[, hay>0])
  }
  
  fit <- try(Arima(train.set, 
                   order = c(1, 1, 1),
                   seasonal = c(1, 1, 1), 
                   lambda = 0,
                   xreg = X.train), 
             silent = TRUE)
  
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, 
                      h = h, 
                      xreg = X.test)
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

datos <- data.frame(
  factor = c(rep("Arima", 12), rep("Alisado", 12)),
  x = c(1:12,1:12),
  y = c(errorArima, errorAlisado)
)

ggplot(datos, aes(x = x, y = y,  colour= factor)) + 
  geom_line() +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12) +
  labs(colour = "Métodos") + 
  theme(legend.position=c(0.1,0.8)) 





