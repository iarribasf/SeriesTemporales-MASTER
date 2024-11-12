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
DefEnfCer <- read.csv2("./series/Enfermedades cerebrovasculares.csv", 
                       header = TRUE)

DefEnfCer <- ts(DefEnfCer[,2], 
                start = 1980, 
                freq = 12)

DefEnfCer <- window(DefEnfCer, 
                    start = 1990)

autoplot(DefEnfCer,
         xlab = "",
         ylab = "",
         main = "")

#- Identificacion
ggtsdisplay(diff(diff(log(DefEnfCer), lag = 12)), lag = 48)

d0299 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 1999)
d0603 <- 1*(cycle(DefEnfCer) == 6 & trunc(time(DefEnfCer)) == 2003)
d0105 <- 1*(cycle(DefEnfCer) == 1 & trunc(time(DefEnfCer)) == 2005)
d0212 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2012)

auto.arima(DefEnfCer, 
           d = 1, 
           D = 1,
           lambda = 0,
           xreg = cbind(d0299, d0603, d0105, d0212))

summary(seas(DefEnfCer))

#- Estimacion + intervencion: ronda 1
DefEnfCerArima1 <- Arima(DefEnfCer, 
                         order = c(0, 1, 1),  
                         seasonal = c(0, 1, 1),
                         lambda = 0,
                         cbind(d0299, d0603, d0105, d0212))

DefEnfCerArima1

error <- residuals(DefEnfCerArima1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1990, 2024, 2))

fechas <- format(seq(as.Date("1990-01-01"), as.Date("2023-12-01"), "month"), "%Y-%m")
fechas[abs(error) > 3 * sderror]

atipicos <- tsoutliers(error)
fechas[atipicos$index]

#- Estimacion + intervencion: ronda 2
d0501 <- 1*(cycle(DefEnfCer) == 5 & trunc(time(DefEnfCer)) == 2001)
d0803 <- 1*(cycle(DefEnfCer) == 8 & trunc(time(DefEnfCer)) == 2003)
d0205 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2005)
d0115 <- 1*(cycle(DefEnfCer) == 1 & trunc(time(DefEnfCer)) == 2015)
d0215 <- 1*(cycle(DefEnfCer) == 2 & trunc(time(DefEnfCer)) == 2015)
d0722 <- 1*(cycle(DefEnfCer) == 7 & trunc(time(DefEnfCer)) == 2022)


DefEnfCerArima2 <- Arima(DefEnfCer, 
                         order = c(0, 1, 1),  
                         seasonal = c(0, 1, 1),
                         lambda = 0,
                         xreg = cbind(d0299, d0501, d0603, 
                                      d0803, d0105, d0205, 
                                      d0212, d0115, d0215, d0722))

DefEnfCerArima2

error <- residuals(DefEnfCerArima2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1990, 2024, 2)) 

fechas[abs(error) > 3 * sderror]

#- Agrupamos meses atípicos
d01aa <- d0105 + d0115
d02aa <- d0299 + d0205 + d0212 + d0215

DefEnfCerArima3 <- Arima(DefEnfCer, 
                         order = c(0, 1, 1),  
                         seasonal = c(0, 1, 1),
                         lambda = 0,
                         xreg = cbind(d01aa, d02aa, 
                                      d0501, d0603, d0803, d0722))
DefEnfCerArima3

#- Validacion: coeficientes significativos
coeftest(DefEnfCerArima3)

#- Validacion: calidad ajuste
accuracy(DefEnfCerArima3)

#- Validacion: hipotesis residuo
error <- residuals(DefEnfCerArima3)

Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

jarque.bera.test(error) 

#- Prediccion
pDefEnfCerArima3 <- forecast(DefEnfCerArima3, 
                             h = 60,
                             xreg = cbind(rep(0, 60), rep(0, 60), rep(0 ,60), 
                                          rep(0 ,60), rep(0, 60), rep(0, 60)), 
                             level = 95)

autoplot(pDefEnfCerArima3, 
         xlab = "",
         ylab = "Defunciones",
         main = "") +
  scale_x_continuous(breaks= seq(1990, 2028, 4)) 

#- Comparacion con alisado
summary(ets(DefEnfCer, lambda = 0))

#- Validacion cruzada
k <- 120                   
h <- 12                    
T <- length(DefEnfCer)     
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)
mapeAlisado <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(DefEnfCer, start = i + 1, end = i + k)
  test.set <-  subset(DefEnfCer, start = i + k + 1, end = i + k + h) 
  
  fit <- Arima(train.set, 
               order = c(0, 1, 1),
               seasonal = c(0, 1, 1), 
               lambda = 0)
  
  fcast <- forecast(fit, h = h)
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, lambda = 0, model = "AAA", damped = FALSE)
  
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorArima <- apply(mapeArima, MARGIN = 2, FUN = median)
errorArima

errorAlisado <- apply(mapeAlisado, MARGIN = 2, FUN = median)
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





