#---------------------------------------------------------------
# Codigo ejemplo tema 4
#---------------------------------------------------------------
#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)
library(tseries)
library(lmtest)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("series/Enfermedades cerebrovasculares.csv",
                       header = TRUE)

DefEnfCer <- ts(DefEnfCer[,2], 
                start = 1980, 
                freq = 12)

DefEnfCer <- window(DefEnfCer, 
                    start = 1990)

DefEnfCer <- aggregate(DefEnfCer, 
                       FUN = sum)

#- Resumen de tema 3
autoplot(DefEnfCer, xlab = "Serie original", ylab = "", main = "")
autoplot(diff(DefEnfCer), xlab = "Serie diferenciada", ylab = "", main = "")

ggAcf(DefEnfCer)
ggAcf(diff(DefEnfCer))

ggtsdisplay(diff(DefEnfCer), main = "Def. por enf. cerebrovasculares (dif.)")

#- Ajuste por modelos arima
auto.arima(DefEnfCer, 
           d = 1)

arima110 <- Arima(DefEnfCer, 
                  order = c(1, 1, 0),
                  include.constant = TRUE)

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
  scale_x_continuous(breaks= seq(1990, 2020, 4)) 

#- Significativdad coeficentes
coeftest(arima110)

#- Validacion
accuracy(arima110)
Box.test(error, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box") 
shapiro.test(error)

#- Prediccion
parima110 <- forecast(arima110, 
                      h = 5, 
                      level = 95)

parima110

autoplot(parima110, 
         ylab = "Defunciones",
         main = "Defunciones por enfermedades cerebrovasculares") +
  scale_x_continuous(breaks= seq(1990, 2026, 4)) 

#Validacion cruzada
k <- 20                   
h <- 5                    
TT <- length(DefEnfCer)   
s <- TT-k-h               

mapeArima <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(DefEnfCer, start = i + 1, end = i + k)
  test.set <-  subset(DefEnfCer, start = i + k + 1, end = i + k + h)
  
  fit <- Arima(train.set, 
               order = c(1, 1, 0),
               include.constant = TRUE)
  
  fcast<-forecast(fit, h = h)
  
  mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorArima <- colMeans(mapeArima)
errorArima

#- Comparacion con alisado
summary(alisado <- ets(DefEnfCer))

palisado <- forecast(alisado, h = 5)

autoplot(DefEnfCer,
         series = "Defunciones",
         main="Defunciones Enf. Cerebrovasculares y previsión",
         xlab="", 
         ylab="Defunciones") +
  autolayer(parima110$mean, series = "Previsión con Arima") +
  autolayer(palisado$mean, series = "Previsión con Alisado") +
  labs(colour = "Series") + 
  theme(legend.position=c(0.8,0.8))
