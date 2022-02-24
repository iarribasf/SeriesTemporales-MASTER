#---------------------------------------------------------------
# Codigo ejemplo tema 2
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("series/Enfermedades cerebrovasculares.csv", header = TRUE)
DefEnfCer <- ts(DefEnfCer[,2], start = 1980, freq = 12)
DefEnfCer <- window(DefEnfCer, start = 1988)

autoplot(DefEnfCer,
         xlab = "",
         ylab = "Casos",
         main = "Defunciones causadas por enfermedades cerebrovasculares") +
  scale_x_continuous(breaks= seq(1980, 2018, 2)) 

#- Ajuste por alisado
DefEnfCerEts <- ets(DefEnfCer)
summary(DefEnfCerEts) 

autoplot(DefEnfCerEts,
         xlab = "",
         main = "Descomposición para defunciones por enfermedades cerebrovasculares")

TT <- nrow(DefEnfCerEts$states)
DefEnfCerEts$states[TT,]

componenteEstacional <- DefEnfCerEts$states[TT, 14:3]
ggplot() +
  geom_line(aes(x = 1:12, y = componenteEstacional)) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("Componente estacional") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 

#- Prediccion
DefEnfCerEtsPre <- forecast(DefEnfCerEts, h = 36, level = 95)
DefEnfCerEtsPre
autoplot(DefEnfCerEtsPre,
         xlab = "",
         ylab = "Casos",
         main = "Muertes por enf. cerebrovasculares (1998-2018) y predicción (2019-2021)",
         PI = FALSE)

#- Error
error <- residuals(DefEnfCerEts, type = "response")
sderror <- sd(error)
autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "Error + Intervención",
         colour = "black") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1988, 2018, 2)) 

#- Origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(DefEnfCer)  
s <- TT - k - h          

mapeAlisado <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(DefEnfCer, start = i + 1, end = i + k)
  test.set <-  subset(DefEnfCer, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MAM", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorAlisado <- colMeans(mapeAlisado)
errorAlisado

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado)) +
  ggtitle("Error de predicción según horizonte temporal") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12)


#- Ajuste por alisado usando otros criterios de ajuste
k <- 120                 
h <- 12                  
TT <- length(DefEnfCer)  
s <- TT - k - h         

mapeAlisado1 <- matrix(NA, s + 1, h)
mapeAlisado2 <- matrix(NA, s + 1, h)
mapeAlisado3 <- matrix(NA, s + 1, h)
mapeAlisado4 <- matrix(NA, s + 1, h)
mapeAlisado5 <- matrix(NA, s + 1, h)
mapeAlisado6 <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(DefEnfCer, start = i + 1, end = i + k)
  test.set <-  subset(DefEnfCer, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MAM", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado1[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "MAM", damped = FALSE, opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h)
  mapeAlisado2[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAA", lambda = 0, biasadj = TRUE, damped = FALSE)
  fcast<-forecast(fit, h = h, biasadj = TRUE)
  mapeAlisado3[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAA", lambda = 0, biasadj = TRUE, damped = FALSE,  opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h, biasadj = TRUE)
  mapeAlisado4[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set/monthdays(train.set), model = "MAM", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado5[i + 1,] <- 100*abs(test.set - fcast$mean * monthdays(fcast$mean))/test.set
  
  fit <- ets(train.set/monthdays(train.set), model = "MAM", damped = FALSE, opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h)
  mapeAlisado6[i + 1,] <- 100*abs(test.set - fcast$mean * monthdays(fcast$mean))/test.set
}

errorAlisado1 <- colMeans(mapeAlisado1)
errorAlisado2 <- colMeans(mapeAlisado2)
errorAlisado3 <- colMeans(mapeAlisado3)
errorAlisado4 <- colMeans(mapeAlisado4)
errorAlisado5 <- colMeans(mapeAlisado5)
errorAlisado6 <- colMeans(mapeAlisado6)

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado1, colour = "Modelo 1")) +
  geom_line(aes(x = 1:12, y = errorAlisado2, colour = "Modelo 2")) + 
  geom_line(aes(x = 1:12, y = errorAlisado3, colour = "Modelo 3")) +
  geom_line(aes(x = 1:12, y = errorAlisado4, colour = "Modelo 4")) +
  geom_line(aes(x = 1:12, y = errorAlisado5, colour = "Modelo 5")) +
  geom_line(aes(x = 1:12, y = errorAlisado6, colour = "Modelo 6")) +
  ggtitle("Errores de previsión extra-muestral. Varios modelos") +
  xlab("") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12) +
  scale_color_discrete(name = "Modelos")
