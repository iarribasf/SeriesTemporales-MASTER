#---------------------------------------------------------------
# Codigo ejemplo tema 2: junio 2020
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("Enfermedades cerebrovasculares.csv", header = TRUE)
DefEnfCer <- ts(DefEnfCer[,2], start = 1980, freq = 12)
DefEnfCer <- window(DefEnfCer, start = 1988)

autoplot(DefEnfCer,
         xlab = "",
         ylab = "Casos",
         main = "Defunciones causadas por enfermedades cerebrovasculares") +
  scale_x_continuous(breaks= seq(1980, 2018, 2)) 

#- Ajuste por alisado
DefEnfCerEts <- ets(DefEnfCer, model = "ZZZ")
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

DefEnfCerEtsPre <- forecast(DefEnfCerEts, h = 36, level = 95)
DefEnfCerEtsPre
autoplot(DefEnfCerEtsPre,
         xlab = "",
         ylab = "Casos",
         main = "Muertes por enf. cerebrovasculares (1998-2018) y predicción (2019-2021)",
         PI = FALSE)


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


DefEnfCerIntra <- subset(DefEnfCer, end = length(DefEnfCer) -36)
DefEnfCerExtra <- subset(DefEnfCer, start = length(DefEnfCer) -35)
DefEnfCerIntraEts <- ets(DefEnfCerIntra, damped = FALSE, model = "MAM")
DefEnfCerIntraEtsPre <- forecast(DefEnfCerIntraEts, h = 36)
accuracy(DefEnfCerIntraEtsPre, DefEnfCerExtra)

#- Ajuste por alisado del logaritmo
lDefEnfCerEts<-ets(DefEnfCer, lambda = 0)
summary(lDefEnfCerEts) 

lDefEnfCerEtsPre<-forecast(lDefEnfCerEts, h = 12)
lDefEnfCerEtsPre
lDefEnfCerIntraEts <- ets(DefEnfCerIntra, 
                          lambda = 0, 
                          damped = FALSE, 
                          model = "AAA")

lDefEnfCerIntraEtsPre <- forecast(lDefEnfCerIntraEts, h = 36)
accuracy(lDefEnfCerIntraEtsPre, DefEnfCerExtra)

#- Ajuste por alisado usando otros criterios de ajuste
mDefEnfCerEts<-ets(DefEnfCer, opt.crit = "mse")
summary(mDefEnfCerEts) 

mDefEnfCerEtsPre<-forecast(mDefEnfCerEts, h = 12)
mDefEnfCerEtsPre

mDefEnfCerIntraEts <- ets(DefEnfCerIntra, 
                          damped = FALSE, 
                          model = "MAM")

mDefEnfCerIntraEtsPre <- forecast(mDefEnfCerIntraEts, h = 36)
accuracy(mDefEnfCerIntraEtsPre, DefEnfCerExtra)

#- Validacion cruzada 
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



