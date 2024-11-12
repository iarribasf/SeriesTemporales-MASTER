#----------------------------------------------------------
# CODIGO EJEMPLO 3: ALISADO EXPONENCIAL
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos Pernoctaciones
#----------------------------------------------------------
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[, 2] / 1000000, 
                     start = 2000, 
                     frequency = 12)

autoplot(Pernoctaciones,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial para la serie anual
#----------------------------------------------------------
# Serie anual
PernoctacionesAnual <- aggregate(Pernoctaciones, FUN = sum)

autoplot(PernoctacionesAnual,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 

# Ajuste
PernoctacionesAnualEts <- ets(PernoctacionesAnual)

round(accuracy(PernoctacionesAnualEts, test = 1:20), 2)

# Prediccion
forecast(PernoctacionesAnualEts, 
         h = 4, 
         level = 95)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial para la serie mensual
#----------------------------------------------------------
# Ajuste
PernoctacionesEts <- ets(Pernoctaciones)
PernoctacionesEts

autoplot(PernoctacionesEts,
         xlab = "",
         main = "")

# Ultimos valores
TT <- nrow(PernoctacionesEts$states)
PernoctacionesEts$states[TT,]

# Estacionalidad
componenteEstacional <- PernoctacionesEts$states[TT, 14:3]

ggplot() +
  geom_line(aes(x = 1:12, y = componenteEstacional)) + 
  geom_hline(yintercept = 0, colour = "blue", lty = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 

# Predicción
PernoctacionesEtsPre <- forecast(PernoctacionesEts, 
                                 h = 48, 
                                 level = 95)

PernoctacionesEtsPre

autoplot(PernoctacionesEtsPre,
         xlab = "",
         ylab = "Casos",
         main = "",
         PI = FALSE)  +
  scale_x_continuous(breaks= seq(2000, 2028, 2)) 

# Análisis del error
error <- residuals(PernoctacionesEts)
sderror <- sd(error)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "",
         colour = "black") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 

fechas <- format(seq(as.Date("2000-01-01"), as.Date("2023-12-01"), by = 'month'), "%Y-%m")
fechas[abs(error) > 3 * sderror]

# Analisis del error precovid
error <- window(error, end = c(2019, 12))
sderror <- sd(error)

fechas <- format(seq(as.Date("2000-01-01"), as.Date("2019-12-01"), by = 'month'), "%Y-%m")

fechas[abs(error) > 3 * sderror]

# Prueba de Tukey 
atipicos <- tsoutliers(error)
fechas[atipicos$index]

# Error por origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(Pernoctaciones)
s <- TT - k - h          

mapeAlisado <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "AAA", damped = TRUE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorAlisado <- apply(mapeAlisado, MARGIN = 2, FUN = median)
errorAlisado

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado)) +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  ylim(0, 14) + 
  scale_x_continuous(breaks= 1:12)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Modelos alternativos
#----------------------------------------------------------
k <- 120                 
h <- 12                  
TT <- length(Pernoctaciones)
s <- TT - k - h

mapeAlisado1 <- mapeAlisado2 <- mapeAlisado3 <- mapeAlisado4 <- 
  mapeAlisado5 <- mapeAlisado6 <-matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "AAA", damped = TRUE)
  fcast<-forecast(fit, h = h)
  mapeAlisado1[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAA", damped = TRUE, opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h)
  mapeAlisado2[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAA", damped = TRUE, lambda = 0)
  fcast<-forecast(fit, h = h, biasadj = TRUE)
  mapeAlisado3[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAA", damped = TRUE, lambda = 0, opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h, biasadj = TRUE)
  mapeAlisado4[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set/monthdays(train.set), model = "AAA", damped = TRUE)
  fcast<-forecast(fit, h = h)
  mapeAlisado5[i + 1,] <- 100*abs(test.set - fcast$mean * monthdays(fcast$mean))/test.set
  
  fit <- ets(train.set/monthdays(train.set), model = "AAA", damped = TRUE, opt.crit = "amse", nmse = 2)
  fcast<-forecast(fit, h = h)
  mapeAlisado6[i + 1,] <- 100*abs(test.set - fcast$mean * monthdays(fcast$mean))/test.set
}


errorAlisado1 <- apply(mapeAlisado1, MARGIN = 2, FUN = median)
errorAlisado2 <- apply(mapeAlisado2, MARGIN = 2, FUN = median)
errorAlisado3 <- apply(mapeAlisado3, MARGIN = 2, FUN = median)
errorAlisado4 <- apply(mapeAlisado4, MARGIN = 2, FUN = median)
errorAlisado5 <- apply(mapeAlisado5, MARGIN = 2, FUN = median)
errorAlisado6 <- apply(mapeAlisado6, MARGIN = 2, FUN = median)

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado1, colour = "Modelo 1")) +
  geom_line(aes(x = 1:12, y = errorAlisado2, colour = "Modelo 2")) + 
  geom_line(aes(x = 1:12, y = errorAlisado3, colour = "Modelo 3")) +
  geom_line(aes(x = 1:12, y = errorAlisado4, colour = "Modelo 4")) +
  geom_line(aes(x = 1:12, y = errorAlisado5, colour = "Modelo 5")) +
  geom_line(aes(x = 1:12, y = errorAlisado6, colour = "Modelo 6")) +
  ggtitle("") +
  xlab("") +
  ylab("MedAPE") +
  scale_x_continuous(breaks= 1:12) +
  scale_color_discrete(name = "Modelos")

