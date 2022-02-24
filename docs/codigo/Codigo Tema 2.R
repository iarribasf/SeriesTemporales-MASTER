#---------------------------------------------------------------
# Codigo Tema 2
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)

#- Serie libros
libros <- read.csv2("series/libros.csv", header = TRUE)
libros <- ts(libros[, 2], start = 1993, frequency  = 1)

#- Serie nacimientos
nacimientos <- read.csv2("series/nacimientos.csv", header = TRUE)
nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

#- Metodos sencillos para libros
(mediaLibros <- meanf(libros, h = 5))
(naiveLibros <- naive(libros, h = 5))
(derivaLibros <- rwf(libros,  h = 5, drift = TRUE))
 
autoplot(libros, series = "Libros",
                xlab = "",
                ylab = "Títulos",
                main = "Libros y predicción por métodos sencillos") +
  autolayer(mediaLibros, series="Media", PI = FALSE) +
  autolayer(naiveLibros, series="Ingenuo", PI = FALSE) +
  autolayer(derivaLibros, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Libros", "Media", "Ingenuo", "Deriva")) +
  guides(colour = guide_legend(title = "Métodos")) + 
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1))

accuracy(mediaLibros)
accuracy(naiveLibros)
accuracy(derivaLibros)

#- Metodo sencillo para nacimientos
(snaive.nacimientos <- snaive(nacimientos, h = 24, level = 95))

autoplot(snaive.nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y predicción por el método Ingenuo con estacionalidad")

accuracy(snaive.nacimientos)

#- Evaluacion predicciones training/test
librosIntra <- subset(libros, end = length(libros) - 6)
librosExtra <- subset(libros, start = length(libros) - 5)

librosExtraPre <- rwf(librosIntra,  h = 6, drift = TRUE)

accuracy(librosExtraPre, librosExtra)


nacimientosIntra <- subset(nacimientos, end = length(nacimientos) - 36)
nacimientosExtra <- subset(nacimientos, start = length(nacimientos) - 35)

nacimientosExtraPre <- snaive(nacimientosIntra, h = 36)

accuracy(nacimientosExtraPre, nacimientosExtra)

#- Evaluacion predicciones origen prediccion movil
nacAnual <- aggregate(nacimientos, FUN = sum)
k <- 20                 
h <- 5                  
TT <- length(nacAnual)  
s <- TT - k - h       

mapeRwf <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(nacAnual, start = i + 1, end = i + k)
  test.set <-  subset(nacAnual, start = i + k + 1, end = i + k + h)
  
  fcast <- rwf(train.set, h = h, drift = TRUE)
  mapeRwf[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeRwf <- colMeans(mapeRwf)
round(mapeRwf, 2)

#- Alisado exponencial simple
librosf <- ses(libros, h = 5, level = 95)
summary(librosf)

librosf$model$states

autoplot(librosf,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado simple")

#- Alisado de Holt
librosf <- holt(libros, h = 5, level = 95)
summary(librosf)

librosf$model$states

autoplot(librosf,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado de Holt")

#- Alisado de Holt con amortiguamiento
librosfd <- holt(libros, damped = TRUE, h = 15, phi = 0.9)
summary(librosfd)

autoplot(librosfd,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado exponencial con amortiguamiento",
         PI = FALSE)

#- Alisado de Holt-Winters multiplicativo       
nacimientosb <- window(nacimientos, start = 2000)
nacimientosbf <- hw(nacimientosb, seasonal = "mult", h = 24)
summary(nacimientosbf)

TT <- nrow(nacimientosbf$model$states)
nacimientosbf$model$states[TT,]

(nacimientosbf$model$states[TT, 1] + (1:12)*nacimientosbf$model$states[TT, 2]) * 
  nacimientosbf$model$states[TT, 14:3]

autoplot(nacimientosbf,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y predicción con alisado de Holt-Winters multiplicativo",
         PI = FALSE)

#- Alisado de Holt-Winters aditivo para el logaritmo con correccion de sesgo   
nacimientosbfl <- hw(nacimientosb, 
                     seasonal = "addit", 
                     h = 24, 
                     lambda = 0, 
                     biasadj = TRUE)

autoplot(nacimientosb,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y dos predicciones con alisado de Holt-Winters") + 
  autolayer(nacimientosbf, series = "Nacimientos", PI = FALSE) + 
  autolayer(nacimientosbfl, series = "Nacimientos (log)", PI = FALSE) + 
  guides(colour = guide_legend(title = "Predicción")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 

#- Alisado de Holt-Winters aditivo para el logaritmo sin correccion de sesgo   
nacimientosbfl2 <- hw(nacimientosb, 
                      seasonal = "addit", 
                      h=24, 
                      lambda = 0,
                      biasadj = FALSE)

nacimientosbf$mean
nacimientosbfl$mean
nacimientosbfl2$mean


#- Alisado exponencial: funcion ETS - Serie Libros
librosEts <- ets(libros)
summary(librosEts) 

librosEtsPre <- forecast(librosEts, h = 5)
librosEtsPre
autoplot(librosEtsPre,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción a 5 años vista")

error <- residuals(librosEts, type = "response")
sderror <- sd(error)
autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1993, 2019, 2)) 

librosIntra <- subset(libros, end = length(libros) - 6)
librosExtra <- subset(libros, start = length(libros) - 5)
librosIntraEts <- ets(librosIntra, model = "MNN")
librosExtraPre <- forecast(librosIntraEts, h = 6)
accuracy(librosExtraPre, librosExtra)

#- Alisado exponencial: funcion ETS - Serie Nacimientos
nacimientosEts <- ets(nacimientosb, damped = FALSE)
summary(nacimientosEts) 

autoplot(nacimientosEts,
         xlab = "Periodo",
         main = "Componentes del modelo óptimo para Nacimientos")

TT <- nrow(nacimientosEts$states)
nacimientosEts$states[TT,]

nacimientosEts$states[TT, 1] + (1:12) * nacimientosEts$states[TT, 2] + 
  nacimientosEts$states[TT, 14:3]

nacimientosEtsPre <- forecast(nacimientosEts, h = 24, level = 95)
nacimientosEtsPre

autoplot(nacimientosEtsPre,
         xlab = "",
         ylab = "Bebés",
         main = "Nacimientos y predicción")


error <- residuals(nacimientosEts, type = "response")
sderror <- sd(error)
autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "Error + Intervención") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2019, 2)) 


k <- 120                  
h <- 12                   
TT <- length(nacimientosb)
s <- TT - k - h           

mapeEts <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(nacimientosb, start = i + 1, end = i + k)
  test.set <-  subset(nacimientosb, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set,  model = "MAA", damped = FALSE)
  fcast <- forecast(fit, h = h)
  mapeEts[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeEts <- colMeans(mapeEts)
round(mapeEts, 2)

# Metodos alternativos: Serie original
accuracy(ets(nacimientosb, 
             damped = FALSE))
accuracy(ets(nacimientosb, 
             damped = FALSE, 
             opt.crit = "mse"))

# Metodos alternativos: Transformación logarítmica
accuracy(ets(nacimientosb, 
             lambda = 0, 
             damped = FALSE))
accuracy(ets(nacimientosb, 
             lambda = 0, 
             damped = FALSE, 
             opt.crit = "mse"))

# Metodos alternativos: Transformación logarítmica insesgada
accuracy(ets(nacimientosb, 
             lambda = 0, 
             biasadj = TRUE,
             damped = FALSE))
accuracy(ets(nacimientosb, 
             lambda = 0, 
             biasadj = TRUE,
             damped = FALSE, 
             opt.crit = "mse"))

# Metodos alternativos: Nacimientos por dia
accuracy(ets(nacimientosb/monthdays(nacimientosb), 
             damped = FALSE))
accuracy(ets(nacimientosb/monthdays(nacimientosb), 
             damped = FALSE, 
             opt.crit = "mse"))

