#---------------------------------------------------------------
# Codigo Tema 2: junio 2020
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)

#- Serie libros
libros <- read.csv2("libros.csv", header = TRUE)
libros <- ts(libros["libros"], start = 1993, frequency  = 1)

#- Serie nacimientos
nacimientos <- read.csv2("nacimientos.csv", header = TRUE)
nacimientos <- ts(nacimientos["nacimientos"],
                  start = c(1975, 1),
                  frequency = 12)

#- Metodos sencillos para libros
mediaLibros <- meanf(libros, h = 5)
naiveLibros <- naive(libros, h = 5)
derivaLibros <- rwf(libros,  h = 5, drift = TRUE)
 
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
snaive.nacimientos <- snaive(nacimientos, h = 24, level = 95)

autoplot(snaive.nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos y predicción por el método Ingenuo con estacionalidad")

accuracy(snaive.nacimientos)

#- Alisado exponencial simple
librosf <- ses(libros, h = 5, level = 95)
summary(librosf)

autoplot(librosf,
         xlab = "",
         ylab = "Títulos",
         main = "Libros y predicción con alisado simple")

#- Alisado de Holt
librosf <- holt(libros, h = 5, level = 95)
summary(librosf)
tail(librosf$model$states, n = 1)

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

#- Alisado de Holt-Winters aditivo para el logartimo con correcion de sesgo   
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

#- Alisado de Holt-Winters aditivo para el logartimo sin correcion de sesgo   
nacimientosbfl2 <- hw(nacimientosb, seasonal = "addit", h=24, lambda = 0, biasadj = FALSE)
datos <- cbind(
  `Sin transformar` = nacimientosbf$mean,
  `log(Nac) insesgadas` = nacimientosbfl$mean,
  `log(Nac) sesgadas` = nacimientosbfl2$mean
  )
head(datos, 12)

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

librosIntra <- subset(libros, end = length(libros) - 7)
librosExtra <- subset(libros, start = length(libros) - 6)
librosIntraEts <- ets(librosIntra, model = "MNN", damped = FALSE)
librosExtraPre <- forecast(librosIntraEts, h = 7)
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


nacimientosIntra <- subset(nacimientosb, end = length(nacimientosb) - 36)
nacimientosExtra <- subset(nacimientosb, start = length(nacimientosb) - 35)
nacimientosIntraEts <- ets(nacimientosIntra)
nacimientosExtraPre <- forecast(nacimientosIntraEts, h = 36)
accuracy(nacimientosExtraPre, nacimientosExtra)

#- Modelos alternativos: serie original
accuracy(ets(nacimientos, damped = TRUE))[5]
accuracy(ets(nacimientos, damped = TRUE, opt.crit = "mse"))[5]
accuracy(ets(nacimientos, damped = TRUE, allow.multiplicative.trend = TRUE))[5]
accuracy(ets(nacimientos, damped = TRUE, allow.multiplicative.trend = TRUE, opt.crit = "mse"))[5]

#- Modelos alternativos: transformación logarítmica
accuracy(ets(nacimientos, lambda = 0, damped = TRUE))[5]
accuracy(ets(nacimientos, lambda = 0, damped = TRUE, opt.crit = "mse"))[5]

#- Modelos alternativos: nacimientos por dia
accuracy(ets(nacimientos/monthdays(nacimientos), damped = TRUE))[5]
accuracy(ets(nacimientos/monthdays(nacimientos), damped = TRUE, opt.crit = "mse"))[5]
accuracy(ets(nacimientos/monthdays(nacimientos), damped = TRUE, allow.multiplicative.trend = TRUE))[5]
accuracy(ets(nacimientos/monthdays(nacimientos), damped = TRUE, allow.multiplicative.trend = TRUE, opt.crit = "mse"))[5]

