#---------------------------------------------------------------
# Codigo Tema 3
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)
library(urca)

#- Serie libros
libros <- read.csv2("series/libros.csv", header = TRUE)
libros <- ts(libros[, 2], start = 1993, freq = 1)

#- Serie nacimientos
nacimientos <- read.csv2("series/nacimientos.csv", header = TRUE)
nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  freq = 12)

#- Gráficas para la transformacion diferencia
autoplot(libros)
autoplot(diff(libros))

autoplot(nacimientos)
autoplot(diff(nacimientos))
autoplot(diff(nacimientos, lag = 12))
autoplot(diff(diff(nacimientos, lag = 12)))

#- Gráficas para la transformacion logaritmica y diferencia
autoplot(log(nacimientos))
autoplot(diff(log(nacimientos)))
autoplot(diff(log(nacimientos), lag = 12))

#- Box-Cox
(ll <- BoxCox.lambda(nacimientos) )
autoplot(BoxCox(nacimientos, ll))

#- Funcion de autocorrelacion - Libros
ggAcf(libros, lag = 10)
ggAcf(diff(libros), lag = 10)
 
ggAcf(diff(libros), lag = 10, plot = FALSE)

#- Funcion de autocorrelacion - Nacimientos
ggAcf(nacimientos, lag = 48)
ggAcf(log(nacimientos), lag = 48)

ggAcf(diff(nacimientos), lag = 48)
ggAcf(diff(log(nacimientos)), lag = 48)

ggAcf(diff(nacimientos, lag = 12), lag = 48)
ggAcf(diff(log(nacimientos), lag = 12), lag = 48)

ggAcf(diff(diff(nacimientos), lag = 12), lag = 48)
ggAcf(diff(diff(log(nacimientos)), lag = 12), lag = 48)

ggAcf(diff(diff(nacimientos, lag=12)), lag = 24, plot = FALSE)

#- Funcion de autocorrelacion parcial - Libros
ggPacf(libros, lag = 10)
ggPacf(diff(libros), lag = 10)

ggPacf(diff(libros), lag = 10, plot = FALSE)

ggtsdisplay(diff(libros))

#- Funcion de autocorrelacion parcial - Nacimientos  
ggPacf(nacimientos, lag = 48)
ggPacf(diff(diff(nacimientos), lag = 12), lag = 48)
  
#- Contraste de raices unitarias - Libros
summary(ur.kpss(libros, type='tau', lags = 'short'))
summary(ur.kpss(libros, type='mu', lags = 'short'))


summary(ur.kpss(libros, type='tau', use.lag = 3)) # Variar use.lag de 1 a 8

ndiffs(libros, alpha = 0.05, test = "kpss", type = "trend")

#- Contraste de raices unitarias - Nacimientos
nacimientosAnual<-aggregate(nacimientos, FUN = sum)
summary(ur.kpss(nacimientosAnual, type='tau', lags = "short"))

summary(ur.kpss(nacimientosAnual, type='tau', use.lag = 3)) #Variar use.lag de 1 a 8

ndiffs(nacimientosAnual, alpha = 0.05, test = "kpss", type = "trend")

