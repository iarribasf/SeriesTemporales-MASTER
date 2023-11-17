#----------------------------------------------------------
# CODIGO TEMA 2
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(urca)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Libros
libros <- read.csv2("./series/libros.csv", 
                    header = TRUE)

libros <- ts(libros[, 2], 
             start = 1993, 
             frequency  = 1)

autoplot(libros,
         xlab = "",
         ylab = "Títulos",
         main = "")

# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Diferenciacion
#----------------------------------------------------------
# Libros
autoplot(libros)
autoplot(diff(libros))

# Nacimientos
autoplot(nacimientos)
autoplot(diff(nacimientos))
autoplot(diff(nacimientos, lag = 12))
autoplot(diff(diff(nacimientos, lag = 12)))

ndiffs(nacimientos)
nsdiffs(nacimientos)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Box-Cox
#----------------------------------------------------------
(ll <- BoxCox.lambda(nacimientos) )
autoplot(BoxCox(nacimientos, ll))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Tasas de variación
#----------------------------------------------------------
# Nacimientos
autoplot(log(nacimientos))
autoplot(diff(log(nacimientos)))
autoplot(diff(log(nacimientos), lag = 12))
autoplot(diff(diff(log(nacimientos)), lag = 12))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion de autocorrelacion
#----------------------------------------------------------
# Libros
ggAcf(libros, lag = 10, ylim = c(-1 ,1))
ggAcf(diff(libros), lag = 10, ylim = c(-1 ,1))
 
ggAcf(diff(libros), lag = 10, plot = FALSE)

# Nacimientos
ggAcf(nacimientos, lag = 48, ylim = c(-1 ,1))
ggAcf(log(nacimientos), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(nacimientos), lag = 48, ylim = c(-1 ,1))
ggAcf(diff(log(nacimientos)), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(nacimientos, lag = 12), lag = 48, ylim = c(-1 ,1))
ggAcf(diff(log(nacimientos), lag = 12), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(diff(nacimientos), lag = 12), lag = 48, ylim = c(-1 ,1))
ggAcf(diff(diff(log(nacimientos)), lag = 12), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(diff(nacimientos, lag=12)), lag = 24, plot = FALSE)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion de autocorrelacion parcial
#----------------------------------------------------------
# Libros 
ggPacf(libros, lag = 10, ylim = c(-1 ,1))
ggPacf(diff(libros), lag = 10, ylim = c(-1 ,1))

ggPacf(diff(libros), lag = 10, plot = FALSE)

ggtsdisplay(diff(libros))

# Nacimientos  
ggPacf(nacimientos, lag = 48, ylim = c(-1 ,1))
ggPacf(diff(diff(nacimientos), lag = 12), lag = 48, ylim = c(-1 ,1))

ggPacf(diff(diff(nacimientos), lag = 12), lag = 48, plot = FALSE)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Contraste de raices unitarias
#---------------------------------------------------------- 
# Libros
summary(ur.kpss(libros, type='tau', lags = 'short'))
summary(ur.kpss(libros, type='tau', use.lag = 3)) # Variar use.lag de 1 a 8

summary(ur.kpss(libros, type='mu', lags = 'short'))

ndiffs(libros, alpha = 0.05, test = "kpss", type = "trend")

# Nacimientos
nacimientosAnual<-aggregate(nacimientos, FUN = sum)
summary(ur.kpss(nacimientosAnual, type='tau', lags = "short"))
summary(ur.kpss(nacimientosAnual, type='tau', use.lag = 3)) #Variar use.lag de 1 a 8

summary(ur.kpss(diff(nacimientosAnual), type='tau', lags = "short"))
summary(ur.kpss(diff(nacimientosAnual), type='mu', lags = "short"))

ndiffs(nacimientosAnual, alpha = 0.05, test = "kpss", type = "trend")

