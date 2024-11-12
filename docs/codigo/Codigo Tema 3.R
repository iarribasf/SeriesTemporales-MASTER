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
residuos <- read.csv2("./series/Residuos.csv", 
                      header = TRUE)

residuos <- ts(residuos[, 2], 
               start = 1995, 
               frequency = 1)

autoplot(residuos,
         xlab = "",
         ylab = "Kg per cápita",
         main = "")

# Nacimientos
nacimientos <- read.csv2("./series/Nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  freq = 12)

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
# Nacimientos
ggAcf(nacimientos, lag = 48, ylim = c(-1 ,1))
ggAcf(log(nacimientos), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(nacimientos), lag = 48, ylim = c(-1 ,1))
ggAcf(diff(log(nacimientos)), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(nacimientos, lag = 12), lag = 48, ylim = c(-1 ,1))
ggAcf(diff(log(nacimientos), lag = 12), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(diff(nacimientos), lag = 12), lag = 48, ylim = c(-1 ,1))
ggAcf(diff(diff(log(nacimientos)), lag = 12), lag = 48, ylim = c(-1 ,1))

ggAcf(diff(diff(nacimientos, lag=12)), 
      lag = 24, 
      plot = FALSE)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion de autocorrelacion parcial
#----------------------------------------------------------
# Nacimientos  
ggPacf(nacimientos, lag = 36)
ggPacf(diff(diff(nacimientos, lag=12)), lag = 36)

ggPacf(diff(diff(nacimientos), lag = 12), 
       lag = 48, 
       plot = FALSE)

ggtsdisplay(diff(diff(nacimientos), lag = 12), main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Contraste de raices unitarias
#---------------------------------------------------------- 
# Residuos
summary(ur.kpss(residuos, 
                type = "tau", 
                lags = "short"))

summary(ur.kpss(residuos, 
                type = "mu", 
                lags = "short"))

summary(ur.kpss(diff(residuos), 
                type = "mu", 
                lags = "short"))

ndiffs(residuos, alpha = 0.05, test = "kpss")

summary(ur.kpss(residuos, 
                type = "tau", 
                use.lag = 4))


# Nacimientos
nacimientosAnual<-aggregate(nacimientos, 
                            FUN = sum)

summary(ur.kpss(nacimientosAnual, 
                type = "tau", 
                lags = "short"))

summary(ur.kpss(nacimientosAnual,
                type='tau', 
                use.lag = 3)) #Variar use.lag de 1 a 8

ndiffs(nacimientosAnual, alpha = 0.05, test = "kpss")

