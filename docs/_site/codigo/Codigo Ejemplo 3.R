#---------------------------------------------------------------
# Codigo ejemplo tema 3
#---------------------------------------------------------------
#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)
library(urca)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("./series/Enfermedades cerebrovasculares.csv", 
                       header = TRUE)

DefEnfCer <- ts(DefEnfCer[,2], 
                start = 1980, 
                frequency = 12)

DefEnfCer <- window(DefEnfCer, 
                    start = 1990)

autoplot(DefEnfCer,
         xlab = "",
         ylab = "Defunciones",
         main = "") +
  scale_x_continuous(breaks= seq(1990, 2024, 2)) 

#- Transformacion logartimica
(nl <- BoxCox.lambda(DefEnfCer))
wDefEnfCer <-BoxCox(DefEnfCer, lambda = nl)

series <- cbind("Original" = DefEnfCer,
                "Transformación Box-Cox" = wDefEnfCer,
                "Logaritmo" = log(DefEnfCer))

autoplot(series, facets = TRUE,
         xlab = "",
         ylab = "",
         main = "")

#- FAC
ggAcf(log(DefEnfCer), lag = 48, ylim = c(-1, 1))
ggAcf(diff(log(DefEnfCer)), lag = 48, ylim = c(-1, 1))
ggAcf(diff(log(DefEnfCer), lag = 12),lag = 48, ylim = c(-1, 1))
ggAcf(diff(diff(log(DefEnfCer), lag=12)), lag = 48, ylim = c(-1, 1))

ndiffs(log(DefEnfCer))
nsdiffs(log(DefEnfCer))

series <- cbind("Original" = DefEnfCer,
                "Dif reg. y est. de log" = diff(diff(log(DefEnfCer), lag = 12)))
autoplot(series, facets = TRUE,
         xlab = "",
         ylab = "",
         main = "")

ggtsdisplay(diff(diff(log(DefEnfCer)), lag = 12))

#- Contraste de raices unitarias
DefEnfCerAnual <- aggregate(DefEnfCer, FUN = sum)

autoplot(DefEnfCerAnual,
         xlab = "",
         ylab = "",
         main = "") 

summary(ur.kpss(DefEnfCer, type='tau', lags = 'short'))

summary(ur.kpss(DefEnfCer, type='mu', lags = 'short'))

summary(ur.kpss(diff(DefEnfCer), type='mu', lags = 'short'))
