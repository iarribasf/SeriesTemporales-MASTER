#----------------------------------------------------------
# CODIGO TEMA 6
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Librerias
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(lmtest)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Residuos
residuos <- read.csv2("./series/Residuos.csv",
                      header = TRUE)

residuos <- ts(residuos[, 2],
               start = 1995, 
               frequency  = 1)

autoplot(residuos,
         xlab = "", 
         ylab = "Kg por cápita", 
         main = "")

# Aforo vehículos
aforo <- read.csv2("./series/aforo_oropesa.csv", 
                   header = TRUE)

aforo <- ts(aforo[, 1], 
            start = 1960, 
            freq = 1)

autoplot(aforo, 
         xlab = "", 
         ylab = "Vehículos por día",
         main = "")

# Nacimientos
nacimientos <- read.csv2("./series/Nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2]/1000, 
                  start = 1975, 
                  frequency = 12)

nacimientos <- aggregate(nacimientos, FUN = sum)

nacimientos <- window(nacimientos, start = 2000)

autoplot(nacimientos, 
         main = "", 
         xlab = "", 
         ylab = "Nacimientos (miles)")
#----------------------------------------------------------
#
# METODO SENCILLO: RESIDUOS
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Metodo de la deriva
derivaResiduos <- rwf(residuos, 
                      h = 5, 
                      drift = TRUE)

summary(derivaResiduos)

autoplot(derivaResiduos, 
         series = "",
         xlab = "",
         ylab = "Kg per cápita",
         main = "")
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: RESIDUOS
#
#----------------------------------------------------------
# Alisado de Holt
#----------------------------------------------------------
# Ajuste
etsResiduos <- ets(residuos, 
                   model = "AAN",
                   damped = FALSE)

summary(etsResiduos)

# Prediccion
tail(etsResiduos$states, 1)

etsResiduosf <- forecast(etsResiduos,
                         h = 5, 
                         level = 95)

etsResiduosf

autoplot(etsResiduosf,
         xlab = "",
         ylab = "Kg. per cápita",
         main = "")

#----------------------------------------------------------
# Alisado de Holt con amortiguamiento
#----------------------------------------------------------
# Ajuste
etsDResiduos <- ets(residuos, 
                    model = "AAN", 
                    damped = TRUE)

summary(etsDResiduos)

# Prediccion
etsDResiduosf <- forecast(etsDResiduos, 
                          h = 15,
                          level = 95)

etsDResiduosf

autoplot(etsDResiduosf,
         xlab = "",
         ylab = "kg per cápita",
         main = "",
         PI = FALSE)
#----------------------------------------------------------
# Ajuste sin restricciones
#----------------------------------------------------------
summary(ets(residuos))
#----------------------------------------------------------
#
# ARIMA: RESIDUOS
#
#----------------------------------------------------------
# Diferenciacion
ndiffs(residuos)

autoplot(residuos)
autoplot(diff(residuos))

# Identificacion
auto.arima(residuos, 
           d = 1)

# Estimacion + Atipicos
arima110 <- Arima(residuos, 
                  order = c(1, 1, 0), 
                  include.constant = FALSE)
arima110

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
  scale_x_continuous(breaks= seq(1995, 2023, 2)) 

# Validacion
coeftest(arima110)

accuracy(arima110)

# Prediccion
parima110 <- forecast(arima110, 
                      h = 5, 
                      level = 95)
parima110

autoplot(parima110, 
         xlab = "", 
         ylab = "Kg. per cápita",
         main = "") +
  scale_x_continuous(breaks= seq(1995, 2028, 2)) 
#----------------------------------------------------------
#
# USO DEL LOGARITMO (EJEMPLO CON ALISADO)
#
#----------------------------------------------------------
# Ajuste
etsLResiduos <- ets(residuos, 
                    lambda = 0,
                    biasadj = TRUE)

summary(etsLResiduos)

# Prediccion
etsLResiduosf <- forecast(etsLResiduos,
                          h = 15,
                          level = 95,
                          biasadj = TRUE)

etsLResiduosf

autoplot(residuos,
         xlab = "",
         ylab = "Kg. per cápita",
         main = "") + 
  autolayer(etsDResiduosf, series = "Serie original", PI = FALSE) + 
  autolayer(etsLResiduosf, series = "Trans. logarítmica", PI = FALSE) + 
  guides(colour = guide_legend(title = "Predicción")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 

#----------------------------------------------------------
#
# METODO SENCILLO: AFORO
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Metodo de la deriva
derivaAforo <- rwf(aforo, 
                   h = 4, 
                   drift = TRUE)

summary(derivaAforo)

autoplot(derivaAforo, 
         series = "",
         xlab = "",
         ylab = "",
         main = "")
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: AFORO
#
#----------------------------------------------------------
# Ajuste
etsAforo <- ets(aforo)

summary(etsAforo) 

# Prediccion
tail(etsAforo$states, 1)

etsResiduosPre <- forecast(etsAforo, 
                           h = 4,
                           level = 95)
etsResiduosPre

autoplot(etsResiduosf,
         xlab = "",
         ylab = "Kg. per cápita",
         main = "")

# Valores atípicos
error <- residuals(etsAforo)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1960, 2022, 4)) 

fechas <- format(seq(as.Date("1960-01-01"), as.Date("2022-01-01"), "year"), "%Y")
fechas[abs(error) > 2.5 * sderror]

atipicos <- tsoutliers(error)
fechas[atipicos$index]
#----------------------------------------------------------
#
# ARIMA: AFORO
#
#----------------------------------------------------------
# Diferenciacion
ndiffs(aforo)

autoplot(aforo)
autoplot(diff(aforo))

# Identificacion
auto.arima(aforo, 
           d = 1)

# Estimacion + Atipicos (1 de 2)
arima010 <- Arima(aforo, 
                  order = c(0, 1, 0),
                  include.constant = FALSE)

error <- residuals(arima010)
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
  scale_x_continuous(breaks= seq(1960, 2020, 4)) 

fechas[abs(error) > 2.5 * sderror]

# Estimacion + Atipicos (2 de 2)
d1979 <- 1*(time(error) == 1979)
d2011 <- 1*(time(error) == 2011)
d2020 <- 1*(time(error) == 2020)

auto.arima(aforo,
           d = 1,
           xreg = cbind(d1979, d2011, d2020))

arima210 <- Arima(aforo, 
                  order = c(2, 1, 0),
                  include.constant = FALSE,
                  xreg = cbind(d1979, d2011, d2020))
arima210

error <- residuals(arima210)
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
  scale_x_continuous(breaks= seq(1960, 2020, 4)) 

# Validacion
coeftest(arima210)

accuracy(arima210)

# Prediccion
parima210 <- forecast(arima210, 
                      h = 4, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 4), d2011=rep(0, 4), 
                                   d2020=rep(0, 4)))
parima210

autoplot(parima110, 
         xlab = "", 
         ylab = "Kg. per cápita",
         main = "") +
  scale_x_continuous(breaks= seq(1995, 2028, 2)) 
#----------------------------------------------------------
#
# COMPARACION ENTRE MODELOS: AFORO
#
#----------------------------------------------------------
autoplot(aforo,
         xlab = "",
         ylab = "Vehículos por día",
         main = "") + 
  autolayer(derivaAforo$mean, series = "Deriva", PI = FALSE) + 
  autolayer(etsResiduosPre$mean, series = "Alisado", PI = FALSE) + 
  autolayer(parima210$mean, series = "Arima", PI = FALSE) + 
  guides(colour = guide_legend(title = "Método")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 


k <- 30
h <- 4
TT <- length(aforo)
s <- TT - k - h

rmseDer <- rmseAli <- rmseAri <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(aforo, start = i + 1, end = i + k)
  test.set <-  subset(aforo, start = i + k + 1, end = i + k + h)
  
  fcast <- rwf(train.set, h = h, drift = TRUE)
  rmseDer[i + 1,] <- (test.set - fcast$mean)^2
  
  fit <- ets(train.set, model = "MAN", damped = FALSE)
  fcast <- forecast(fit, h = h)
  rmseAli[i + 1,] <- (test.set - fcast$mean)^2
  
  fit <- Arima(train.set, order = c(2, 1, 0), include.constant = FALSE)
  fcast <- forecast(fit, h = h)
  rmseAri[i + 1,] <- (test.set - fcast$mean)^2
}

rmseDerMedia <- sqrt(colMeans(rmseDer))
rmseAliMedia <- sqrt(colMeans(rmseAli))
rmseAriMedia <- sqrt(colMeans(rmseAri))

round(rmseDerMedia, 2)
round(rmseAliMedia, 2)
round(rmseAriMedia, 2)
#----------------------------------------------------------
#
# METODO SENCILLO: NACIMIENTOS
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Metodo de la deriva
derivaNacimientos <- rwf(nacimientos, 
                         h = 4, 
                         lambda = 0, 
                         drift = TRUE)

summary(derivaNacimientos)
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: NACIMIENTOS (LOG)
#
#----------------------------------------------------------
# Ajuste
etsNacimientos <- ets(nacimientos, 
                      lambda = 0)

summary(etsNacimientos)

autoplot(etsNacimientos)

# Prediccion
tail(etsNacimientos$states, 1)

etsNacimientosPre <- forecast(etsNacimientos, 
                              h = 4,
                              level = 95)
etsNacimientosPre
#----------------------------------------------------------
#
# ARIMA: NACIMIENTOS
#
#----------------------------------------------------------
# Diferenciacion
ndiffs(log(nacimientos))

autoplot(log(nacimientos))
autoplot(diff(log(nacimientos)))

# Identificacion
auto.arima(nacimientos, 
           d = 1,
           lambda = 0)

# Estimacion + Atipicos (1 de 2)
arima210 <- Arima(nacimientos, 
                  order = c(2, 1, 0),
                  include.constant = FALSE)

arima210

error <- residuals(arima210)
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
  scale_x_continuous(breaks= seq(2000, 2024, 4)) 

fechas <- format(seq(as.Date("2000-01-01"), as.Date("2023-01-01"), "year"), "%Y")

fechas[abs(error) > 2.5 * sderror]

# Estimacion + Atipicos (2 de 2)
d2009 <- 1*(time(error) == 2009)

auto.arima(nacimientos,
           d = 1,
           lambda = 0,
           xreg = cbind(d2009))

arima110 <- Arima(nacimientos, 
                  order = c(1, 1, 0),
                  include.constant = FALSE,
                  lambda = 0,
                  xreg = cbind(d2009))

arima110

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
  scale_x_continuous(breaks= seq(2000, 2024, 4)) 

fechas[abs(error) > 2.5 * sderror]

# Validacion
coeftest(arima110)

accuracy(arima110)

# Prediccion
parima110 <- forecast(arima110, 
                      h = 4, 
                      level = 95,
                      xreg = cbind(d1979=rep(0, 4)))
parima110 
#----------------------------------------------------------
#
# COMPARACION ENTRE MODELOS: NACIMIENTOS
#
#----------------------------------------------------------
autoplot(nacimientos,
         xlab = "",
         ylab = "Bebés",
         main = "") + 
  autolayer(derivaNacimientos$mean, series = "Deriva") + 
  autolayer(etsNacimientosPre$mean, series = "Alisado") + 
  autolayer(parima110$mean, series = "Arima") + 
  guides(colour = guide_legend(title = "Método")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 



k <- 20
h <- 4
TT <- length(nacimientos)
s <- TT - k - h

mapeDer <- mapeAli <- mapeAri <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h)
  
  fcast <- rwf(train.set, h = h, drift = TRUE, lambda = 0)
  mapeDer[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAN", damped = FALSE, lambda = 0)
  fcast <- forecast(fit, h = h)
  mapeAli[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- Arima(train.set, order = c(1, 1, 0), include.constant = FALSE, lambda = 0)
  fcast <- forecast(fit, h = h)
  mapeAri[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeDerMedia <- colMeans(mapeDer)
mapeAliMedia <- colMeans(mapeAli)
mapeAriMedia <- colMeans(mapeAri)

round(mapeDerMedia, 2)
round(mapeAliMedia, 2)
round(mapeAriMedia, 2)






