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
         main = "",
         xlab = "",
         ylab = "Kg per cápita")

# Nacimientos
nacimientos <- read.csv2("./series/Nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")


# Demanda electrica
electricidad <- read.csv("./series/Consumo electrico.csv", 
                         header = TRUE)

electricidad <- ts(electricidad[, 1],
                   start = c(1, 7),
                   frequency = 7)

autoplot(electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")

# Alimentos per capita
alimentospc <- read.csv2("./series/Alimentacionpc.csv",
                         header = TRUE)

alimentospc <- ts(alimentospc,
                  start = 1990, 
                  freq = 1)

autoplot(alimentospc, 
         xlab = "", 
         ylab = "Kg per cápita",
         main = "",
         ylim = c(0, 700))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Metodos sencillos
#----------------------------------------------------------
# Residuos
(mediaResiduos <- meanf(residuos, h = 5))
(naiveResiduos <- naive(residuos, h = 5))
(derivaResiduos <- rwf(residuos, drift = TRUE , h = 5))
derivaResiduos$model$par$drift

autoplot(residuos, 
         series = "Residuos",
         xlab = "",
         ylab = "Kg per cápita",
         main = "") +
  autolayer(mediaResiduos, series="Media", PI = FALSE) +
  autolayer(naiveResiduos, series="Ingenuo", PI = FALSE) +
  autolayer(derivaResiduos, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Residuos", "Media", 
                                 "Ingenuo", "Deriva")) +
  labs(colour="Métodos") + 
  theme(legend.position=c(0.2,0.3))

accuracy(mediaResiduos)
accuracy(naiveResiduos)
accuracy(derivaResiduos)

summary(mediaResiduos)
summary(naiveResiduos) 
summary(derivaResiduos)

# Nacimientos
(snaive.nacimientos <- snaive(nacimientos, 
                              h = 24, 
                              level = 95))
accuracy(snaive.nacimientos)

summary(snaive.nacimientos)

autoplot(snaive.nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "",
         PI = FALSE,
         xlim = c(2000, 2025))

# Demanda electrica
snaive.electricidad <- snaive(electricidad, 
                              h = 28, 
                              level = 95)

accuracy(snaive.electricidad)

summary(snaive.electricidad)

autoplot(snaive.electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Evaluación de las predicciones: training set/test set
#----------------------------------------------------------
# Residuos
residuosIntra <- subset(residuos, end = length(residuos) - 7)
residuosExtra <- subset(residuos, start = length(residuos) - 6)

residuosExtraPre <- rwf(residuosIntra,  h = 7, drift = TRUE)

accuracy(residuosExtraPre, residuosExtra)

# Nacimientos
nacimientosIntra <- subset(nacimientos, end = length(nacimientos) - 36)
nacimientosExtra <- subset(nacimientos, start = length(nacimientos) - 35)

nacimientosExtraPre <- snaive(nacimientosIntra, h = 36)

accuracy(nacimientosExtraPre, nacimientosExtra)

# Demanda electrica
electricidadIntra <- subset(electricidad, end = length(electricidad) - 56)
electricidadExtra <- subset(electricidad, start = length(electricidad) - 55)

electricidadExtraPre <- snaive(electricidadIntra, h = 56)

accuracy(electricidadExtraPre, electricidadExtra)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Evaluación de las predicciones:  Origen de predicción móvil
#----------------------------------------------------------
# Nacimientos
nacAnual <- aggregate(nacimientos, FUN = sum)

k <- 20                   #Minimo numero de datos para estimar
h <- 5                    #Horizonte de las prediciciones
TT <- length(nacAnual)    #Longitud serie
s <- TT - k - h           #Total de estimaciones

mapeRwf <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(nacAnual, start = i + 1, end = i + k)
  test.set <-  subset(nacAnual, start = i + k + 1, end = i + k + h)
  
  fcast <- rwf(train.set, h = h, drift = TRUE)
  mapeRwf[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeRwfMedia <- colMeans(mapeRwf)
round(mapeRwfMedia, 2)

mapeRwfMediana <- apply(mapeRwf, MARGIN = 2, FUN = median)
round(mapeRwfMediana, 2)

# Demanda electrica
k <- 140                  
h <- 28                   
TT <- length(electricidad)
s <- TT - k - h           

rmseRwf <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(electricidad, start = i + 1, end = i + k)
  test.set <-  subset(electricidad, start = i + k + 1, end = i + k + h)
  
  fcast <- snaive(train.set, h = h)
  rmseRwf[i + 1,] <- (test.set - fcast$mean)^2
}

rmseRwfMedia <- sqrt(colMeans(rmseRwf))
round(rmseRwfMedia, 2)

rmseRwfMediana <- sqrt(apply(rmseRwf, MARGIN = 2, FUN = median))
round(rmseRwfMediana, 2)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Alisado exponencial
#----------------------------------------------------------
# Alisado simple
etsAlimentospc <- ets(alimentospc, 
                      model = "ANN")

summary(etsAlimentospc)

etsAlimentospc$states

etsAlimentospcf <- forecast(etsAlimentospc,
                            h = 5, 
                            level = 95)
etsAlimentospcf

autoplot(etsAlimentospcf,
         xlab = "",
         ylab = "Kg per cápita",
         main = "")

# Alisado de Holt
etsResiduos <- ets(residuos, 
                   model = "AAN",
                   damped = FALSE)
summary(etsResiduos)

etsResiduos$states

etsResiduosf <- forecast(etsResiduos,
                         h = 5, 
                         level = 95)
etsResiduosf

autoplot(etsResiduosf,
         xlab = "",
         ylab = "Kg per cápita",
         main = "")

# Alisado de Holt con pendiente amortiguada
etsDResiduos <- ets(residuos, 
                    model = "AAN", 
                    damped = TRUE)
summary(etsDResiduos)

etsDResiduosf <- forecast(etsDResiduos, 
                          h = 15,
                          level = 95)
etsDResiduosf

autoplot(etsDResiduosf,
         xlab = "",
         ylab = "Kg per cápita",
         main = "",
         PI = FALSE)

# Alisado de HW Aditivo
electricidadr <- window(electricidad, 
                        start = c(6, 1), 
                        end = c(22, 7)) 

electricidadEts <- ets(electricidadr, 
                       model = "AAA", 
                       damped = FALSE)

summary(electricidadEts)

autoplot(electricidadEts)

TT <- nrow(electricidadEts$states)
electricidadEts$states[TT,]

electricidadEts$states[TT, 1] + (1:7)*electricidadEts$states[TT, 2] + 
  electricidadEts$states[TT, 9:3]

electricidadf <- forecast(electricidadEts,
                          h = 14, 
                          level = 95)
electricidadf

autoplot(electricidadf,
         xlab = "",
         ylab = "GWh",
         main = "",
         PI = FALSE)

# Alisado de HW multiplicativo 
nacimientosb <- window(nacimientos, start = 2000)

nacimientosbEts <- ets(nacimientosb, 
                       model = "MAM", 
                       damped = FALSE)

summary(nacimientosbEts)

autoplot(nacimientosbEts)

TT <- nrow(nacimientosbEts$states)
nacimientosbEts$states[TT,]

(nacimientosbEts$states[TT, 1] + (1:12)*nacimientosbEts$states[TT, 2]) * 
  nacimientosbEts$states[TT, 14:3]

nacimientosbf <- forecast(nacimientosbEts, 
                          h = 24, 
                          level = 95)
nacimientosbf

autoplot(nacimientosbf,
         xlab = "",
         ylab = "Nacimientos",
         main = "",
         PI = FALSE)

# Alisado de Holt-Winters con transformacion logaritmica
nacimientosbEtsl <- ets(nacimientosb, 
                        model = "AAA",
                        damped = FALSE,
                        lambda = 0, 
                        biasadj = TRUE)

summary(nacimientosbEtsl)

nacimientosbfl <- forecast(nacimientosbEtsl,
                           h = 24,
                           level = 95,
                           biasadj = TRUE)
nacimientosbfl

autoplot(nacimientosb,
         xlab = "",
         ylab = "Nacimientos",
         main = "") + 
  autolayer(nacimientosbf, series = "Nacimientos", PI = FALSE) + 
  autolayer(nacimientosbfl, series = "Nacimientos (log)", PI = FALSE) + 
  guides(colour = guide_legend(title = "Predicción")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Residuos
#----------------------------------------------------------
# Ajuste
residuosEts <- ets(residuos)
summary(residuosEts) 

# Prediccion
residuosEtsPre <- forecast(residuosEts, 
                           h = 5,
                           level = 95)
residuosEtsPre

autoplot(residuosEtsPre,
         xlab = "",
         ylab = "Kg per cápita",
         main = "")

# Analisis error
error <- residuals(residuosEts)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1995, 2022, 2)) 

# Error extramuestral: training set/test set
residuoIntra <- subset(residuos, end = length(residuos) - 6)
residuoExtra <- subset(residuos, start = length(residuos) - 5)

residuoIntraEts <- ets(residuoIntra, model = "MNN")

residuoExtraPre <- forecast(residuoIntraEts, h = 6)

accuracy(residuoExtraPre, residuoExtra)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Nacimientos
#----------------------------------------------------------
# Ajuste
nacimientosEts <- ets(nacimientosb)

summary(nacimientosEts) 

autoplot(nacimientosEts,
         xlab = "Periodo",
         main = "")

# Prediccion
TT <- nrow(nacimientosEts$states)
nacimientosEts$states[TT,]

nacimientosEtsPre <- forecast(nacimientosEts, 
                              h = 24, 
                              level = 95)
nacimientosEtsPre

autoplot(nacimientosEtsPre,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Analisis del error
error <- residuals(nacimientosEts)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 

fechas <- format(seq(as.Date("2000-1-1"), as.Date("2023-12-1"), "month"), "%Y-%m")
fechas[abs(error) > 3 * sderror]

# Prueba de Tukey
atipicos <- tsoutliers(error)
fechas[atipicos$index]

# Error extramuestral: origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(nacimientosb)
s <- TT - k - h 

mapeAlisado <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(nacimientosb, start = i + 1, end = i + k)
  test.set <-  subset(nacimientosb, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "AAA", damped = FALSE)
  fcast<-forecast(fit, h = h)
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

errorAlisado <- colMeans(mapeAlisado)
errorAlisado

ggplot() +
  geom_line(aes(x = 1:12, y = errorAlisado)) +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Funcion ets: Demanda electrica
#----------------------------------------------------------
# Ajuste
electricidadEts <- ets(electricidad,
                       damped = FALSE)

summary(electricidadEts)

# Prediccion
TT <- nrow(electricidadEts$states)
electricidadEts$states[TT,]

electricidadEtsPre <- forecast(electricidadEts, 
                               h = 28, 
                               level = 95)
electricidadEtsPre

autoplot(electricidadEtsPre,
         xlab = "",
         ylab = "GWh",
         main = "")

# Analisis del error
error <- residuals(electricidadEts)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Semana",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(6, 26, 2)) 

fechas <- format(seq(as.Date("2023-1-1"), as.Date("2023-12-31"), "day"), "%Y-%m-%d")
fechas[abs(error) > 3 * sderror]

# Prueba de Tukey.
atipicos <- tsoutliers(error)
fechas[atipicos$index]
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Otras alternativas para predecir Nacimientos
#----------------------------------------------------------
# Serie Nacimientos
accuracy(ets(nacimientos))[5]
accuracy(ets(nacimientos, 
             opt.crit = "mse"))[5]
accuracy(ets(nacimientos, 
             opt.crit = "amse",
             nmse = 4))[5]

# Transformación logarítmica
accuracy(ets(nacimientos, 
             lambda = 0))[5]
accuracy(ets(nacimientos, 
             lambda = 0, 
             opt.crit = "mse"))[5]
accuracy(ets(nacimientos, 
             lambda = 0, 
             opt.crit = "amse",
             nmse = 4))[5]

# Transformación logarítmica insesgada
accuracy(ets(nacimientos, 
             lambda = 0,
             biasadj = TRUE))[5]
accuracy(ets(nacimientos, 
             lambda = 0, 
             biasadj = TRUE,
             opt.crit = "mse"))[5]
accuracy(ets(nacimientos, 
             lambda = 0, 
             biasadj = TRUE,
             opt.crit = "amse",
             nmse = 4))[5]

# Nacimientos por dia
accuracy(ets(nacimientos/monthdays(nacimientos)))[5]
accuracy(ets(nacimientos/monthdays(nacimientos), 
             opt.crit = "mse"))[5]
accuracy(ets(nacimientos/monthdays(nacimientos), 
             opt.crit = "amse",
             nmse = 4))[5]

