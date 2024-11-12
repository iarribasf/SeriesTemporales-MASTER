#----------------------------------------------------------
# CODIGO TEMA 7
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
# Nacimientos
nacimientos <- read.csv2("./series/Nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

nacimientos <- window(nacimientos, start = 2000)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Electricidad 
electricidad <- read.csv("./series/Consumo electrico.csv", 
                         header = TRUE)

electricidad <- ts(electricidad[, 1], 
                   start = c(1, 7), 
                   frequency = 7)

autoplot(electricidad, 
         main = "", 
         xlab = "", 
         ylab = "GWh")
#----------------------------------------------------------
#
# METODO SENCILLO: NACIMIENTOS
#
#----------------------------------------------------------
# Metodo ingenuo con estacionalidad
snaive.nacimientos <- snaive(nacimientos, 
                             h = 24, 
                             level = 95)

accuracy(snaive.nacimientos)

autoplot(snaive.nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "",
         PI = FALSE)
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: NACIMIENTOS
#
#----------------------------------------------------------
# Ajuste
nacimientosEts <- ets(nacimientos)

summary(nacimientosEts)

autoplot(nacimientosEts,
         xlab = "Periodo",
         main = "")

# Prediccion
TT <- nrow(nacimientosEts$states)

nacimientosEts$states[TT,]

nacimientosEts$states[TT, 1] + (1:12) * nacimientosEts$states[TT, 2] + nacimientosEts$states[TT, 14:3]

nacimientosEtsPre <- forecast(nacimientosEts, 
                              h = 24, 
                              level = 95)
nacimientosEtsPre

autoplot(nacimientosEtsPre,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Valores atipicos
error <- residuals(nacimientosEts)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "Periodo",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "blue", "blue", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 

fechas <- format(seq(as.Date("2000-1-1"), as.Date("2023-12-1"), "month"), "%Y-%m")
fechas[abs(error) > 3 * sderror]

atipicos <- tsoutliers(error)
fechas[atipicos$index]
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: LOG(NACIMIENTOS)
#
#----------------------------------------------------------
# Ajuste
nacimientosEtsl <- ets(nacimientos, 
                       lambda = 0,
                       damped = FALSE)

summary(nacimientosEtsl)

# Prediccion
nacimientosfl <- forecast(nacimientosEtsl,
                          h = 24,
                          level = 95)

nacimientosfl

# Comparacion
autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "") + 
  autolayer(nacimientosEtsPre, series = "Nacimientos", PI = FALSE) + 
  autolayer(nacimientosfl, series = "Nacimientos (log)", PI = FALSE) + 
  guides(colour = guide_legend(title = "Predicción")) + 
  theme(legend.position=c(0.98,0.98), legend.justification=c(1,1)) 

# Calidad de las predicciones con y sin log
k <- 180
h <- 12
TT <- length(nacimientos)
s <- TT - k - h

mapeAli <- mapeAliL <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "AAA", damped = FALSE)
  fcast <- forecast(fit, h = h)
  mapeAli[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "ANA", damped = FALSE, lambda = 0)
  fcast <- forecast(fit, h = h)
  mapeAliL[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeAliMedia <- colMeans(mapeAli)
mapeAliLMedia <- colMeans(mapeAliL)

round(mapeAliMedia, 2)
round(mapeAliLMedia, 2)

#----------------------------------------------------------
#
# METODO SENCILLO: ELECTRICIDAD
#
#----------------------------------------------------------
# Metodo ingenuo con estacionalidad
snaive.electricidad <- snaive(electricidad, 
                              h = 28,
                              level = 95)

accuracy(snaive.electricidad)

autoplot(snaive.electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: ELECTRICIDAD
#
#----------------------------------------------------------
# Ajuste
electricidadEts <- ets(electricidad,
                       damped = FALSE)

summary(electricidadEts) 

# Prediccion
TT <- nrow(electricidadEts$states)

electricidadEts$states[TT,]

electricidadEts$states[TT, 1] + electricidadEts$states[TT, 8:2]

electricidadEtsPre <- forecast(electricidadEts, 
                               h = 28, 
                               level = 95)

electricidadEtsPre

autoplot(electricidadEtsPre,
         xlab = "",
         ylab = "GWh",
         main = "")

# Valores atipicos
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

atipicos <- tsoutliers(error)
fechas[atipicos$index]
#----------------------------------------------------------
#
# OTRAS ALTERNATIVAS: NACIMIENTOS
#
#----------------------------------------------------------
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
