#----------------------------------------------------------
# CODIGO TEMA 5
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
# Libros
# Alimentos per capita
alimentospc <- read.csv2("./series/Alimentacionpc.csv", 
                         header = TRUE)

alimentospc <- ts(alimentospc, 
                  start = 1990, 
                  freq = 1)

autoplot(alimentospc, 
         main = "", 
         xlab = "Año", 
         ylab = "")

# Defunciones
defunciones <- read.csv2("./series/Defunciones.csv",
                         header = TRUE)

defunciones <- ts(defunciones, 
                  start = 1, 
                  freq = 1)

autoplot(defunciones, 
         main = "", 
         xlab = "Día", 
         ylab = "")
#----------------------------------------------------------
#
# CONJUNTO DE ENTRENAMIENTO/PRUEBA: ALIMENTOS
#
#----------------------------------------------------------
# Ingenuo I
#----------------------------------------------------------

# Definimos las observaciones intra- y extramuestrales
AlimentospcIntra <- subset(alimentospc, end = length(alimentospc) - 7)
AlimentospcExtra <- subset(alimentospc, start = length(alimentospc) - 6)

# Estimamos el modelo con todos los datos menos los 7 ultimos y
# predecimos los 7 años que hemos quitado de la serie 
AlimentospcExtraPre <- naive(AlimentospcIntra,  h = 7)

# Vemos la calidad del ajuste. Primero la predicción y luego los datos reales
accuracy(AlimentospcExtraPre, AlimentospcExtra)
#----------------------------------------------------------
# Arima
#----------------------------------------------------------

# Definimos las observaciones intra- y extramuestrales
AlimentospcIntra <- subset(alimentospc, end = length(alimentospc) - 7)
AlimentospcExtra <- subset(alimentospc, start = length(alimentospc) - 6)

# Estimamos el modelo con todos los datos menos los 7 ultimos y
# predecimos los 7 años que hemos quitado de la serie 
ariAlimentospcIntra <- Arima(AlimentospcIntra, 
                             order = c(1, 0, 0),
                             include.constant = TRUE)

ariAlimentospcIntraPre <- forecast(ariAlimentospcIntra,  h = 7)

# Vemos la calidad del ajuste. Primero la predicción y luego los datos reales
accuracy(ariAlimentospcIntraPre, AlimentospcExtra)
#----------------------------------------------------------
#
# ORIGEN DE PREDICCION MOVIL: ALIMENTOS
#
#----------------------------------------------------------
# Ingenuo I
#----------------------------------------------------------
k <- 15                   # Minimo numero de datos para estimar
h <- 5                    # Horizonte de las prediciciones
TT <- length(alimentospc) # Longitud serie
s <- TT - k - h           # Total de estimaciones

mapeIng <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(alimentospc, start = i + 1, end = i + k)
  test.set <-  subset(alimentospc, start = i + k + 1, end = i + k + h)
  
  fcast <- naive(train.set, h = h)
  mapeIng[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeIngMedia <- colMeans(mapeIng)
round(mapeIngMedia, 2)

mapeIngMediana <- apply(mapeIng, MARGIN = 2, FUN = median)
round(mapeIngMediana, 2)
#----------------------------------------------------------
# Arima
#----------------------------------------------------------
k <- 15                   # Minimo numero de datos para estimar
h <- 5                    # Horizonte de las prediciciones
TT <- length(alimentospc) # Longitud serie
s <- TT - k - h           # Total de estimaciones

mapeAri <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(alimentospc, start = i + 1, end = i + k)
  test.set <-  subset(alimentospc, start = i + k + 1, end = i + k + h)
  
  fit <- Arima(train.set, order = c(1, 0, 0), include.constant = TRUE)
  fcast<- forecast(fit, h = h)
  mapeAri[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
}

mapeAriMedia <- colMeans(mapeAri)
round(mapeAriMedia, 2)

mapeAriMediana <- apply(mapeAri, MARGIN = 2, FUN = median)
round(mapeAriMediana, 2)
#----------------------------------------------------------
# Comparacion entre modelos
#----------------------------------------------------------
ggplot() +
  geom_line(aes(x = 1:5, y = mapeIngMediana, colour = "blue")) + 
  geom_line(aes(x = 1:5, y = mapeAriMediana, colour = "red")) + 
  ggtitle("") +
  xlab("") +
  ylab("%") +
  scale_color_discrete(name = "Método", 
                       labels = c("Ingenuo I", "Arima")) +
  theme(legend.position=c(0.2,0.8))
#----------------------------------------------------------
#
# DEFUNCIONES (Alisado y Arima)
#
#----------------------------------------------------------
# Conjunto de entrenamiento/prueba
#----------------------------------------------------------
DefuncionesIntra <- subset(defunciones, end = length(defunciones) - 14)
DefuncionesExtra <- subset(defunciones, start = length(defunciones) - 13)

# Estimamos el modelo de Alisado MNN y predecimos
fit <- ets(DefuncionesIntra, 
           model = "MNN")

DefuncionesExtraPreAli <- forecast(fit, h = 14)

# Estimamos el modelo de Arima(2, 0 ,0) y predecimos
fit <- Arima(DefuncionesIntra, 
             order = c(2, 0 ,0), 
             include.constant = TRUE)

DefuncionesExtraPreAri <- forecast(fit, h = 14)

accuracy(DefuncionesExtraPreAli, DefuncionesExtra)
accuracy(DefuncionesExtraPreAri, DefuncionesExtra)

#----------------------------------------------------------
# Origen de prediccion movil
#----------------------------------------------------------
k <- 140                  # Minimo numero de datos para estimar
h <- 14                   # Horizonte de las prediciciones
TT <- length(defunciones) # Longitud serie
s <- TT - k - h           # Total de estimaciones

rmseAli <- matrix(NA, s + 1, h)
rmseAri <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(defunciones, start = i + 1, end = i + k)
  test.set <-  subset(defunciones, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MNN")
  fcast <- forecast(fit, h = h)
  rmseAli[i + 1,] <- (test.set - fcast$mean)^2
  
  fit <- Arima(train.set, order = c(2, 0, 0), include.constant = TRUE)
  fcast <- forecast(fit, h = h)
  rmseAri[i + 1,] <- (test.set - fcast$mean)^2
}

rmseAli <- sqrt(colMeans(rmseAli))
round(rmseAli, 2)

rmseAri <- sqrt(colMeans(rmseAri))
round(rmseAri, 2)
#----------------------------------------------------------
# Comparacion entre modelos
#----------------------------------------------------------
ggplot() +
  geom_line(aes(x = 1:14, y = rmseAli, colour = "blue")) + 
  geom_line(aes(x = 1:14, y = rmseAri, colour = "red")) + 
  ggtitle("") +
  xlab("") +
  ylab("Defunciones") +
  scale_color_discrete(name = "Método", 
                       labels = c("Alisado", "Arima")) +
  theme(legend.position=c(0.2,0.8))
