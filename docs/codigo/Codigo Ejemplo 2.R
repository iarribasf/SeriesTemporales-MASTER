#----------------------------------------------------------
# CODIGO EJEMPLO 2: METODOS SENCILLOS
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
# Importamos Pernoctaciones
#----------------------------------------------------------
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[, 2] / 1000000, 
                     start = 2000, 
                     frequency = 12)

autoplot(Pernoctaciones,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Metodo ingenuo para la serie anual
#----------------------------------------------------------
PernoctacionesAnual <- aggregate(Pernoctaciones, FUN = sum)

autoplot(PernoctacionesAnual,
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 

# Ajustes por metodos sencillos
naivePernoctaciones <- naive(PernoctacionesAnual, h = 4)
derivaPernoctaciones <- rwf(PernoctacionesAnual,  h = 4, drift = TRUE)

autoplot(PernoctacionesAnual, series = "Pernoctaciones",
         xlab = "",
         ylab = "Noches (millones)",
         main = "") +
  autolayer(naivePernoctaciones, series="Ingenuo", PI = FALSE) +
  autolayer(derivaPernoctaciones, series="Deriva", PI = FALSE) +
  scale_colour_discrete(limits=c("Pernoctaciones", "Ingenuo", "Deriva")) +
  labs(colour="Métodos") + 
  theme(legend.position=c(0.15,0.2))

# Error de ajuste
round(accuracy(naivePernoctaciones, test = 1:20), 2)
round(accuracy(derivaPernoctaciones, test = 1:20), 2)

# Error con origen de predicciones movil
k <- 10                  
h <- 4                 
TT <- length(PernoctacionesAnual) 
s <- TT - k - h          

mapeNaiveI <- matrix(NA, s + 1, h)
mapeDeriva <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(PernoctacionesAnual, start = i + 1, end = i + k)
  test.set <-  subset(PernoctacionesAnual, start = i + k + 1, end = i + k + h)
  
  fcast <- naive(train.set, h = h)
  mapeNaiveI[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fcast <- rwf(train.set, h = h,  drift = TRUE)
  mapeDeriva[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeNaiveI <- apply(mapeNaiveI, MARGIN = 2, FUN = median)
mapeDeriva <- apply(mapeDeriva, MARGIN = 2, FUN = median)

mapeNaiveI
mapeDeriva

# Predicciones
derivaPernoctaciones
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Metodo ingenuo con estacionalidad
#----------------------------------------------------------
# Ajuste
PernoctacionesPre <- snaive(Pernoctaciones, 
                            h = 48, 
                            level = 0.95)

PernoctacionesPre

# Error de ajuste
accuracy(PernoctacionesPre, test = 1:240)

# Prevision
autoplot(PernoctacionesPre,
         xlab = "",
         ylab = "Noches",
         main = "",
         PI = FALSE) +
  scale_x_continuous(breaks= seq(2000, 2028, 2)) 

# Error con origen de prediccion movil
k <- 120                 
h <- 12                  
TT <- length(Pernoctaciones)  
s <- TT - k - h          

mapeSnaive <- matrix(NA, s + 1, h)
for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h)
  
  fit <- snaive(train.set, h = h)
  mapeSnaive[i + 1,] <- 100*abs(test.set - fit$mean)/test.set
}

mapeSnaive <- apply(mapeSnaive, MARGIN = 2, FUN = median)
mapeSnaive

ggplot() +
  geom_line(aes(x = 1:12, y = mapeSnaive)) +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12)
