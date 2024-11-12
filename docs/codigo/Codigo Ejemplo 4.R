#----------------------------------------------------------
# CODIGO EJEMPLO 4: ARIMA SIN ESTACIONALIDAD
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
# Importamos Pernoctaciones
#----------------------------------------------------------
Pernoctaciones <- read.csv2("./series/Pernoctaciones.csv", 
                            header = TRUE)

Pernoctaciones <- ts(Pernoctaciones[, 2] / 1000000, 
                     start = 2000, 
                     frequency = 12)

Pernoctaciones <- aggregate(Pernoctaciones, FUN = sum)

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
# Transformacion
#----------------------------------------------------------
autoplot(Pernoctaciones, xlab = "", ylab = "", main = "")
autoplot(diff(Pernoctaciones), xlab = "", ylab = "", main = "")

ndiffs(Pernoctaciones)
ndiffs(window(Pernoctaciones, end = 2019))

#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Identificación
d2020 <- 1 * (time(Pernoctaciones) == 2020)
d2021 <- 1 * (time(Pernoctaciones) == 2021)

auto.arima(Pernoctaciones, 
           d = 1,
           xreg = cbind(d2020, d2021))

# Estimacion
arima010 <- Arima(Pernoctaciones, 
                  order = c(0, 1, 0),
                  include.constant = FALSE,
                  xreg = cbind(d2020, d2021))

arima010

# Intervencion
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
  scale_x_continuous(breaks= seq(2000, 2024, 2))

# Valdacion
accuracy(arima010)

coeftest(arima010)

# Origen de prevision movil
k <- 10                  
h <- 4                    
T <- length(Pernoctaciones)     
s <- T - k - h    

mapeArima010 <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  fit <- Arima(train.set, 
               include.constant = FALSE,
               order = c(0, 1, 0))
  
  fcast <- forecast(fit, h = h)
  
  mapeArima010[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
}

mapeArima010 <- apply(mapeArima010, MARGIN = 2, FUN = median)
mapeArima010

# Predicción
parima010 <- forecast(arima010, 
                      h = 4, 
                      level = 95,
                      xreg = cbind(rep(0, 4), rep(0, 4)))

parima010

autoplot(parima010, 
         ylab = "Noches (millones)",
         xlab = "",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2028, 4)) 
