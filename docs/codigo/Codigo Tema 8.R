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
library(lmtest)
library(seasonal)
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
                  freq = 12)

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

# Exportaciones
exportaciones <- read.csv2("./series/Exportaciones.csv", 
                           header = TRUE)

exportaciones <- ts(exportaciones,
                    start = c(1999, 1),
                    freq = 12)

autoplot(exportaciones,
         xlab = "",
         ylab = "Millones de €",
         main = "")
#----------------------------------------------------------
#
# DIFERENCIACION
#
#----------------------------------------------------------
autoplot(nacimientos)
autoplot(diff(nacimientos))
autoplot(diff(nacimientos, lag = 12))
autoplot(diff(diff(nacimientos, lag = 12)))

nsdiffs(nacimientos)
#----------------------------------------------------------
#
# FUNCION DE AUTOCORRELACION (FAC)
#
#----------------------------------------------------------
ggAcf(nacimientos, lag = 48)
ggAcf(log(nacimientos), lag = 48)
ggAcf(diff(nacimientos), lag = 48)
ggAcf(diff(log(nacimientos)), lag = 48)
ggAcf(diff(nacimientos, lag = 12), lag = 48)
ggAcf(diff(log(nacimientos), lag = 12), lag = 48)
ggAcf(diff(diff(nacimientos, lag = 12)), lag = 48)
ggAcf(diff(diff(log(nacimientos), lag = 12)), lag = 48)
#----------------------------------------------------------
#
# NACIMIEMTOS
#
#----------------------------------------------------------
# Serie desde 2000
nacimientos <- window(nacimientos, start = 2000)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Estacionariedad y ergodicidad
ggAcf(log(nacimientos), lag = 48, ylim = c(-1,1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(nacimientos)), lag = 48, ylim = c(-1,1), 
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(nacimientos), lag = 12), lag = 48, ylim = c(-1,1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(diff(log(nacimientos), lag = 12)), lag = 48, ylim = c(-1,1),
      xlab = "", ylab = "", main = "")

ndiffs(nacimientos)

nsdiffs(nacimientos)

# Identificación
monthdays(nacimientos)
easter(nacimientos)

DiasMes <- monthdays(nacimientos)
SemanaSanta <- easter(nacimientos)

d0111 <- 1*(cycle(nacimientos) == 1  & trunc(time(nacimientos)) == 2011)
d1220 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2020)
d0221 <- 1*(cycle(nacimientos) == 2  & trunc(time(nacimientos)) == 2021)
d0321 <- 1*(cycle(nacimientos) == 3  & trunc(time(nacimientos)) == 2021)

auto.arima(nacimientos, 
           d = 1, 
           D = 1, 
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta, 
                        d0111, d1220, d0221, d0321))

summary(seas(nacimientos))
summary(seas(nacimientos, transform.function = "log"))

# Estimación + atipicos (1 de 2)
d1210 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2010)
d1120 <- 1*(cycle(nacimientos) == 11 & trunc(time(nacimientos)) == 2020)
d0121 <- 1*(cycle(nacimientos) ==  1 & trunc(time(nacimientos)) == 2021)

nac.ar1 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1210,  d0111, d1120, d1220, d0121, d0221, d0321)) 
nac.ar1

error <- residuals(nac.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 

fechas <- format(seq(as.Date("2000-1-1"), as.Date("2023-12-1"), "month"), "%Y-%m")
fechas[abs(error) > 2.5 * sderror]

# Estimación + atipicos (1 de 2)
d1206 <- 1*(cycle(nacimientos) == 12 & trunc(time(nacimientos)) == 2006)
d0416 <- 1*(cycle(nacimientos) ==  4 & trunc(time(nacimientos)) == 2016)
d1122 <- 1*(cycle(nacimientos) == 11 & trunc(time(nacimientos)) == 2022)

nac.ar2 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d1210, d0111, d0416,
                              d1120, d1220, d0121, d0221, d1122))
nac.ar2

error <- residuals(nac.ar2)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2022, 2)) 

fechas[abs(error) > 2.5 * sderror]

# Compensacion
d12100111 <- d1210 - d0111

# Variables de intervencion asociadas a la Covid-19
d11200221 <- d1120 + d0221
d12200121 <- d1220 + d0121

nac.ar3 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1206, d12100111, d0416,
                              d11200221, d12200121, d1122))

nac.ar3

# Validacion
coeftest(nac.ar3)

accuracy(nac.ar3)

# Origen de prediccion movil
k <- 180                   
h <- 12                    
T <- length(nacimientos)   
s<-T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasMes, SemanaSanta))

for (i in 0:s) {
  train.set <- subset(nacimientos, start = i + 1, end = i + k)
  test.set <-  subset(nacimientos, start = i + k + 1, end = i + k + h) 
  
  X.train <- as.matrix(X[(i + 1):(i + k),])
  X.test <- as.matrix(X[(i + k + 1):(i + k + h),])
  
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- apply(mapeArima, MARGIN = 2, FUN = median, na.rm = TRUE)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

# Predicción
tmp <- ts(rep(0, 48), start = 2024, freq = 12)
pdm <- monthdays(tmp)
pss <- easter(tmp)
pnac.ar3 <- forecast(nac.ar3, 
                     h = 48,
                     xreg = cbind(pdm, pss, 
                                  rep(0,48), rep(0,48), rep(0,48),
                                  rep(0,48), rep(0,48), rep(0,48)), 
                     level = 95)

pnac.ar3

autoplot(pnac.ar3, 
         ylab = "Nacimientos",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2028, 2)) 
#----------------------------------------------------------
#
# EXPORTACIONES
#
#----------------------------------------------------------

# Estacionariedad y ergodicidad
ggAcf(log(exportaciones), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(exportaciones)), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(exportaciones), lag = 12), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(diff(log(exportaciones), lag = 12)), lag = 48, ylim = c(-1, 1),
      xlab = "", ylab = "", main = "")

ndiffs(log(exportaciones))
nsdiffs(log(exportaciones))

# Identificación
auto.arima(exportaciones,
           d = 1,
           D = 1,
           lambda = 0,
           xreg = cbind(monthdays(exportaciones), easter(exportaciones)))

summary(seas(exportaciones))

# Estimación + atipicos
DiasLaborables <- bizdays(exportaciones, FinCenter = "London")
SemanaSanta <- easter(exportaciones)

d0320 <- 1*(cycle(exportaciones) ==  3 & trunc(time(exportaciones)) == 2020)
d0420 <- 1*(cycle(exportaciones) ==  4 & trunc(time(exportaciones)) == 2020)
d0520 <- 1*(cycle(exportaciones) ==  5 & trunc(time(exportaciones)) == 2020)

l1208 <- 1*(trunc(time(exportaciones)) > 2008) + 
  1*(cycle(exportaciones) >= 12 & trunc(time(exportaciones)) == 2008)

l0423 <- 1*(trunc(time(exportaciones)) > 2023) + 
  1*(cycle(exportaciones) >= 4 & trunc(time(exportaciones)) == 2023)

exp.ar1 <- Arima(exportaciones, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasLaborables, SemanaSanta, 
                              l1208, d0320, d0420, d0520, l0423))
exp.ar1

error <- residuals(exp.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1998, 2022, 2)) 

fechas <- format(seq(as.Date("1999-1-1"), as.Date("2023-12-1"), "month"), "%Y-%m")
fechas[abs(error) > 2.5 * sderror]

# Validacion
coeftest(exp.ar1)

accuracy(exp.ar1)

# Origen de prediccion movil
k <- 120                   
h <- 12                    
T <- length(exportaciones)   
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(DiasLaborables, SemanaSanta))

for (i in 0:s) {
  train.set <- subset(exportaciones, start = i + 1, end = i + k)
  test.set <-  subset(exportaciones, start = i + k + 1, end = i + k + h) 
  
  X.train <- as.matrix(X[(i + 1):(i + k),])
  X.test <- as.matrix(X[(i + k + 1):(i + k + h),])
  
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- apply(mapeArima, MARGIN = 2, FUN = median, na.rm = TRUE)
errorArima

ggplot() +
  geom_line(aes(x = 1:12, y = errorArima), colour = "Blue") +
  ggtitle("") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12)

# Predicción
tmp <- ts(rep(0, 48), start = 2024, freq = 12)
pdl <- bizdays(tmp, FinCenter = "London")
pss <- easter(tmp)
pexp.ar1 <- forecast(exp.ar1, 
                     h = 48,
                     xreg = cbind(pdl, pss, 
                                  rep(1,48), 
                                  rep(0,48), rep(0,48), rep(0,48),
                                  rep(1,48)), 
                     level = 95)

pexp.ar1

autoplot(pexp.ar1, 
         xlab = "",
         ylab = "Millones de euros",
         main = "",
         PI = FALSE) +
  scale_x_continuous(breaks= seq(1998, 2028, 2))
#----------------------------------------------------------
#
# ELECTRICIDAD
#
#----------------------------------------------------------
# 
#----------------------------------------------------------
# Transformación
ggAcf(electricidad, lag = 42, xlab = "", ylab = "", main = "")
ggAcf(diff(electricidad), lag = 42, xlab = "", ylab = "", main = "")
ggAcf(diff(electricidad, lag = 12), lag = 42, xlab = "", ylab = "", main = "")
ggAcf(diff(diff(electricidad, lag = 12)), lag = 42, xlab = "", ylab = "", main = "")

ndiffs(electricidad)
nsdiffs(electricidad)

# Efectos calendario
fechas <- seq(as.Date("2023-1-1"), as.Date("2023-12-31"), "day")

fiestas <- as.Date(c("2023-01-01", "2023-01-02", "2023-01-06", 
                     "2023-04-06", "2023-04-07", "2023-04-10", 
                     "2023-05-01", "2023-08-15", "2023-10-12", "2023-11-01", 
                     "2023-12-06", "2023-12-08", "2023-12-25"))

festivosEntreSemana <- (fechas %in% fiestas) * (cycle(electricidad) < 6)
festivosDomingo <- (fechas %in% fiestas) * (cycle(electricidad) == 7)

# Efecto temperatura
temperatura <- read.csv("./series/Temperatura.csv")
temperatura <- temperatura[, 1]

excesotemperatura <- abs(temperatura - mean(temperatura))

ggplot() +
  geom_point(aes(x = excesotemperatura, y = electricidad), size = 2) +
  xlab("Exceso de temperatura (ºC)") + 
  ylab("Demanda eléctrica (GWh)")

# Identificacion
auto.arima(electricidad,
           d = 1,
           D = 1,
           xreg = cbind(festivosEntreSemana, festivosDomingo, 
                        excesotemperatura))

festivos <- festivosEntreSemana + festivosDomingo

# Estimacion + atipicos
ele.ar1 <- Arima(electricidad, 
                 order = c(0, 1, 0),
                 seasonal = c(1, 1, 0),
                 xreg = cbind(festivos, excesotemperatura))
ele.ar1

error <- residuals(ele.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2,2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1998, 2022, 2)) 

fechas[abs(error) > 3 * sderror]

# Validacion
coeftest(ele.ar1)

accuracy(ele.ar1)

# Origen de prediccion movil
k <- 140               
h <- 7                   
T <- length(electricidad)   
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(festivos, excesotemperatura))

for (i in 0:s) {
  train.set <- subset(electricidad, start = i + 1, end = i + k)
  test.set <-  subset(electricidad, start = i + k + 1, end = i + k + h) 
  
  X.train <- as.matrix(X[(i + 1):(i + k),])
  X.test <- as.matrix(X[(i + k + 1):(i + k + h),])
  
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 0),
                   seasonal = c(1, 1, 0),
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
errorArima

# Prediccion
pfestivos <- c(1, 0, 0, 0, 0, 1, 0)
ptemperatura <- c(6.6, 4.0, 10.0, 9.2, 7.6, 5.4, 4.8)
pexcesotemperatura <- ptemperatura - mean(temperatura)

pele.ar1 <- forecast(ele.ar1, 
                     h = 7,
                     xreg = cbind(pfestivos, pexcesotemperatura), 
                     level = 95)
pele.ar1

autoplot(pele.ar1, 
         xlab = "",
         ylab = "GWh",
         main = "") +
  scale_x_continuous(breaks= seq(46, 56, 1)) 

