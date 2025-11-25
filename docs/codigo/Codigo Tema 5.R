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
library(lmtest)
library(seasonal)
library(tseries)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Importamos series
#----------------------------------------------------------
# Nacimientos
nacimientos <- read.csv2("./series/nacimientos.csv", 
                         header = TRUE)

nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  freq = 12)

nacimientos <- window(nacimientos, start = 2000)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "")

# Exportaciones
exportaciones <- read.csv("./series/Exportaciones.csv", 
                          header = TRUE)

exportaciones <- ts(exportaciones,
                    start = c(1999, 1),
                    freq = 12)

autoplot(exportaciones,
         xlab = "",
         ylab = "Millones de €",
         main = "")

# Consumo electrico
electricidad <- read.csv("./series/Consumo electrico.csv", 
                         header = TRUE)

electricidad <- ts(electricidad[, 1],
                   start = c(1, 1),
                   frequency = 7)

autoplot(electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")

# Temperatura
temperatura <- read.csv("./series/Temperatura.csv")
temperatura <- temperatura[, 1]
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Nacimientos
#----------------------------------------------------------
# Estacionariedad y ergodicidad
ggAcf(log(nacimientos), lag = 48, ylim = c(-1, 1))
ggAcf(diff(log(nacimientos)), lag = 48, ylim = c(-1, 1))
ggAcf(diff(log(nacimientos), lag = 12), lag = 48, ylim = c(-1, 1))
ggAcf(diff(diff(log(nacimientos), lag = 12)), lag = 48, ylim = c(-1, 1))

ndiffs(log(nacimientos))
ndiffs(log(nacimientos))

# Identificación
ggtsdisplay(diff(diff(log(nacimientos), lag = 12)), lag = 48)

monthdays(nacimientos)
easter(nacimientos)

DiasMes <- monthdays(nacimientos)
SemanaSanta <- easter(nacimientos)

fechas <- format(seq(as.Date("2000-1-1"), as.Date("2025-06-1"), "month"), "%Y-%m")

d0111 <- 1*(fechas == "2011-01")
d1220 <- 1*(fechas == "2020-12")
d0221 <- 1*(fechas == "2021-02")
d0321 <- 1*(fechas == "2021-03")

auto.arima(nacimientos, 
           d = 1, 
           D = 1, 
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta, 
                        d0111, d1220, d0221, d0321))

#summary(seas(nacimientos))
summary(seas(nacimientos, transform.function = "log"))

# Estimación e intervencion
d1210 <- 1*(fechas == "2010-12")
d1120 <- 1*(fechas == "2020-11")
d0121 <- 1*(fechas == "2021-01")

nac.ar1 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1210,  d0111, d1120, d1220, 
                              d0121, d0221, d0321)) 
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
  scale_x_continuous(breaks= seq(2000, 2026, 2)) 

fechas[abs(error) > 3 * sderror]

nac.ar2 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d1210,  d0111, d1120, 
                              d1220, d0121, d0221))
nac.ar2

# Simplificacion intervencion: compensacion
d12100111 <- d1210 - d0111

# Simplificacion intervencion: Covid-19
d11200221 <- d1120 + d0221
d12200121 <- d1220 + d0121

nac.ar3 <- Arima(nacimientos, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasMes, SemanaSanta, 
                              d12100111, d11200221, d12200121))

nac.ar3

# Validacion: Significatividad
coeftest(nac.ar3)

# Validacion: Error de ajuste
accuracy(nac.ar3)

# Validación: error de prediccion
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
  labs(x = "Horizonte temporal de predicción", y = "", title = "") +
  scale_x_continuous(breaks= 1:12)

# Validacion: Hipotesis sobre el residuo
error <- residuals(nac.ar3)

Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

ggAcf(error, lag = 36, ylim = c(-0.3, 0.3), main = "")

jarque.bera.test(error) 

# Predicción
tmp <- ts(rep(0, 54), start = c(2025, 7), freq = 12)
pdm <- monthdays(tmp)
pss <- easter(tmp)

pnac.ar3 <- forecast(nac.ar3, 
                     h = 54,
                     xreg = cbind(pdm, pss, 
                                  rep(0,54), rep(0,54), rep(0,54)), 
                     level = 95)

pnac.ar3

autoplot(pnac.ar3, 
         ylab = "Nacimientos",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2030, 2))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Exportaciones
#----------------------------------------------------------
# Estacionariedad y ergodicidad
ggAcf(log(exportaciones), lag = 48, ylim = c(-1, 1))
ggAcf(diff(log(exportaciones)), lag = 48, ylim = c(-1, 1))
ggAcf(diff(log(exportaciones), lag = 12), lag = 48, ylim = c(-1, 1))
ggAcf(diff(diff(log(exportaciones), lag = 12)), lag = 48, ylim = c(-1, 1))

ndiffs(log(exportaciones))
nsdiffs(log(exportaciones))

# Identificación
auto.arima(exportaciones,
           d = 1,
           D = 1,
           lambda = 0,
           xreg = cbind(monthdays(exportaciones), easter(exportaciones)))

summary(seas(exportaciones))

# Estimación e intervencion
DiasLaborables <- bizdays(exportaciones, FinCenter = "London")
SemanaSanta <- easter(exportaciones)

fechas <- format(seq(as.Date("1999-01-01"), as.Date("2025-07-01"), "month"), "%Y-%m")
d0320 <- 1*(fechas == "2020-03")
d0420 <- 1*(fechas == "2020-04")
d0520 <- 1*(fechas == "2020-05")

l1208 <- 1*(fechas > "2008-11")

exp.ar1 <- Arima(exportaciones, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 lambda = 0,
                 xreg = cbind(DiasLaborables, SemanaSanta, 
                              l1208, d0320, d0420, d0520))
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
  scale_x_continuous(breaks= seq(1998, 2025, 2)) 

fechas[abs(error) > 3 * sderror]

# Validacion: significatividad
coeftest(exp.ar1)

# Validacion: Error de ajuste
accuracy(exp.ar1)

# Validacion: Error de predicciones
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
  labs(x = "Horizonte temporal de predicción", y = "", title = "") +
  scale_x_continuous(breaks= 1:12)

# Validacion: Hipotesis sobre el residuo
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")

jarque.bera.test(error) 

# Predicción
tmp <- ts(rep(0, 41), start = c(2025, 8), freq = 12)
pdl <- bizdays(tmp, FinCenter = "London")
pss <- easter(tmp)

pexp.ar1 <- forecast(exp.ar1, 
                     h = 41,
                     xreg = cbind(pdl, pss, 
                                  rep(1,41), 
                                  rep(0,41), rep(0,41), rep(0,41)), 
                     level = 95)

pexp.ar1

autoplot(pexp.ar1, 
         xlab = "",
         ylab = "Millones de euros",
         main = "",
         PI = FALSE) +
  scale_x_continuous(breaks= seq(1998, 2029, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Consumo electrico
#----------------------------------------------------------
# Transformación
ggAcf(electricidad, lag = 42, ylim = c(-1, 1))
ggAcf(diff(electricidad), lag = 42, ylim = c(-1, 1))
ggAcf(diff(electricidad, lag = 7), lag = 42, ylim = c(-1, 1))
ggAcf(diff(diff(electricidad, lag = 7)), lag = 42, ylim = c(-1, 1))

ndiffs(electricidad)
nsdiffs(electricidad) 

# Intervencion: festivos
fechas <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), "day")

fiestas <- as.Date(c("2024-01-01", "2024-01-06", "2024-03-29", "2024-03-31",
                     "2024-04-01", "2024-05-01", "2024-08-15", "2024-10-12",
                     "2024-11-01", "2024-12-06", "2024-12-08", "2024-12-25"))

festivosEntreSemana <- (fechas %in% fiestas) * (cycle(electricidad) < 6)

festivosFinSemana <- (fechas %in% fiestas) * (cycle(electricidad) > 5)

# Intervencion: temperatura
ggplot() +
  geom_point(aes(x = temperatura, y = electricidad), size = 2) +
  xlab("Temperatura (ºC)") + 
  ylab("Demanda eléctrica (GWh)")

diferenciaTemperatura <- abs(temperatura - mean(temperatura))

# Identificacion
auto.arima(electricidad,
           d = 1,
           D = 1,
           xreg = cbind(festivosEntreSemana, festivosFinSemana, 
                        diferenciaTemperatura))

# Estimacion e intervencion
ele.ar1 <- Arima(electricidad, 
                 order = c(0, 1, 1),
                 seasonal = c(0, 1, 1),
                 xreg = cbind(festivosEntreSemana, diferenciaTemperatura))

ele.ar1

error <- residuals(ele.ar1)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, 3)*sderror, 
             colour = c("red",  "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1, 54, 4)) 

fechas[abs(error) > 3 * sderror]

# Validacion: ignificatividad
coeftest(ele.ar1)

# Validacion: Error de ajuste
accuracy(ele.ar1)

# Velidación: error de prediccion
k <- 140               
h <- 7               
T <- length(electricidad)   
s <- T - k - h               

mapeArima <- matrix(NA, s + 1, h)

X <- data.frame(cbind(festivosEntreSemana, diferenciaTemperatura))

for (i in 0:s) {
  train.set <- subset(electricidad, start = i + 1, end = i + k)
  test.set <-  subset(electricidad, start = i + k + 1, end = i + k + h) 
  
  X.train <- as.matrix(X[(i + 1):(i + k),])
  X.test <- as.matrix(X[(i + k + 1):(i + k + h),])
  
  fit <- try(Arima(train.set, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   xreg = X.train))
  
  if(!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
errorArima

# Validacion: Hipotesis residuo
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 14,type = "Ljung-Box")

Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 14, type = "Ljung-Box")

ggAcf(error, lag = 35, main = "")

jarque.bera.test(error) 



# Prediccion
pfestivos <- c(1, 0, 0, 0, 0, 1, 0)
ptemperatura <- c(4.6, 5.0, 3.8, 3.5, 7.8, 8.8, 6.3)
pdiferenciaTemperatura <- abs(ptemperatura - mean(temperatura))

pele.ar1 <- forecast(ele.ar1, 
                     h = 7,
                     xreg = cbind(pfestivos, pdiferenciaTemperatura), 
                     level = 95)

pele.ar1

autoplot(pele.ar1, 
         xlab = "",
         ylab = "GWh",
         main = "") +
  xlim(46, 55)
