#----------------------------------------------------------
# CODIGO EJEMPLO 5: ARIMA CON ESTACIONALIDAD
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
library(timeDate)
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
# Asignamos NA al periodo de Covid-19
#----------------------------------------------------------
fechas <- format(seq(as.Date("2000-01-01"), as.Date("2023-12-31"), by = "month"), "%Y-%m")

covid <- format(seq(as.Date("2020-03-01"), as.Date("2022-02-28"), by = "month"), "%Y-%m")

covid_filtro <- fechas %in% covid

# Creamos una nueva serie...
Pernoctacionesp <- Pernoctaciones

#...y durante la Covid asignamos NA
Pernoctacionesp[covid_filtro] <- NA

autoplot(Pernoctacionesp,
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
ggAcf(log(Pernoctacionesp), lag = 48, ylim = c(-1 ,1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(Pernoctacionesp)), lag = 48,  ylim = c(-1 ,1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(log(Pernoctacionesp), lag = 12), lag = 48,  ylim = c(-1 ,1),
      xlab = "", ylab = "", main = "")

ggAcf(diff(diff(log(Pernoctacionesp), lag=12)), lag = 48, ylim = c(-1 ,1),
      xlab = "", ylab = "", main = "")

ndiffs(log(Pernoctacionesp))
nsdiffs(log(Pernoctacionesp))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Identificacion
#----------------------------------------------------------
# Auto.arima
DiasMes <- monthdays(Pernoctacionesp)
SemanaSanta <- easter(Pernoctacionesp)

auto.arima(Pernoctacionesp, 
           d = 1, 
           D = 1,
           lambda = 0,
           xreg = cbind(DiasMes, SemanaSanta))

# Seas
modeloSeas <- seas(Pernoctacionesp, 
                   transform.function = "log", 
                   na.action = na.x13)

summary(modeloSeas)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Estimacion (Ajuste + Intervencion)
#----------------------------------------------------------
# Semana Santa
LunSanto <- Easter(2000:2027, shift = -6)
MarSanto <- Easter(2000:2027, shift = -5)
MieSanto <- Easter(2000:2027, shift = -4)
JueSanto <- Easter(2000:2027, shift = -3)
VieSanto <- Easter(2000:2027, shift = -2)
SabSanto <- Easter(2000:2027, shift = -1)
DomSanto <- Easter(2000:2027, shift =  0)
LunPascu <- Easter(2000:2027, shift =  1)
MarPascu <- Easter(2000:2027, shift =  2)
MiePascu <- Easter(2000:2027, shift =  3)
JuePascu <- Easter(2000:2027, shift =  4)
ViePascu <- Easter(2000:2027, shift =  5)


SemanaSanta <- c(LunSanto, MarSanto, MieSanto, JueSanto, VieSanto,
                 SabSanto, DomSanto, LunPascu ,MarPascu, MiePascu, 
                 JuePascu, ViePascu)
fechaDiaria <- timeSequence(from = "2000-01-01", to = "2027-12-31")
biz <- fechaDiaria[isBizday(fechaDiaria, holidays = SemanaSanta, wday = 0:6)]
bizdays <- format(biz, format = "%Y-%m")

SemanaSanta <- table(bizdays)
SemanaSanta <- ts(SemanaSanta, start = 2000, frequency = 12)
window(SemanaSanta, end = c(2004, 12))
SemanaSanta <- (monthdays(SemanaSanta) - SemanaSanta)/12 #Nuestra SS tiene 12 dias
window(round(SemanaSanta, 2), end = c(2004, 12))

pSemanaSanta <- subset(SemanaSanta, start = length(SemanaSanta) - 47)
SemanaSanta <- subset(SemanaSanta, end = length(SemanaSanta) - 48)

d0305 <- 1 * (trunc(time(Pernoctaciones)) == 2005 & cycle(Pernoctaciones) == 3)
d0511 <- 1 * (trunc(time(Pernoctaciones)) == 2011 & cycle(Pernoctaciones) == 5)

Arima1 <- Arima(Pernoctacionesp, 
                order = c(0, 1, 1),  
                seasonal = c(0, 1, 1),
                lambda = 0,
                xreg = cbind(DiasMes, SemanaSanta,
                             d0305, d0511))
Arima1

#Intervencion
error <- residuals(Arima1)
sderror <- sd(error, na.rm = TRUE)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, -2, 2, 3)*sderror, 
             colour = c("red", "green", "green", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(2000, 2024, 2)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Validacion
#----------------------------------------------------------
# Coeficientes significativos
coeftest(Arima1)

# Error de ajuste
accuracy(Arima1)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Prediccion
#----------------------------------------------------------
tmp <- ts(rep(0, 48), start = 2024, frequency = 12)
pDiasMes <- monthdays(tmp)

pArima1 <- forecast(Arima1, 
                    h = 48,
                    xreg = cbind(pDiasMes, pSemanaSanta, 
                                 rep(0, 48), rep(0, 48)), 
                    level = 95)
autoplot(pArima1, 
         xlab = "",
         ylab = "Pernoctaciones",
         main = "") +
  scale_x_continuous(breaks= seq(2000, 2028, 4)) 
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Comparacion entre modelos
#----------------------------------------------------------
# Error de ajuste
modelSen <- snaive(Pernoctacionesp)
modelAli <- ets(Pernoctacionesp, model = "AAA", damped = TRUE)
modelAliL <- ets(Pernoctacionesp, model = "AAA", damped = TRUE, lambda = 0)
Arima1L <- Arima(Pernoctacionesp, 
                 order = c(0, 1, 1),  
                 seasonal = c(0, 1, 1),
                 xreg = cbind(DiasMes, SemanaSanta, d0305, d0511))
accuracy(modelSen)
accuracy(modelAli)
accuracy(modelAliL)
accuracy(Arima1L)
accuracy(Arima1)

# Errores con origen de predicción movil
k <- 120                   
h <- 12                    
T <- length(Pernoctacionesp)     
s <- T - k - h               

mapeIngenuo <- matrix(NA, s + 1, h)
mapeAlisado <- matrix(NA, s + 1, h)
mapeAlisadoLog <- matrix(NA, s + 1, h)
mapeArima <- matrix(NA, s + 1, h)
mapeArimaLog <- matrix(NA, s + 1, h)


X <- data.frame(cbind(DiasMes, SemanaSanta))

for (i in 0:s) {
  train.set <- subset(Pernoctaciones, start = i + 1, end = i + k)
  test.set <-  subset(Pernoctaciones, start = i + k + 1, end = i + k + h) 
  
  train.setp <- subset(Pernoctacionesp, start = i + 1, end = i + k)
  test.setp <-  subset(Pernoctacionesp, start = i + k + 1, end = i + k + h) 
  
  X.train <- as.matrix(X[(i + 1):(i + k),])
  X.test <- as.matrix(X[(i + k + 1):(i + k + h),])
  
  #Ingenuo
  fit <- snaive(train.set, h = h)
  mapeIngenuo[i + 1,] <- 100*abs(test.set - fit$mean)/test.set
  
  #Alisado sin log
  fit <- ets(train.set, model = "AAA", damped = TRUE)
  fcast <- forecast(fit, h = h) 
  mapeAlisado[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  #Alisado con log
  fit <- ets(train.set, model = "AAA", damped = TRUE, lambda = 0)
  fcast <- forecast(fit, h = h) 
  mapeAlisadoLog[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  #ARIMA sin log
  fit <- try(Arima(train.setp, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   xreg = X.train), 
             silent = TRUE)
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test)
    mapeArima[i + 1,] <- 100*abs(test.setp - fcast$mean)/test.setp
  }
  
  #ARIMA con log
  fit <- try(Arima(train.setp, 
                   order = c(0, 1, 1),
                   seasonal = c(0, 1, 1),
                   lambda = 0,
                   xreg = X.train),
             silent = TRUE)
  
  if (!is.element("try-error", class(fit))) {
    fcast <- forecast(fit, h = h, xreg = X.test) 
    mapeArimaLog[i + 1,] <- 100*abs(test.setp - fcast$mean)/test.setp
  }
  
}

mapeIngenuo <- apply(mapeIngenuo, MARGIN = 2, FUN = median)
mapeAlisado <- apply(mapeAlisado, MARGIN = 2, FUN = median)
mapeAlisadoLog <- apply(mapeAlisadoLog, MARGIN = 2, FUN = median)
mapeArima <- apply(mapeArima, MARGIN = 2, FUN = median, na.rm = TRUE, )
mapeArimaLog <- apply(mapeArimaLog, MARGIN = 2, FUN = median, na.rm = TRUE)

ggplot() +
  geom_line(aes(x = 1:12, y = mapeIngenuo, colour = "Ingenuo")) +
  geom_line(aes(x = 1:12, y = mapeAlisado, colour = "Alisado")) + 
  geom_line(aes(x = 1:12, y = mapeAlisadoLog, colour = "Alisado (log)")) +
  geom_line(aes(x = 1:12, y = mapeArima, colour = "Arima")) +
  geom_line(aes(x = 1:12, y = mapeArimaLog, colour = "Arima (log)")) +
  ggtitle("") +
  xlab("") +
  ylab("MAPE") +
  scale_x_continuous(breaks= 1:12) +
  scale_color_discrete(name = "Modelos")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Efecto de la pandemia de la Covid-19 sobre las pernoctaciones
#----------------------------------------------------------
# Trabajamos con la serie hasta dis de 2019
Pernoctacionesb <- window(Pernoctaciones, end = c(2019, 12))
SemanaSantab <- window(SemanaSanta, end = c(2019, 12))
DiasMesb <- monthdays(Pernoctacionesb)
d0305b <- 1 * (trunc(time(Pernoctacionesb)) == 2005 & cycle(Pernoctacionesb) == 3)
d0511b <- 1 * (trunc(time(Pernoctacionesb)) == 2011 & cycle(Pernoctacionesb) == 5)

# Estimamos 
Arimab <- Arima(Pernoctacionesb, 
                order = c(0, 1, 1),  
                seasonal = c(0, 1, 1),
                lambda = 0,
                xreg = cbind(DiasMesb, SemanaSantab,
                             d0305b, d0511b))

# Predecimos
tmp <- ts(rep(0, 48), start = 2020, frequency = 12)
pDiasMesb <- monthdays(tmp)
pSemanaSantab <- window(SemanaSanta, start = c(2020, 1), end = c(2023, 12))

pArimab <- forecast(Arimab, 
                    h = 48,
                    xreg = cbind(pDiasMesb, pSemanaSantab, 
                                 rep(0, 48), rep(0, 48)))

# Comparamos la prediccion con las reales
autoplot(Pernoctaciones, 
         series = "Pernoctaciones reales",
         xlab = "",
         ylab = "",
         main = "") +
  xlim(2019, 2024) +
  autolayer(pArimab,
            PI = FALSE,
            series="Pernoctaciones estimadas")   

# Lo vemos numericamente 
aggregate(Pernoctaciones - pArimab$mean, FUN = sum)
