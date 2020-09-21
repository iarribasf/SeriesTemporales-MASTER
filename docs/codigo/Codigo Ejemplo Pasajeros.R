
library(gridExtra)
library(tseries)
library(aod)
library(forecast)
library(ggplot2); theme_set(theme_bw())
library(seasonal)
library(knitr)
library(timeDate)

meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

Pasajeros <- read.csv2("Pasajeros.csv", header = TRUE)
Pasajeros <- ts(Pasajeros/1000, start = 1996, freq = 12)


autoplot(Pasajeros, colour = "darkblue",
         xlab = "",
         ylab = "Millones de pasajeros",
         main = "Figura 1.1. Pasajeros en transporte urbano (datos mensuales)") +
  scale_x_continuous(breaks= seq(1996, 2019, 2)) 


PasajerosAnual <- aggregate(Pasajeros, FUN = sum)
autoplot(PasajerosAnual, colour = "darkblue",
         xlab = "",
         ylab = "Millones de pasajeros",
         main = "Figura 2.1. Pasajeros en transporte urbano (datos anuales)") +
  scale_x_continuous(breaks= seq(1996, 2019, 2))


AnoNuevo <- timeCalendar(d = 1, m = 1, y = 1996:2024)
Reyes <- timeCalendar(d = 6, m = 1, y = 1996:2024)
ViernesSanto <- Easter(1996:2024, shift = -2)
DomingoSanto <- Easter(1996:2024)
DiaTrabajo <- timeCalendar(d = 1, m = 5, y = 1996:2024)
Asuncion <- timeCalendar(d = 15, m = 8, y = 1996:2024)
Hispanidad <- timeCalendar(d = 12, m = 10, y = 1996:2024)
TodoSantos <- timeCalendar(d = 1, m = 11, y = 1996:2024)
Constitucion <- timeCalendar(d = 6, m = 12, y = 1996:2024)
Inmaculada <- timeCalendar(d = 8, m = 12, y = 1996:2024)
Navidad <- timeCalendar(d = 25, m = 12, y = 1996:2024)

FestivosNacionales <- c(AnoNuevo, Reyes, ViernesSanto, DomingoSanto, 
                        DiaTrabajo, Asuncion,  Hispanidad, TodoSantos, 
                        Constitucion, Inmaculada, Navidad)

fechaDiaria <- timeSequence(from = "1996-01-01", to = "2024-12-31")
biz <- fechaDiaria[isBizday(fechaDiaria, holidays = FestivosNacionales)]
bizdays <- format(biz, format = "%Y-%m")

DiasLaborables <- table(bizdays)
DiasLaborables <- ts(DiasLaborables, start = 1996, frequency = 12)

subset(DiasLaborables, start = 229) #Mostramos solo los 10 últimos años

pDiasLaborables <- subset(DiasLaborables, start = length(DiasLaborables) - 59)
DiasLaborables <- subset(DiasLaborables, end = length(DiasLaborables) - 60)


PasajerosDL <- Pasajeros/DiasLaborables

g1 <- ggsubseriesplot(Pasajeros) +
  ylab("Millones de pasajeros") +
  xlab("") +
  ggtitle("Figura 2.2a. Gráfico estacional para Pasajeros")

g2 <- ggsubseriesplot(PasajerosDL) +
  ylab("Millones de pasajeros") +
  xlab("") +
  ggtitle("Figura 2.2b. Gráfico estacional para Pasajeros por día laboral")

grid.arrange(
  grobs = list(g1, g2),
  ncol = 1
)


MediaAnual = as.numeric(aggregate(Pasajeros, FUN = mean))
DesviacionAnual = as.numeric(aggregate(Pasajeros, FUN = sd))

ggplot() +
  geom_point(aes(x = MediaAnual, y = DesviacionAnual), size = 2) +
  xlab("Media de pasajeros por año") + 
  ylab("Desviación típica de pasajeros por año") + 
  ggtitle("Figura 2.3. Identificación del tipo de esquema")


PasajerosMedia <- tapply(Pasajeros - mean(Pasajeros), 
                         cycle(Pasajeros), 
                         mean)
PasajerosDLMedia <- tapply((PasajerosDL - mean(PasajerosDL)), 
                           cycle(PasajerosDL), 
                           mean)

datos <- cbind(PasajerosMedia, PasajerosDLMedia)
colnames(datos) <- c("Pasajeros", "Pasajeros por día laborable")
rownames(datos) <- meses

kable(datos, 
      digits = 2,
      caption = "Efecto estacional")


PasajerosStl <- stl(Pasajeros[,1], s.window = "periodic", robust = TRUE)

error <- remainder(PasajerosStl)
sderror <- sd(error)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "Figura 2.4. Error + Intervención",
         colour = "darkblue") +
  geom_hline(yintercept = c(3, 2, -2, -3)*sderror, 
             colour = c("red", "green", "green", "red"),
             lty = 2) + 
  scale_x_continuous(breaks= seq(1996, 2019, 4))


PasajerosStl <- stl(PasajerosDL[,1], s.window = "periodic", robust = TRUE)

error <- remainder(PasajerosStl)
sderror <- sd(error)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "Figura 2.5. Error + Intervención",
         colour = "darkblue") +
  geom_hline(yintercept = c(3, 2, -2, -3)*sderror, 
             colour = c("red", "green", "green", "red"),
             lty = 2) + 
  scale_x_continuous(breaks= seq(1996, 2019, 4))


accuracy(PasajerosSnaive)
autoplot(PasajerosSnaive, 
         PI = FALSE,
         xlab = "",
         ylab = "Millones de pasajeros",
         main = "Figura 3.1. Pasajeros en transporte urbano y predicción.\nMétodo ingenuo con estacionalidad") +
  scale_x_continuous(breaks= seq(1996, 2024, 2))


PasajerosEts <- ets(Pasajeros, model = "ZZZ")
summary(PasajerosEts) 


PasajerosEtsEst <- PasajerosEts$states[nrow(PasajerosEts$states), 14:3]
names(PasajerosEtsEst) <- meses

round(PasajerosEtsEst, 2)

ggplot() +
  geom_line(aes(x = 1:12, y = PasajerosEtsEst), colour = "darkblue") + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) +
  ggtitle("Figura 4.1. Componente estacional estimada con Alisado exponencial") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = meses)


PasajerosEtsPre <- forecast(PasajerosEts, h = 60, level = 95)
autoplot(PasajerosEtsPre,
         xlab = "",
         ylab = "Millones de pasajeros",
         main = "Figura 4.2. Pasajeros (1996-2019) y predicción (2020-2024).\nMétodo de alisado exponencial") 


error <- residuals(PasajerosEts)
sderror <- sd(error)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "Figura 4.3. Error + Intervención. Método de alisado",
         colour = "darkblue") +
  geom_hline(yintercept = c(-3, -2, 2 ,3)*sderror, 
             colour = c("red", "green", "green", "red"), lty = 2) + 
  scale_x_continuous(breaks= seq(1996, 2019, 2))


ets(PasajerosDL)$method
ets(Pasajeros, lambda = 0)$method


k <- 120                 
h <- 12                  
TT <- length(Pasajeros)  
s <- TT - k - h          

mapeAlisadoPas <- matrix(NA, s + 1, h)
mapeAlisadolPas <- matrix(NA, s + 1, h)
mapeAlisadoPasDL <- matrix(NA, s + 1, h)

for (i in 0:s) {
  train.set <- subset(Pasajeros, start = i + 1, end = i + k)
  test.set <-  subset(Pasajeros, start = i + k + 1, end = i + k + h)
  
  trainDL.set <- subset(PasajerosDL, start = i + 1, end = i + k)
  testDL.set <-  subset(PasajerosDL, start = i + k + 1, end = i + k + h)
  
  fit <- ets(train.set, model = "MAA", damped = TRUE)
  fcast <- forecast(fit, h = h)
  mapeAlisadoPas[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(train.set, model = "AAA", damped = TRUE, lambda = 0)
  fcast <- forecast(fit, h = h, biasadj = TRUE)
  mapeAlisadolPas[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  
  fit <- ets(trainDL.set, model = "MAA", damped = TRUE)
  fcast <- forecast(fit, h = h)
  mapeAlisadoPasDL[i + 1,] <- 100*abs(testDL.set - fcast$mean)/testDL.set
}

errorAlisadoPas <- colMeans(mapeAlisadoPas)
errorAlisadoPasDL <- colMeans(mapeAlisadoPasDL)
errorAlisadolPas <- colMeans(mapeAlisadolPas)

datos <- cbind(1:12, errorAlisadoPas, errorAlisadoPasDL, errorAlisadolPas)
colnames(datos) <- c("Horizonte", "Pasajeros", 
                     "Pasajeros por día laborable", "Pasajeros (log)")

kable(datos, 
      digits = 2,
      caption = "MAPE según serie y horizonte temporal")

datos <- data.frame(
  factor = c(rep("Pasajeros", 12), 
             rep("Pasajeros por día laborable", 12), 
             rep("Pasajeros (log)", 12)),
  x = c(1:12, 1:12, 1:12),
  y = c(errorAlisadoPas, errorAlisadoPasDL, errorAlisadolPas)
)

ggplot(datos, aes(x = x, y = y,  colour= factor)) + 
  geom_line() +
  ggtitle("Figura 4.4. Error de predicción (MAPE) según horizonte temporal y enfoque") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12) +
  scale_y_continuous(breaks= seq(2.6, 4, .2)) +
  guides(colour = guide_legend(title = "Métodos")) + 
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1))


BoxCox.lambda(Pasajeros)


grid.arrange(
  ggAcf(log(Pasajeros), lag = 72, main = "Figura 5.1. FAC para Pasajeros (log)", 
        xlab = "", ylab = expression(log(y[t]))),
  ggAcf(diff(log(Pasajeros)), lag = 72, main = "", 
        xlab = "", ylab = expression(nabla*log(y[t]))),
  ggAcf(diff(log(Pasajeros), lag = 12),lag = 72, main = "", 
        xlab = "", ylab = expression(nabla[12]*log(y[t]))),
  ggAcf(diff(diff(log(Pasajeros), lag=12)), lag = 72, main = "", 
        xlab = "", ylab = expression(nabla*nabla[12]*log(y[t]))),
  nrow = 2
)


ndiffs(log(Pasajeros))
nsdiffs(log(Pasajeros))


series <- cbind("Original" = Pasajeros,
                "Dif reg. y est. de log" = diff(diff(log(Pasajeros), lag = 12)))
autoplot(series, facets = TRUE,
         xlab = "",
         ylab = "",
         main = "Figura 5.2. Pasajeros ")


ggtsdisplay(diff(diff(log(Pasajeros), lag = 12)), lag = 72,
            main = "Figura 5.3. FAC y FACP Pasajeros (log y d = D = 1)")


DiasNoLaborables <- monthdays(Pasajeros) - DiasLaborables
pDiasNoLaborables <- monthdays(PasajerosEtsPre$mean) - pDiasLaborables

fechas <- as.POSIXlt(seq(from = as.Date("1996-1-1"), 
                         to = as.Date("2024-12-31"), 
                         by = 1))
LunesNavidad <- 1*(fechas$wday == 1 & fechas$mon == 11 & fechas$mday == 25)
fechas <- format(fechas, format = "%Y-%m")
LunesNavidad <- tapply(LunesNavidad, fechas, sum)
LunesNavidad <- ts(LunesNavidad, start = 1996, frequency = 12)
pLunesNavidad <- subset(LunesNavidad, start = length(LunesNavidad) - 59)
LunesNavidad <- subset(LunesNavidad, end = length(LunesNavidad) - 60)
LunesNavidad[LunesNavidad == 1]

LunSanto <- Easter(1996:2024, shift = -6)
MarSanto <- Easter(1996:2024, shift = -5)
MieSanto <- Easter(1996:2024, shift = -4)
JueSanto <- Easter(1996:2024, shift = -3)

PreSanta <- c(LunSanto, MarSanto, MieSanto, JueSanto)
biz <- fechaDiaria[isBizday(fechaDiaria, holidays = PreSanta, wday = 0:6)]
bizdays <- format(biz, format = "%Y-%m")

DiasPreSanta <- table(bizdays)
DiasPreSanta <- ts(DiasPreSanta, start = 1996, frequency = 12)
DiasPreSanta <- (monthdays(DiasPreSanta) - DiasPreSanta)/4
DiasPreSanta[cycle(DiasPreSanta) == 4] <- -DiasPreSanta[cycle(DiasPreSanta) == 3]  

pDiasPreSanta <- subset(DiasPreSanta, start = length(DiasPreSanta) - 59)
DiasPreSanta <- subset(DiasPreSanta, end = length(DiasPreSanta) - 60)

LunPascua <- Easter(1996:2024, shift = 1)
MarPascua <- Easter(1996:2024, shift = 2)
MiePascua <- Easter(1996:2024, shift = 3)
JuePascua <- Easter(1996:2024, shift = 4)
ViePascua <- Easter(1996:2024, shift = 5)

Pascua <- c(LunPascua, MarPascua, MiePascua, JuePascua, ViePascua)
biz <- fechaDiaria[isBizday(fechaDiaria, holidays = Pascua, wday = 0:6)]
bizdays <- format(biz, format = "%Y-%m")

DiasPascua <- table(bizdays)
DiasPascua <- ts(DiasPascua, start = 1996, frequency = 12)
DiasPascua <- (monthdays(DiasPascua) - DiasPascua)/5
DiasPascua[cycle(DiasPascua) == 4] <- -DiasPascua[cycle(DiasPascua) == 3]  

pDiasPascua <- subset(DiasPascua, start = length(DiasPascua) - 59)
DiasPascua <- subset(DiasPascua, end = length(DiasPascua) - 60)

autoplot(DiasPreSanta, series="Pre Semana Santa",
         xlab = "",
         ylab = "Proporción de días en marzo",
         main = "Figura 5.4. Intervención para periodos vacacionales de Semana Santa") +
  autolayer(DiasPascua, series="Post Semana Santa") +
  scale_colour_manual(values=c("Pre Semana Santa"="black","Post Semana Santa"="red"),
                      breaks=c("Pre Semana Santa","Post Semana Santa"))


auto.arima(Pasajeros, 
           lambda = 0,
           d = 1, 
           D = 1,
           xreg = cbind(DiasLaborables, DiasNoLaborables, 
                        LunesNavidad, DiasPreSanta, DiasPascua))


summary(seas(Pasajeros))


PasajerosAri <- Arima(Pasajeros, 
                      lambda = 0,
                      order = c(0, 1, 1),  
                      seasonal = list(order = c(0, 1, 1), period = 12),
                      xreg = cbind(DiasLaborables, DiasNoLaborables, 
                                   LunesNavidad, DiasPreSanta, DiasPascua))
PasajerosAri

error <- residuals(PasajerosAri)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Figura 5.5. Error + Intervención. Modelo ARIMA") +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3)*sderror, 
             colour = c("red", "blue", "black", "blue", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1996, 2019, 2))


d0402 <- 1*(trunc(time(Pasajeros)) == 2002 & cycle(Pasajeros) == 4)
d0805 <- 1*(trunc(time(Pasajeros)) == 2005 & cycle(Pasajeros) == 8)
d0806 <- 1*(trunc(time(Pasajeros)) == 2006 & cycle(Pasajeros) == 8)
d0310 <- 1*(trunc(time(Pasajeros)) == 2010 & cycle(Pasajeros) == 3)

PasajerosAri <- Arima(Pasajeros,
                      lambda = 0,
                      order = c(0, 1, 1),  
                      seasonal = list(order = c(0, 1, 1), period = 12),
                      xreg = cbind(DiasLaborables, DiasNoLaborables, 
                                   LunesNavidad, DiasPreSanta, DiasPascua,
                                   d0402, d0805, d0806, d0310))
PasajerosAri

error <- residuals(PasajerosAri)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "Figura 5.6. Error + Intervención. Modelo ARIMA") +
  geom_hline(yintercept = c(-3, -2, 0, 2, 3)*sderror, 
             colour = c("red", "blue", "black", "blue", "red"), 
             lty = 2) + 
  scale_x_continuous(breaks= seq(1996, 2019, 2))


datos <- NULL
for(i in 1:length(coef(PasajerosAri))) {
  datos <- rbind(datos,
                 data.frame(
                   "Coeficiente" = names(coef(PasajerosAri))[i],
                   "Valor de p" = wald.test(b = coef(PasajerosAri), 
                                            Sigma = vcov(PasajerosAri), 
                                            Terms = i)$result$chi2[3])
                 )
}
kable(datos, digits = 4, row.names = FALSE)


ggAcf(error, lag = 36, main = "Figura 5.6. FAC del error del modelo")
Box.test(error, lag = 2,type = "Ljung-Box")
Box.test(error, lag = 24,type = "Ljung-Box")
Box.test(error^2, lag = 2, type = "Ljung-Box")
Box.test(error^2, lag = 24, type = "Ljung-Box")
jarque.bera.test(error) 

round(accuracy(PasajerosAri),2)

k <- 120                  
h <- 12                   
T <- length(Pasajeros)    
s<-T - k - h            

mapeArima <- matrix(NA, s + 1, h)

X <- cbind(DiasLaborables, DiasNoLaborables, 
           LunesNavidad, DiasPreSanta, DiasPascua,
           d0402, d0805, d0806, d0310)

for (i in 0:s) {
  train.set <- subset(Pasajeros, start = i + 1, end = i + k)
  test.set <-  subset(Pasajeros, start = i + k + 1, end = i + k + h) 
  
  X.train <- X[(i + 1):(i + k),]
  hay <- colSums(X.train)
  X.train <- X.train[, hay>0]
  
  X.test <- X[(i + k + 1):(i + k + h),]
  X.test <- X.test[, hay>0]
  
  if (length(X.train) > 0) {
    fit <- try(Arima(train.set, 
                     lambda = 0,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1), period = 12),
                     xreg=X.train), silent = TRUE)} else {
                       fit <- try(Arima(train.set,
                                        lambda = 0,
                                        order = c(0, 1, 1),
                                        seasonal = list(order = c(0, 1, 1), 
                                                        period = 12)), 
                                  silent = TRUE)
                     }
  
  if (!is.element("try-error", class(fit))) {
    if (length(X.train) > 0) fcast <- forecast(fit, h = h, xreg = X.test) else
      fcast <- forecast(fit, h = h)
    mapeArima[i + 1,] <- 100*abs(test.set - fcast$mean)/test.set
  }
}

errorArima <- colMeans(mapeArima, na.rm = TRUE)
round(errorArima, 2)


pPasajerosAri <- forecast(PasajerosAri, 
                          h = 60,
                          xreg = cbind(pDiasLaborables, pDiasNoLaborables, 
                                       pLunesNavidad, pDiasPreSanta, pDiasPascua,
                                       rep(0, 60), rep(0, 60), 
                                       rep(0 ,60), rep(0, 60)), 
                          level = 95)
autoplot(pPasajerosAri, 
         xlab = "",
         ylab = "",
         main = "Figura 5.7. Pasajeros (1996-2019) y predicción (2020-2024). Modelo Arima.") +
  scale_x_continuous(breaks= seq(1996, 2024, 4))


datos <- data.frame(
  factor = c(rep("Alisado", 12), 
             rep("Arima", 12)),
  x = c(1:12, 1:12),
  y = c(errorAlisadoPas, errorArima)
)

ggplot(datos, aes(x = x, y = y,  colour= factor)) + 
  geom_line() +
  ggtitle("Figura 6.1. Error de predicción (MAPE) según horizonte temporal y enfoque") +
  xlab("Horizonte temporal de predicción") +
  ylab("%") +
  scale_x_continuous(breaks= 1:12) +
  scale_y_continuous(breaks= seq(1.5, 4, .5)) +
  guides(colour = guide_legend(title = "Métodos")) + 
  theme(legend.position=c(0.02,0.98), legend.justification=c(0,1))
