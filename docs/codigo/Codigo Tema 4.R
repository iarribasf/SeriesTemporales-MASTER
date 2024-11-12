#----------------------------------------------------------
# CODIGO TEMA 4
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
# Alimentos per capita
alimentospc <- read.csv2("./series/Alimentacionpc.csv", 
                         header = TRUE)

alimentospc <- ts(alimentospc, 
                  start = 1990, 
                  freq = 1)

autoplot(alimentospc, 
         main = "", 
         xlab = "Año", 
         ylab = "", 
         ylim = c(0, 700))

# Defunciones
defunciones <- read.csv2("./series/Defunciones.csv",
                         header = TRUE)

defunciones <- ts(defunciones, 
                  start = 1, 
                  freq = 1)

autoplot(defunciones, 
         main = "", 
         xlab = "Día", 
         ylab = "", 
         ylim = c(500, 2000))
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Media movil
#----------------------------------------------------------
# Funcion
mmf <- function (x, r = 3, h = 10, level = c(80, 95)) 
{
  n <- length(x)
  startx <- start(x)
  frequencyx <-frequency(x)
  
  mm <- stats::filter(x, rep(1/r, r), side = 1)
  fits <- c(NA, mm[-n])
  res <- x - fits
  f <- rep(mm[n], h)
  
  if (min(level) > 0 && max(level) < 1) {
    level <- 100 * level
  } else if (min(level) < 0 || max(level) > 99.99) 
    stop("Confidence limit out of range")
  
  nconf <- length(level)
  s <- sd(res, na.rm = TRUE)
  
  lower <- upper <- matrix(NA, nrow = h, ncol = nconf)
  for (i in 1:nconf) {
    if (n > 1) {
      tfrac <- qt(0.5 - level[i]/200, n - 1)
    } else {
      tfrac <- -Inf
    }
    w <- -tfrac * s * sqrt(1 + 1/n)
    lower[, i] <- f - w
    upper[, i] <- f + w
  }
  
  colnames(lower) <- colnames(upper) <- paste(level, "%", sep = "")
  
  fits <- ts(fits, start = startx, frequency = frequencyx)
  res <- ts(res, start = startx, frequency = frequencyx)
  f <- ts(f, start = time(x)[n] + 1/frequencyx, frequency = frequencyx)
  lower <- ts(lower, start = time(x)[n] + 1/frequencyx, frequency = frequencyx)
  upper <- ts(upper, start = time(x)[n] + 1/frequencyx, frequency = frequencyx)
  
  out <- list(method = "Moving average", level = level, x = x,
              mean = f, lower = lower, upper = upper, fitted = fits, 
              residuals = res, order = r)
  
  return(structure(out, class = "forecast"))
}
#----------------------------------------------------------
#
# METODOS SENCILLOS: ALIMENTOS
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Predicciones
mediaAlimentospc <- meanf(alimentospc, 
                          h = 5)
mediaAlimentospc

naiveAlimentospc <- naive(alimentospc,
                          h = 5)
naiveAlimentospc

mmAlimentospc <- mmf(alimentospc, 
                     r = 4, 
                     h = 5)

mmAlimentospc

# Grafica predicciones
autoplot(alimentospc, 
         series = "Alimentos",
         xlab = "",
         ylab = "Kg per cápita",
         main = "",
         ylim = c(0, 700)) +
  autolayer(mediaAlimentospc, series="Media", PI = FALSE) +
  autolayer(naiveAlimentospc, series="Ingenuo", PI = FALSE) +
  autolayer(mmAlimentospc, series="Media móvil (r = 4)", PI = FALSE) +
  scale_colour_discrete(limits=c("Alimentos", "Media", 
                                 "Ingenuo", "Media móvil (r = 4)")) +
  labs(colour="Métodos") + 
  theme(legend.position=c(0.2,0.3))

# Calidad de ajuste
accuracy(mediaAlimentospc)
accuracy(naiveAlimentospc)
accuracy(mmAlimentospc)

# Valores alternativos para el orden de la media movil
for(r in 1:5) {
  error <- accuracy(mmf(alimentospc, r = r))
  print(error)
}
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: ALIMENTOS
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Ajuste
etsAlimentospc <- ets(alimentospc, 
                      model = "ZNN")

summary(etsAlimentospc)

tail(etsAlimentospc$states, 1)

# Prediccion
etsAlimentospcf <- forecast(etsAlimentospc,
                            h = 5, 
                            level = 95)

etsAlimentospcf

autoplot(etsAlimentospcf,
         xlab = "",
         ylab = "Kg per cápita",
         main = "")

# Valores atípicos por score
error <- residuals(etsAlimentospc)
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
  scale_x_continuous(breaks= seq(1990, 2024, 4)) 

fechas <- format(seq(as.Date("1990-01-01"), as.Date("2023-01-01"), "year"), "%Y")
fechas[abs(error) > 2.5 * sderror]

# Valores atípicos por Tukey
atipicos <- tsoutliers(error)
fechas[atipicos$index]
#----------------------------------------------------------
#
# ARIMA: ALIMENTOS
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Identificacion
ndiffs(alimentospc)

d2020 <- 1* (time(alimentospc) == 2020)
d2022 <- 1* (time(alimentospc) == 2022)

auto.arima(alimentospc,
           d = 0,
           xreg = cbind(d2020, d2022),
           trace = TRUE)

# Estimacion + valores atípicos
ariAlimentospc <- Arima(alimentospc, 
                        order = c(1, 0, 0),
                        include.constant = TRUE,
                        xreg = cbind(d2020, d2022))

ariAlimentospc

error <- residuals(ariAlimentospc)
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
  scale_x_continuous(breaks= seq(1990, 2024, 4)) 

fechas[abs(error) > 2.5 * sderror]

d2023 <- 1* (time(alimentospc) == 2023)

ariAlimentospc <- Arima(alimentospc, 
                        order = c(1, 0, 0),
                        include.constant = TRUE,
                        xreg = cbind(d2020, d2022, d2023))
ariAlimentospc

# Validacion
coeftest(ariAlimentospc)

accuracy(ariAlimentospc)

# Prediccion sin efecto ocio
ariAlimentospcf <- forecast(ariAlimentospc, 
                            h = 5, 
                            level = 95,
                            xreg = cbind(rep(0, 5), rep(0, 5), rep(0, 5)))
ariAlimentospcf

# Prediccion con efecto ocio
ariAlimentospcfb <- forecast(ariAlimentospc, 
                             h = 5, 
                             level = 95,
                             xreg = cbind(rep(0, 5), rep(0, 5), rep(1, 5)))
ariAlimentospcfb

autoplot(alimentospc, 
         series = "Alimentos",
         xlab = "",
         ylab = "Kg per cápita",
         main = "",
         PI = FALSE,
         ylim = c(300, 700)) +
  autolayer(ariAlimentospcf,  PI = FALSE, series = "Aumento ocio desaparece") +
  autolayer(ariAlimentospcfb, PI = FALSE, series = "Aumento ocio se mantiene") +
  scale_x_continuous(breaks= seq(1990, 2028, 4)) +
  scale_colour_discrete(limits=c("Alimentos", "Aumento ocio desaparece", 
                                 "Aumento ocio se mantiene")) +
  labs(colour="Predicciones") + 
  theme(legend.position=c(0.2,0.2))

# Comparacion entre las diferentes predicciones
autoplot(alimentospc, 
         series = "Alimentos",
         xlab = "",
         ylab = "Kg per cápita",
         main = "") +
  autolayer(mediaAlimentospc, series="Media", PI = FALSE) +
  autolayer(naiveAlimentospc, series="Ingenuo", PI = FALSE) +
  autolayer(mmAlimentospc, series="Media móvil", PI = FALSE) +
  autolayer(etsAlimentospcf, series="Alisado", PI = FALSE) +
  autolayer(ariAlimentospcfb, series="ARIMA", PI = FALSE) +
  scale_colour_discrete(limits=c("Alimentos", "Media", "Ingenuo",
                                 "Media móvil", "Alisado", "ARIMA")) +
  labs(colour="Métodos") + 
  theme(legend.position=c(0.1,0.3))
#----------------------------------------------------------
#
# METODOS SENCILLOS: DEFUNCIONES
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Predicciones
naiveDefunciones <- naive(defunciones, 
                          h = 28,
                          level = 95)

summary(naiveDefunciones)

mmDefunciones <- mmf(defunciones, 
                     r = 70,
                     h = 28)

summary(mmDefunciones)
#----------------------------------------------------------
#
# ALISADO EXPONENCIAL: DEFUNCIONES
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Ajuste
etsDefunciones <- ets(defunciones, 
                      model = "ZNN")

summary(etsDefunciones)

# Prediccion
etsDefuncionesf <- forecast(etsDefunciones, 
                            h = 28,
                            level = 95)
etsDefuncionesf

autoplot(etsDefuncionesf)

# Valores atípicos por score
error <- residuals(etsDefunciones)
sderror <- sd(error)

autoplot(error, series="Error",
         colour = "black",
         xlab = "",
         ylab = "Error",
         main = "") +
  geom_hline(yintercept = c(-3, 3)*sderror, 
             colour = c("red", "red"), 
             lty = 2) + 
  geom_point() +
  scale_x_continuous(breaks= seq(1990, 2024, 4)) 

fechas <- format(seq(as.Date("2022-1-1"), as.Date("2023-12-31"), "day"), "%Y-%m-%d")

fechas[abs(error) > 3 * sderror]

# Valores atípicos por Tukey
atipicos <- tsoutliers(error)
fechas[atipicos$index]
#----------------------------------------------------------
#
# ARIMA: DEFUNCIONES
#
#----------------------------------------------------------
#
#----------------------------------------------------------
# Identificacion
auto.arima(defunciones,
           d = 0)

# Estimacion + valores atípicos
ariDefunciones <- Arima(defunciones, 
                        order = c(2, 0, 0),
                        include.constant = TRUE)
ariDefunciones

error <- residuals(ariDefunciones)
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
  scale_x_continuous(breaks= seq(1990, 2024, 4)) 

fechas[abs(error) > 3 * sderror]


# Validacion
coeftest(ariDefunciones)

accuracy(ariDefunciones)

# Prediccion
arimaDefuncionesf <- forecast(ariDefunciones, 
                              h = 28,
                              level = 95)
arimaDefuncionesf

autoplot(arimaDefuncionesf)


autoplot(arimaDefuncionesf)

# Comparacion entre las diferentes predicciones
autoplot(defunciones, 
         series = "Defunciones",
         xlab = "",
         ylab = "",
         main = "",
         ylim = c(900, 1600)) +
  xlim(600, 750) +
  autolayer(naiveDefunciones, series="Ingenuo I", PI = FALSE) +
  autolayer(mmDefunciones, series="Media móvil (r = 70)", PI = FALSE) +
  autolayer(etsDefuncionesf, series="Alisado", PI = FALSE) +
  autolayer(arimaDefuncionesf, series="Arima", PI = FALSE) +
  scale_colour_discrete(limits=c("Defunciones", "Ingenuo I", "Media móvil (r = 70)", 
                                 "Alisado", "Arima")) +
  labs(colour="Métodos") + 
  theme(legend.position=c(0.13, 0.75))




