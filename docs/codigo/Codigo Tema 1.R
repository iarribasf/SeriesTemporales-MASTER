#----------------------------------------------------------
# CODIGO TEMA 1
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
         xlab = "",
         ylab = "Kg per cápita",
         main = "")

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

# Demanda eléctrica
electricidad <- read.csv("./series/Consumo electrico.csv", 
                         header = TRUE)

electricidad <- ts(electricidad[, 1],
                   start = c(1, 7),
                   frequency = 7)

autoplot(electricidad,
         xlab = "",
         ylab = "GWh",
         main = "")

start(nacimientos)
end(nacimientos)
frequency(nacimientos)
time(nacimientos)
cycle(nacimientos)

start(electricidad)
end(electricidad)
frequency(electricidad)
time(electricidad)
cycle(electricidad)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Esquema
#----------------------------------------------------------
# Aditivo: electricidad
CasosSemana = aggregate(electricidad, FUN = sum)
DesviacionSemana = aggregate(electricidad, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosSemana, y = DesviacionSemana), size = 2) +
  xlab("Consumo semanal") + 
  ylab("Des. tip. intrasemanal")

# Multiplicativo: nacimientos
CasosAno = aggregate(nacimientos, FUN = sum)
DesviacionAno = aggregate(nacimientos, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosAno, y = DesviacionAno), size = 2) +
  xlab("Nacimientos anuales") + 
  ylab("Des. tip. intraanual")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Tendencia
#----------------------------------------------------------
# Extracción de la tendencia
nacimientosAnual <- aggregate(nacimientos, FUN = sum)

autoplot(nacimientosAnual/1000,
         xlab = "",
         ylab = "Nacimientos (miles)",
         main = "")

electricidadSemanal <- aggregate(electricidad, FUN = sum)

autoplot(electricidadSemanal,
         xlab = "",
         ylab = "GWh",
         main = "")
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Estacionalidad
#----------------------------------------------------------
# Graficas
nacimientosb <- window(nacimientos, start = 2000)

ggsubseriesplot(nacimientosb) +
  labs(x = "", y = "Nacimientos", title = "") 
ggtitle("")

ggseasonplot(window(nacimientos, start = 2018, end = c(2024, 12)),
             year.labels = TRUE) +
  labs(x = "", y = "Nacimientos", title = "") 

# Componente numerica
componenteEstacional <- tapply(nacimientosb/mean(nacimientosb), 
                               cycle(nacimientosb), 
                               FUN = mean)

round(componenteEstacional, 2)

componenteEstacional <- tapply(electricidad - mean(electricidad), 
                               cycle(electricidad), 
                               FUN = mean)

round(componenteEstacional, 2)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Extracción de una subserie
#----------------------------------------------------------
window(nacimientos, start = c(2000, 1), end = c(2009, 12))
window(nacimientos, start = c(2010, 3))
window(nacimientos, end = c(1999, 12))
window(nacimientos, start = c(2000, 3), freq = TRUE)

subset(nacimientos, start = 10, end = 34)
subset(nacimientos, start = 121)
subset(nacimientos, start = length(nacimientos) - 47)
subset(nacimientos, end = length(nacimientos) - 48)
subset(nacimientos, season  = 5)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Descomposición por medias móviles
#----------------------------------------------------------
#Esquema aditivo
eleDesAdi <- decompose(electricidad, 
                       type = "addi")

autoplot(eleDesAdi,
         xlab = "",
         main = "")

tmp <- trendcycle(eleDesAdi) + seasonal(eleDesAdi) + remainder(eleDesAdi)
summary(electricidad - tmp)

componenteEstacionalElecDes <- seasonal(eleDesAdi)[1:7]
componenteEstacionalElecDes
sum(componenteEstacionalElecDes)

ggplot() +
  geom_line(aes(x = 1:7, y = componenteEstacionalElecDes)) + 
  geom_hline(yintercept = 0, colour = "blue", lty = 2) +
  labs(x = "", y = "GWh", title = "") +
  scale_x_continuous(breaks= 1:7, 
                     labels = c("Lunes", "Martes", "Miércoles", "Jueves", 
                                "Viernes", "Sábado", "Domingo")) 


# Esquema multiplicativo
nacDesMul <- decompose(nacimientos, 
                       type = "mult")

autoplot(nacDesMul,
         xlab = "",
         main = "")

componenteEstacionalNacDes <- seasonal(nacDesMul)[1:12]
componenteEstacionalNacDes
sum(componenteEstacionalNacDes)
#----------------------------------------------------------
#
#
#
#----------------------------------------------------------
# Descomposición por regresiones locales ponderadas
#----------------------------------------------------------
eleStl <- stl(electricidad, 
              s.window = "periodic",
              robust = TRUE)

autoplot(eleStl,
         xlab = "",
         main = "")

componenteEstacionalElecStl <- seasonal(eleStl)[1:7]
componenteEstacionalElecStl
sum(componenteEstacionalElecStl)

# tapply
round(as.numeric(componenteEstacionalElec), 2)
# decompose
round(componenteEstacionalElecDes, 2)
# stl
round(componenteEstacionalElecStl, 2)


# Estacionalidad no fija
eleStl11 <- stl(electricidad, 
                s.window = 11,
                robust = TRUE)

xx <- window(seasonal(eleStl), start = 20, end = 40)
yy <- window(seasonal(eleStl11), start = 20, end = 40)
autoplot(xx, series="s.window = 'periodic'",
         xlab = "",
         ylab = "GWh",
         main = "") +
  autolayer(yy, series="s.window = 11") +
  scale_colour_manual(values=c("s.window = 'periodic'"="black","s.window = 11"="red"))

