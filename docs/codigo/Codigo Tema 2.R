#----------------------------------------------------------
# CODIGO TEMA 2
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
  ylab("Nacimientos") +
  xlab("") +
  ggtitle("")

ggseasonplot(window(nacimientos, start = 2018),
             year.labels = TRUE, 
             xlab = "",
             ylab = "Nacimientos",
             main = "")

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

autoplot(electricidad, 
         series="Demanda eléctrica",
         xlab = "",
         ylab = "MWh",
         main = "") +
  autolayer(trendcycle(eleDesAdi), 
            series="Tendencia") +
  scale_colour_manual(values=c("Demanda eléctrica"="black","Tendencia"="red"),
                      breaks=c("Demanda eléctrica","Tendencia"))

eleDesAdi$figure
sum(eleDesAdi$figure)

compEstacional <- eleDesAdi$figure[c(2:7, 1)]
ggplot() +
  geom_line(aes(x = 1:7, y = compEstacional)) + 
  geom_hline(yintercept = 0, colour = "blue", lty = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Componente estacional") +
  scale_x_continuous(breaks= 1:7, 
                     labels = c("Lunes", "Martes", "Miércoles", "Jueves", 
                                "Viernes", "Sábado", "Domingo")) 


# Esquema multiplicativo
nacDesMul <- decompose(nacimientos, 
                       type = "mult")

autoplot(nacDesMul,
         xlab = "",
         main = "")

nacDesMul$figure
sum(nacDesMul$figure)

ggplot() +
  geom_line(aes(x = 1:12, y = nacDesMul$figure, colour = "black")) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 
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

head(eleStl$time.series, n = 7)

autoplot(eleStl,
         xlab = "",
         main = "")

sum(head(seasonal(eleStl), 7))

#tapply
round(as.numeric(componenteEstacional), 2)
# decompose
round(seasonal(eleDesAdi)[c(2:7, 1)], 2)
# stl
round(seasonal(eleStl)[c(2:7, 1)], 2)


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
  autolayer(yy, 
            series="s.window = 11") +
  scale_colour_manual(values=c("s.window = 'periodic'"="black","s.window = 11"="red"))

