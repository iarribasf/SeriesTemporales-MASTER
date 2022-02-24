#---------------------------------------------------------------
# Codigo Tema 1
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)

#- Serie Libros
libros <- read.csv2("series/libros.csv", header = TRUE)
libros <- ts(libros[, 2], start = 1993, frequency  = 1)

autoplot(libros,
         xlab = "",
         ylab = "Títulos",
         main = "Títulos publicados (libros y folletos)")

#- Serie Nacimientos
nacimientos <- read.csv2("series/nacimientos.csv", header = TRUE)
nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos mensuales")

# Serie electricidad
electricidad <- read.csv2("series/Consumo electrico.csv", header = TRUE)
electricidad <- ts(electricidad[, 2],
                   start = c(1, 2),
                   frequency = 7)

autoplot(electricidad,
         xlab = "",
         ylab = "MWh",
         main = "Consumo diario de electricidad")


start(nacimientos)
start(electricidad)

end(nacimientos)
end(electricidad)

frequency(nacimientos)
frequency(electricidad)

time(nacimientos)
time(electricidad)

cycle(nacimientos)
cycle(electricidad)

#- Análisis de la tendencia
nacimientosAnual <- aggregate(nacimientos, FUN = sum)
autoplot(nacimientosAnual/1000,
         xlab = "",
         ylab = "Nacimientos (miles)",
         main = "Nacimientos por año")

electricidadSemanal <- aggregate(electricidad, FUN = sum)
autoplot(electricidadSemanal,
         xlab = "",
         ylab = "MWh",
         main = "Consumo de electricidad por semana")

#- Análisis de la estacionalidad
nacimientosb <- window(nacimientos, start = 2000)
ggsubseriesplot(nacimientosb) +
  ylab("Nacimientos") +
  xlab("") +
  ggtitle("Gráfico estacional de subseries")

ggseasonplot(nacimientosb, 
             year.labels=TRUE, 
             xlab = "",
             ylab = "Nacimientos",
             main = "Gráfico estacional de lineas")


#- Análisis del esquema
CasosAno = aggregate(nacimientosb, FUN = sum)
DesviacionAno = aggregate(nacimientosb, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosAno, y = DesviacionAno), size = 2) +
  xlab("Nacimientos anuales") + 
  ylab("Des. Tip. intra-anual")


CasosSemana = aggregate(electricidad, FUN = sum)
DesviacionSemana = aggregate(electricidad, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosSemana, y = DesviacionSemana), size = 2) +
  xlab("Consumo semanal") + 
  ylab("Des. Tip. intra-semanal")

#- Calculo de la componente estacional. Método sencillo
componenteEstacional <- tapply(nacimientosb/mean(nacimientosb), 
                               cycle(nacimientosb), 
                               FUN = mean)
round(componenteEstacional, 2)


componenteEstacional <- tapply(electricidad - mean(electricidad), 
                               cycle(electricidad), 
                               FUN = mean)
round(componenteEstacional, 2)

#- Descomposicion ADITIVA por medias moviles
eleDesAdi <- decompose(electricidad, type = "addi")
autoplot(eleDesAdi,
         xlab = "",
         main = "Descomposición aditiva de Electricidad por medias móviles")

summary(electricidad - trendcycle(eleDesAdi) - seasonal(eleDesAdi) 
        - remainder(eleDesAdi))

autoplot(electricidad, series="Demanda eléctrica",
         xlab = "",
         ylab = "MWh",
         main = "Demanda eléctrica: serie y tendencia") +
  autolayer(trendcycle(eleDesAdi), series="Tendencia") +
  scale_colour_manual(values=c("Demanda eléctrica"="black","Tendencia"="red"),
                      breaks=c("Demanda eléctrica","Tendencia"))

eleDesAdi$figure
sum(eleDesAdi$figure)

compEstacional <- eleDesAdi$figure[c(7, 1:6)]
ggplot() +
  geom_line(aes(x = 1:7, y = compEstacional)) + 
  geom_hline(yintercept = 0, colour = "blue", lty = 2) +
  ggtitle("Componente estacional de Electricidad (esquema aditivo)") +
  xlab("") +
  ylab("Componente estacional") +
  scale_x_continuous(breaks= 1:7, 
                     labels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")) 

#- Descomposicion MULTIPLICATIVA por medias moviles
nacDesMul <- decompose(nacimientos, type = "mult")
autoplot(nacDesMul,
         xlab = "",
         main = "Descomposición multiplicativa de Nacimientos por medias móviles")

nacDesMul$figure
sum(nacDesMul$figure)

ggplot() + 
  geom_line(aes(x = 1:12, y = nacDesMul$figure)) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("Componente estacional de Nacimientos (esquema multiplicativo)") +
  xlab("") +
  ylab("Componente estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 

#- Descomposicion por regresiones locales ponderadas
eleStl <- stl(electricidad, 
              s.window = "periodic",
              robust = TRUE)
head(eleStl$time.series)

autoplot(eleStl,
         xlab = "",
         main = "Descomposición de Electricvidad por regresores locales ponderados")

head(seasonal(eleStl), 7)
sum(head(seasonal(eleStl), 7))

# Sencillo
round(as.numeric(componenteEstacional), 2)
# decompose
round(seasonal(eleDesAdi)[c(7, 1:6)], 2)
# stl
round(seasonal(eleStl)[c(7, 1:6)], 2)

#- Lo mismo asumiendo que la estacionalidad varia en el tiempo
eleStl11 <- stl(electricidad, 
                s.window = 11,
                robust = TRUE)

xx <- window(seasonal(eleStl), start = 20, end = 40)
yy <- window(seasonal(eleStl11), start = 20, end = 40)
autoplot(xx, series="s.window = 'periodic'",
         ylab = "Componente estacional",
         main = "Componente estacional para Electricidad") +
  autolayer(yy, series="s.window = 11") +
  scale_colour_manual(values=c("s.window = 'periodic'"="black","s.window = 11"="red"))

