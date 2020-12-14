#---------------------------------------------------------------
# Codigo Tema 1
#---------------------------------------------------------------


#- Cargamos las librerias que necesitamos para este tema
library(forecast)
library(ggplot2)

#- Serie Libros
libros <- read.csv2("libros.csv", header = TRUE)
libros <- ts(libros[, 2], start = 1993, frequency  = 1)

autoplot(libros,
         xlab = "",
         ylab = "Títulos",
         main = "Títulos publicados (libros y folletos)")

#- Serie Nacimientos
nacimientos <- read.csv2("nacimientos.csv", header = TRUE)
nacimientos <- ts(nacimientos[, 2],
                  start = c(1975, 1),
                  frequency = 12)

autoplot(nacimientos,
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos mensuales")

start(nacimientos)
end(nacimientos)
frequency(nacimientos)
head(time(nacimientos), n = 48)  #Mostramos sólo los 4 primeros años
head(cycle(nacimientos), n = 48) #Mostramos sólo los 4 primeros años

#- Serie Sucursal
sucursal <- read.table("sucursal.csv", header = TRUE)
sucursal <- ts(sucursal, start = c(1, 5), freq = 7)
autoplot(sucursal,
         xlab = "",
         ylab = "Euros",
         main = "Extracción de dinero de un cajero") + 
  scale_x_continuous(breaks= 22:26, labels = 22:26) 

#- Análisis de la tendencia
nacimientosAnual <- aggregate(nacimientos, FUN = sum)
autoplot(nacimientosAnual/1000,
         xlab = "",
         ylab = "Nacimientos (miles)",
         main = "Nacimientos por año")

sucursalAnual <- aggregate(sucursal,FUN = sum)
autoplot(sucursalAnual,
         xlab = "",
         ylab = "Euros",
         main = "Extracción de dinero de un cajero por semana")

#- Análisis de la estacionalidad
nacimientosb <- window(nacimientos, start = 2000)
ggseasonplot(nacimientosb, 
             year.labels=TRUE, 
             year.labels.left=TRUE,
             xlab = "",
             ylab = "Nacimientos",
             main = "Gráfico estacional de lineas.\nNacimientos")

ggseasonplot(nacimientosb, polar=TRUE,
             xlab = "",
             ylab = "",
             main = "Gráfico estacional polar.\nNacimientos") +
  guides(colour=FALSE)

ggsubseriesplot(nacimientosb) +
  ylab("Nacimientos") +
  xlab("") +
  ggtitle("Gráfico estacional de subseries.\nNacimientos")

#- Calculo de la componente estacional. Método sencillo
componenteEstacional <- tapply(nacimientosb/mean(nacimientosb), 
                               cycle(nacimientosb), 
                               FUN = mean)
round(componenteEstacional, 2)

#- Descomposicion ADITIVA por medias moviles
nacDesAdi <- decompose(nacimientos, type = "addi")
autoplot(nacDesAdi,
         xlab = "",
         main = "Descomposición aditiva de Nacimientos por medias móviles")

summary(nacimientos - trendcycle(nacDesAdi) - seasonal(nacDesAdi) 
        - remainder(nacDesAdi))

autoplot(nacimientos, series="Nacimientos",
         xlab = "",
         ylab = "Nacimientos",
         main = "Nacimientos en España: serie y tendencia") +
  autolayer(trendcycle(nacDesAdi), series="Tendencia") +
  scale_colour_manual(values=c("Nacimientos"="black","Tendencia"="red"),
                      breaks=c("Nacimientos","Tendencia"))

nacDesAdi$figure
sum(nacDesAdi$figure)

ggplot() +
  geom_line(aes(x = 1:12, y = nacDesAdi$figure)) + 
  geom_hline(yintercept = 0, colour = "blue", lty = 2) +
  ggtitle("Componente estacional de Nacimientos (esquema aditivo)") +
  xlab("") +
  ylab("Componente estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) 

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
nacStl <- stl(nacimientos[, 1], 
               s.window = "periodic",
               robust = TRUE)
head(nacStl$time.series)

autoplot(nacStl,
         xlab = "",
         main = "Descomposición de Nacimientos por regresores locales ponderados")

head(seasonal(nacStl), 12)
sum(head(seasonal(nacStl), 12))

#- Lo mismo asumiendo que la estacionalidad varia en el tiempo
nacStl17 <- stl(nacimientos, 
                s.window = 17,
                robust = TRUE)

xx <- window(seasonal(nacStl), start = 1995, end = 2005 - 1/12)
yy <- window(seasonal(nacStl17), start = 1995, end = 2005 - 1/12)
autoplot(xx, series="s.window = 'periodic'",
         ylab = "Componente estacional",
         main = "Componente estacional para Nacimientos") +
  autolayer(yy, series="s.window = 17") +
  scale_colour_manual(values=c("s.window = 'periodic'"="black","s.window = 17"="red"))#,
                 #     breaks=c("s.window = 'periodic'","s.window = 17"))
