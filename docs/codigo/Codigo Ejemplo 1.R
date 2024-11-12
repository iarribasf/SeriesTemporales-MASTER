#---------------------------------------------------------------
# Codigo ejemplo tema 1
#---------------------------------------------------------------
#- Cargamos las librerias que necesitamos para este ejemplo
library(forecast)
library(ggplot2)

#- Cargamos el ejemplo
DefEnfCer <- read.csv2("./series/Enfermedades cerebrovasculares.csv", 
                       header = TRUE)

DefEnfCer <- ts(DefEnfCer[,2], 
                start = 1980,
                freq = 12)

autoplot(DefEnfCer,
         xlab = "",
         ylab = "Casos",
         main = "") +
  scale_x_continuous(breaks= seq(1980, 2024, 2)) 

#- Tipo de esquema
CasosAnual = aggregate(DefEnfCer, FUN = sum)
DesviacionAnual = aggregate(DefEnfCer, FUN = sd)

ggplot() +
  geom_point(aes(x = CasosAnual, y = DesviacionAnual), size = 2) +
  xlab("Total de casos") + 
  ylab("Desviación típica") + 
  ggtitle("")

#- Tendencia
autoplot(CasosAnual,
         xlab = "",
         ylab = "Casos",
         main = "") +
  scale_x_continuous(breaks= seq(1980, 2024, 4))

#- Estacionalidad
ggsubseriesplot(DefEnfCer, 
                xlab = "",
                ylab = "",
                main = "") +
  guides(colour=FALSE)

#- Descomposicion
DefEnfCerDesMul <- decompose(DefEnfCer, 
                             type = "mult")

autoplot(DefEnfCerDesMul,
         xlab = "",
         main = "")

DefEnfCerDesMul$figure

# - Descomposicion de los casos por día
DefEnfCerDiaDesMul <- decompose(DefEnfCer/monthdays(DefEnfCer), 
                                type = "mult")

ggplot() +
  geom_line(aes(x = 1:12, y = DefEnfCerDesMul$figure, colour = "black")) + 
  geom_line(aes(x = 1:12, y = DefEnfCerDiaDesMul$figure, colour = "red")) + 
  geom_hline(yintercept = 1, colour = "blue", lty = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Efecto estacional") +
  scale_x_continuous(breaks= 1:12, 
                     labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                                "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_color_discrete(name = "Componente estacional", 
                       labels = c("Defunciones", "Defunciones por día")) +
  theme(legend.position=c(0.15,0.15))

#- Analisis grñafico del error
error <- log(remainder(DefEnfCerDesMul))

sderror <- sd(error, na.rm = TRUE)

autoplot(error,
         xlab = "",
         ylab = "Error",
         main = "",
         colour = "black") +
  geom_hline(yintercept = c(3, 2, -2, -3)*sderror, 
             colour = c("red", "green", "green", "red"),
             lty = 2) + 
  scale_x_continuous(breaks= seq(1980, 2024, 4)) 

fechas <- format(seq(as.Date("1980-01-01"), as.Date("2023-12-01"), "month"), "%Y-%m")
fechas[abs(error) > 3 * sderror]

atipicos <- tsoutliers(error)
fechas[atipicos$index]

#- Incorrelacion
gglagplot(error, 
          lags = 1, 
          do.lines = FALSE, 
          colour = FALSE, 
          main = "")

#- Homocedasticidad
DesviacionAnual = aggregate(error, FUN = sd)
ggplot() +
  geom_point(aes(x = time(DesviacionAnual), y = DesviacionAnual), size = 2) +
  ggtitle("") +
  xlab("") +
  ylab("Desviación estándar del error") 
