library(readxl)
library(ggplot2)

Futuros_platino<- read.csv("Datos históricos Futuros platino.csv",  sep = ",")
Spot_platino <- read.csv("Datos históricos XPT_USD.csv",  sep = ",")

Futuros_platino[2:5] <- as.data.frame(lapply(Futuros_platino[2:5], function(x) as.numeric(gsub(",", "\\.", gsub("\\.", "", x)))))
Spot_platino[2:5] <- as.data.frame(lapply(Spot_platino[2:5], function(x) as.numeric(gsub(",", "\\.", gsub("\\.", "", x)))))


Futuros_platino$X..var. <- gsub(as.array("%") , "", Futuros_platino$X..var.)
Futuros_platino$X..var. <- as.numeric(gsub(as.array(",") , ".", Futuros_platino$X..var.))/100
Spot_platino$X..var. <- gsub(as.array("%") , "", Spot_platino$X..var.)
Spot_platino$X..var. <- as.numeric(gsub(as.array(",") , ".", Spot_platino$X..var.))/100


#Proyección mensual del precio spot del 1-01-23 a 2 años.

#Tasa libre de riesgo

r_spot <- mean(Spot_platino$X..var.[13:96])

Spot_platino_proyectados <- c()

for(i in 1:24){
  Spot_platino_proyectados[i] <- Spot_platino$Apertura[12]*exp(r_spot*(i/12))
}

Spot_platino_proyeccion = ggplot() + 
  geom_line(aes(x = 1:24, y = Spot_platino_proyectados, color = "Línea de Proyección"), linetype = "solid", size = 1) + 
  labs(title = "Proyección precios Spot platino a 2 años", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal() +
  scale_color_manual(values = "blue",guide = FALSE)

print(Spot_platino_proyeccion)

#Comparación de los precios spot proyectados y los reales para el 2023
Comparación_spot_platino1 = ggplot() + 
  geom_line(aes(x = 1:12, y = Spot_platino_proyectados[1:12], color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:12, y = rev(Spot_platino$Apertura[1:12]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Comparación de los precios spot proyectados y los reales para el 2023", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal()
print(Comparación_spot_platino1)


#Comparación de los precios spot proyectados y los reales en dos años
Comparación_spot_platino = ggplot() + 
  geom_line(aes(x = 13:36, y = Spot_platino_proyectados, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:24, y = rev(Spot_platino$Apertura[1:24]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Comparación de los precios spot proyectados y los reales", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal()
print(Comparación_spot_platino)

#Proyección mensual de futuros a 2 años

Futuros_platino_proyectados<- c()

#Proyecciones para el 2023

for(i in 1:12 ){
  r <- mean(Spot_platino$X..var.[(13-i):(25-i)])
  Futuros_platino_proyectados[i] <- Spot_platino$Apertura[13-i]*exp(r*(1/12))
}

#Proyecciones 2024
r_futuros <- mean(Spot_platino$X..var.[1:12])

#for(i in 13:24){
  #Futuros_platino_proyectados[i] <- Spot_platino$Apertura[1]*exp(r_futuros*(i/12))
#}

for(i in 13:25){
  Futuros_platino_proyectados[i] <- Spot_platino_proyectados[i-1]*exp(r_futuros*(1/12))
}


Futuros_platino_proyeccion = ggplot() + 
  geom_line(aes(x = 1:24, y = Futuros_platino_proyectados[1:24], color = "Línea de Proyección"), linetype = "solid", size = 1) + 
  labs(title = "Proyección precios futuros platino a 2 años", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal() +
  scale_color_manual(values = "blue",guide = FALSE)
print(Futuros_platino_proyeccion)

#Comparación de los precios futuros proyectados y los reales para el 2023
Comparación_futuros_platino = ggplot() + 
  geom_line(aes(x = 1:12, y = Futuros_platino_proyectados[1:12] , color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:12, y = rev(Futuros_platino$Apertura[1:12]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Comparación de los precios futuros proyectados y los reales para el 2023", x = "Tiempo", y = "¨Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal()
print(Comparación_futuros_platino)


#Comparación de los precios spot proyectados y los reales en dos años
Comparación_futuros_platino2 = ggplot() + 
  geom_line(aes(x = 13:37, y = Futuros_platino_proyectados, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:24, y = rev(Futuros_platino$Apertura[1:24]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Comparación de los precios spot proyectados y los reales", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal()
print(Comparación_futuros_platino2)
