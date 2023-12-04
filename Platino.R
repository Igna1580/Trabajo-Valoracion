library(readxl)
library(ggplot2)
library(lubridate)

Futuros_platino <- read.csv("Datos históricos Futuros platino.csv",  sep = ",")
Futuros_platino[2:5] <- as.data.frame(lapply(Futuros_platino[2:5], function(x) as.numeric(gsub(",", "\\.", gsub("\\.", "", x)))))
Futuros_platino$X..var. <- gsub(as.array("%") , "", Futuros_platino$X..var.)
Futuros_platino$X..var. <- as.numeric(gsub(as.array(",") , ".", Futuros_platino$X..var.))/100
Futuros_platino$Fecha <- as.Date(Futuros_platino$Fecha, format = "%d.%m.%Y")

Spot_platino <- read.csv("Datos históricos XPT_USD.csv",  sep = ",")
Spot_platino[2:5] <- as.data.frame(lapply(Spot_platino[2:5], function(x) as.numeric(gsub(",", "\\.", gsub("\\.", "", x)))))
Spot_platino$X..var. <- gsub(as.array("%") , "", Spot_platino$X..var.)
Spot_platino$X..var. <- as.numeric(gsub(as.array(",") , ".", Spot_platino$X..var.))/100
Spot_platino$Fecha <- as.Date(Spot_platino$Fecha, format = "%d.%m.%Y")

#Proyección mensual del precio spot del 1-01-23 a 2 años.

#Tasa libre de riesgo

Faltantes_platino <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01", "2024-07-01", 
                               "2024-08-01", "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01"))
Fecha <- c(rev(Spot_platino$Fecha[1:12]), Faltantes_platino)
Resultados_Spot_platino <- data.frame(Fecha = Fecha, Spot_platino_proyectados = rep(0, 24))

r_spot <- mean(Spot_platino$X..var.[13:96])
Spot_platino_proyectados <- c()

for(i in 1:24){
  Spot_platino_proyectados[i] <- Spot_platino$Apertura[12]*exp(r_spot*(i/12))
}

Resultados_Spot_platino$Spot_platino_proyectados <- Spot_platino_proyectados

#Comparación de los precios spot proyectados y los reales en dos años
Comparación_spot_platino <- ggplot() +
  geom_line(Resultados_Spot_platino, mapping=aes(x = Fecha, y = Spot_platino_proyectados, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(rev(Spot_platino[1:24,]), mapping=aes(x = Fecha, y = Apertura, color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "", x = "Tiempo", y = "Precio Spot", color = "") +
  theme_minimal()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(Comparación_spot_platino)

Spot_platino_proyeccion = ggplot() + 
  geom_line(aes(x = 1:24, y = Spot_platino_proyectados, color = "Línea de Proyección"), linetype = "solid", size = 1) + 
  labs(title = "", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal() +
  scale_color_manual(values = "blue",guide = FALSE)+ 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))


print(Spot_platino_proyeccion)

#Comparación de los precios spot proyectados y los reales para el 2023
Comparación_spot_platino1 = ggplot() + 
  geom_line(aes(x = 1:12, y = Spot_platino_proyectados[1:12], color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:12, y = rev(Spot_platino$Apertura[1:12]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal()+ 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(Comparación_spot_platino1)


#Proyección mensual de futuros a 2 años

Resultados_futuros_platino <- data.frame(Fecha = Fecha, Futuros_platino_proyectados = rep(0, 24))
Futuros_platino_proyectados<- c()

#Proyecciones 2023 y 2024

Futuros_platino_proyectados[1] <- Spot_platino$Apertura[12]*exp(r_spot*(1/12))

for(i in 2:24){
  Futuros_platino_proyectados[i] <- Spot_platino_proyectados[i-1]*exp(r_spot*(1/12))
}

Resultados_futuros_platino$Futuros_platino_proyectados <- Futuros_platino_proyectados


Futuros_platino_proyeccion = ggplot() + 
  geom_line(Resultados_futuros_platino, mapping=aes(x = Fecha, y = Futuros_platino_proyectados, color = "Predicción"), linetype = "solid", linewidth = 1) + 
  labs(title = "", x = "Tiempo", y = "Precio Futuros", color = "") +
  cowplot::theme_cowplot()+theme_minimal() +
  scale_color_manual(values = "blue",guide = FALSE)+ 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(Futuros_platino_proyeccion)


#Comparación de los precios futuros proyectados y los reales en dos años
Comparación_futuros_platino = ggplot() + 
  geom_line(aes(x = 13:36, y = Futuros_platino_proyectados, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:24, y = rev(Futuros_platino$Apertura[1:24]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "", x = "Tiempo", y = "Precio Spot", color = "") +
  cowplot::theme_cowplot() + theme_minimal()+ 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(Comparación_futuros_platino)


#Comparación futuros 2023

Resultados_futuros_platino_2023 <- data.frame(Fecha = rev(Spot_platino$Fecha[1:12]), Futuros_platino_2023 = rep(0, 24))
Futuros_platino_2023 <- c()
for(i in 1:12 ){
  r <- mean(Spot_platino$X..var.[(13-i):(25-i)])
  Futuros_platino_2023[i] <- Spot_platino$Apertura[13-i]*exp(r*(1/12))
}
Resultados_futuros_platino_2023$Futuros_platino_2023 <- Futuros_platino_2023


#Comparación de los precios futuros proyectados y los reales para el 2023

Comparación_futuros_platino2 <- ggplot() +
  geom_line(Resultados_futuros_platino_2023, mapping=aes(x = Fecha, y = Futuros_platino_2023, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(rev(Futuros_platino[1:12,]), mapping=aes(x = Fecha, y = Apertura, color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "", x = "Tiempo", y = "Precio Futuros", color = "") +
  theme_minimal()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(Comparación_futuros_platino2)


#Análisis descriptivo de los datos

#Comparación futuros y spots históricos

Comparación_futuros_spots = ggplot() + 
  geom_line(rev(Spot_platino[13:96,]),mapping = aes(x = Fecha, y = Apertura, color = "Spots"), linetype = "solid", linewidth = 1) +
  geom_line(rev(Futuros_platino[13:96,]), mapping = aes(x = Fecha, y = Apertura , color = "Futuros"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Spots" = "darkblue", "Futuros" = "maroon")) +
  labs(title = "", x = "Tiempo", y = "Precio", color = "") +
  cowplot::theme_cowplot() + theme_minimal()+ 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(Comparación_futuros_spots)

#Mínimos, maximos, moda, media de los precios de apertura y rendimientos de los spots

mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

max_spot <- max(Spot_platino$Apertura[13:96])
min_spot <- min(Spot_platino$Apertura[13:96])
moda_spot <- mode(Spot_platino$Apertura[13:96])
media_spot <-mean(Spot_platino$Apertura[13:96])

max_spot_r <- max(Spot_platino$X..var.[13:96])
min_spot_r <- min(Spot_platino$X..var.[13:96])
moda_spot_r <- mode(Spot_platino$X..var.[13:96])
media_spot_r <-mean(Spot_platino$X..var.[13:96])

#Mínimos, maximos, moda, media de los precios de apertura y rendimientos de los futuros

max_futuros <- max(Futuros_platino$Apertura[13:96])
min_futuros <- min(Futuros_platino$Apertura[13:96])
moda_futuros <- mode(Futuros_platino$Apertura[13:96])
media_futuros <-mean(Futuros_platino$Apertura[13:96])

max_futuros_r <- max(Futuros_platino$X..var.[13:96])
min_futuros_r <- min(Futuros_platino$X..var.[13:96])
moda_futuros_r <- mode(Futuros_platino$X..var.[13:96])
media_futuros_r <-mean(Futuros_platino$X..var.[13:96])

#Distribuciones de los datos del spot
densidad_spots <- ggplot(Spot_platino[c(13:96),], aes(x = Apertura)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "", x = "Precios Spots", y = "Densidad")+
  theme_minimal()+
  scale_x_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))


print(densidad_spots)

#Densidad de los datos de los futuros
densidad_futuros <- ggplot(Futuros_platino[c(13:96),], aes(x = Apertura)) +
  geom_density(fill = "lightblue", color = "black") + 
  labs(title = "", x = "Precios Futuros", y = "Densidad")+
  theme_minimal()+
  scale_x_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE))

print(densidad_futuros)
