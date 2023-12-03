library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(stringr)
library(scales)


Fut_bitcoin <- read.csv("Datos históricos Futuros Bitcoin CME.csv", sep = ",")
Spot_bitcoin <- read.csv("Datos históricos del Bitcoin.csv", sep = ",")

for(i in 2:5){
  Fut_bitcoin[,i] <- as.numeric(gsub(as.array(",") , "", Fut_bitcoin[,i]))*1000
  Spot_bitcoin[,i] <- as.numeric(gsub(as.array(",") , "", Spot_bitcoin[,i]))*1000
}
Fut_bitcoin$X..var. <- gsub(as.array("%") , "", Fut_bitcoin$X..var.)
Fut_bitcoin$X..var. <- as.numeric(gsub(as.array(",") , ".", Fut_bitcoin$X..var.))/100
Spot_bitcoin$X..var. <- gsub(as.array("%") , "", Spot_bitcoin$X..var.)
Spot_bitcoin$X..var. <- as.numeric(gsub(as.array(",") , ".", Spot_bitcoin$X..var.))/100

r <- mean(Spot_bitcoin$X..var.[13:24])

#Prediccion de Spot
Spot_predecidos <- c()
for(i in 1:24){
  Spot_predecidos <- c(Spot_predecidos, Spot_bitcoin$Apertura[12]*exp(r*(i/12)))
}

Prediccion_Spot = ggplot() + 
  geom_line(aes(x = 1:24, y = Spot_predecidos , color = "Prediccón"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:12, y = rev(Spot_bitcoin$Apertura[1:12]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Prediccón" = "darkblue", "Real" = "maroon")) +
  labs(title = "Titulon't", x = "Tiempo", y = "Precio Spot") +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
  cowplot::theme_cowplot()
print(Prediccion_Spot)

Proyeccion_Spot = ggplot() + 
  geom_line(aes(x = 37:60, y = Spot_predecidos , color = "Prediccón"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:48, y = rev(Spot_bitcoin$Apertura[1:48]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Prediccón" = "darkblue", "Real" = "maroon")) +
  labs(title = "Titulon't", x = "Tiempo", y = "Precio Spot") +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
  cowplot::theme_cowplot()
print(Proyeccion_Spot)

#Comparacion con futuros
# r <-  0.17 r de mejor ajuste
Fut_predecidos <- c()
for(i in 1:12){
  r <- mean(Spot_bitcoin$X..var.[(13-i):(25-i)]) #Toma 1 año 
  Fut_predecidos <- c(Fut_predecidos, Spot_bitcoin$Apertura[13-i]*exp(r*(1/12)))
}

Prediccion_Fut = ggplot() + 
  geom_line(aes(x = 1:12, y = Fut_predecidos , color = "Prediccón"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:12, y = rev(Fut_bitcoin$Apertura[1:12]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Prediccón" = "darkblue", "Real" = "maroon")) +
  labs(title = "Titulon't", x = "Tiempo", y = "Precio Spot") +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
  cowplot::theme_cowplot()
print(Prediccion_Fut)

