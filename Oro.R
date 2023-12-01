library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(stringr)


Fut_oro <- read.csv("Datos históricos Futuros oro.csv", sep = ",")
Spot_oro <- read.csv("Datos históricos XAU_USD.csv", sep = ",")

for(i in 2:5){
  Fut_oro[,i] <- as.numeric(gsub(as.array(",") , "", Fut_oro[,i]))*1000
  Spot_oro[,i] <- as.numeric(gsub(as.array(",") , "", Spot_oro[,i]))*1000
}
Fut_oro$X..var. <- gsub(as.array("%") , "", Fut_oro$X..var.)
Fut_oro$X..var. <- as.numeric(gsub(as.array(",") , ".", Fut_oro$X..var.))/100
Spot_oro$X..var. <- gsub(as.array("%") , "", Spot_oro$X..var.)
Spot_oro$X..var. <- as.numeric(gsub(as.array(",") , ".", Spot_oro$X..var.))/100

r <- mean(Spot_oro$X..var.[12:24])

#Prediccion de Spot
Spot_predecidos <- c()
for(i in 1:24){
  Spot_predecidos <- c(Spot_predecidos, Spot_oro$Apertura[1]*exp(r*(i/12)))
}

Prediccion_Spot = ggplot() + 
  geom_line(aes(x = 1:24, y = Spot_predecidos , color = "Prediccón"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:24, y = Spot_oro$Apertura[1:24] , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Prediccón" = "darkblue", "Real" = "maroon")) +
  labs(title = "Titulon't", x = "Tiempo", y = "¨Precio Spot") +
  cowplot::theme_cowplot()
print(Prediccion_Spot)

#Comparacion con futuros
# r <-  0.17 r de mejor ajuste
Fut_predecidos <- c()
for(i in 1:12){
  r <- mean(Spot_oro$X..var.[(13-i):(25-i)]) #Toma 1 año 
  Fut_predecidos <- c(Fut_predecidos, Spot_oro$Apertura[13-i]*exp(r*(1/12)))
}

Prediccion_Fut = ggplot() + 
  geom_line(aes(x = 1:12, y = Fut_predecidos , color = "Prediccón"), linetype = "solid", linewidth = 1) +
  geom_line(aes(x = 1:12, y = rev(Fut_oro$Apertura[1:12]) , color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Prediccón" = "darkblue", "Real" = "maroon")) +
  labs(title = "Titulon't", x = "Tiempo", y = "¨Precio Spot") +
  cowplot::theme_cowplot()
print(Prediccion_Fut)

