#---------Librerías------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

#--------Lectura de Datos------------------------------------------

Spot_Plata <- read_csv("XAG_USD Historical Data.csv", 
                       col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                        Price = col_number(), Open = col_number(), 
                                        High = col_number(), Low = col_number(), 
                                        Vol. = col_skip(), `Change %` = col_number()))
Spot_Plata <- Spot_Plata %>% arrange(Spot_Plata$Date)
Spot_Plata$`Change %` <- (Spot_Plata$`Change %`)/100

Futuros_Plata <- read_csv("Silver Futures Historical Data.csv", 
                          col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                           Price = col_number(), Open = col_number(), 
                                           High = col_number(), Low = col_number(), 
                                           Vol. = col_skip(), `Change %` = col_number()))
Futuros_Plata <- Futuros_Plata %>% arrange(Futuros_Plata$Date)
Futuros_Plata$`Change %` <- (Futuros_Plata$`Change %`)/100

#---------Datos previos-----------------------------------------

mean(Spot_Plata$Price) #19.65
mean(Futuros_Plata$Price) #20.33
max(Spot_Plata$Price) #28.22
max(Futuros_Plata$Price) #29.74
min(Spot_Plata$Price) #13.97
min(Futuros_Plata$Price) #14.18


mean(Spot_Plata$`Change %`) #0.0095
mean(Futuros_Plata$`Change %`) #0.0097
max(Spot_Plata$`Change %`) #0.3442
max(Futuros_Plata$`Change %`) #0.3046
min(Spot_Plata$`Change %`) #-0.1775
min(Futuros_Plata$`Change %`) #-0.1731

#---------Poryeccion Spot Price-----------------------------------

r <- mean(Spot_Plata$`Change %`[73:84])
Faltantes <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", "2024-06-01", "2024-07-01", 
                       "2024-08-01", "2024-09-01", "2024-10-01", "2024-11-01", "2024-12-01"))
Fecha <- c(Spot_Plata$Date[85:96], Faltantes)
Resultados_Spot <- data.frame(Fecha = Fecha, Exp.Price = rep(0, 24))

Exp.Price <- c()
for(i in 1:24){
    Exp.Price[i] <- Spot_Plata$Price[84]*exp(r*(i/12))
}

Resultados_Spot$Exp.Price <- Exp.Price


Prediccion_Spot <- ggplot() +
  geom_line(Resultados_Spot, mapping=aes(x = Fecha, y = Exp.Price, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(Spot_Plata[73:96,], mapping=aes(x = Date, y = Price, color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de la Plata", x = "Tiempo", y = "Precio Spot") +
  theme_minimal()
print(Prediccion_Spot)


#--------Proyeccion Futuros---------------------------------------------

Resultados_Futures <- data.frame(Fecha = Fecha, Exp.Future.P = rep(0, 24))

for (i in 1:24){
    if(i<13){
      r <- mean(Spot_Plata$`Change %`[(71+i):(83+i)])
      Resultados_Futures$Exp.Future.P[i] <- Spot_Plata$Price[84+i]*exp(r*(1/12))
    }else{
      r <- Spot_Plata$`Change %`[96]
      Resultados_Futures$Exp.Future.P[i] <- Resultados_Spot$Exp.Price[i]*exp(r*(1/12))
    }
  }



Prediccion_Futuros <- ggplot() +
  geom_line(Resultados_Futures[1:12,], mapping=aes(x = Fecha, y = Exp.Future.P, color = "Predicción"), linetype = "solid", linewidth = 1) +
  geom_line(Futuros_Plata[85:96,], mapping=aes(x = Date, y = Price, color = "Real"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios de los Futuros de Plata", x = "Tiempo", y = "Precio Spot") +
  theme_minimal()
print(Prediccion_Futuros)
