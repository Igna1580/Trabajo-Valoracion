

#----Librerias--------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(tseries)
library(forecast)

#--------Datos Spot------------------------------------

#Plata
Spot_Plata <- read_csv("XAG_USD Historical Data.csv", 
                       col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                        Price = col_number(), Open = col_number(), 
                                        High = col_number(), Low = col_number(), 
                                        Vol. = col_skip(), `Change %` = col_number()))
Spot_Plata <- Spot_Plata %>% arrange(Spot_Plata$Date)
Spot_Plata$`Change %` <- (Spot_Plata$`Change %`)/100

#Bitcoin
Spot_Bitcoin <- read_csv("Bitcoin Historical Data.csv", 
                         col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                          Price = col_number(), Open = col_number(), 
                                          High = col_number(), Low = col_number(), 
                                          Vol. = col_skip(), `Change %` = col_number()))
Spot_Bitcoin <- Spot_Bitcoin %>% arrange(Spot_Bitcoin$Date)
Spot_Bitcoin$`Change %` <- (Spot_Bitcoin$`Change %`)/100


#Platino
Spot_Platino <- read_csv("XPT_USD Historical Data.csv", 
                         col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                          Price = col_number(), Open = col_number(), 
                                          High = col_number(), Low = col_number(), 
                                          Vol. = col_skip(), `Change %` = col_number()))
Spot_Platino <- Spot_Platino %>% arrange(Spot_Platino$Date)
Spot_Platino$`Change %` <- (Spot_Platino$`Change %`)/100


#-------Prueba ADF----------------------------------

adf.test(Spot_Plata$Price) #0.48 (no estacionario)
adf.test(Spot_Bitcoin$Price[1:84])#0.48 (no estacionario)
adf.test(Spot_Platino$Price) #0.22 (no estacionario)

#------Prueba ADF a primera derivada-----------------

Spot_Plata_dif <- diff(ts(Spot_Plata$Price))
Spot_Bitcoin_dif <- diff(ts(Spot_Bitcoin$Price[1:84]))
Spot_Platino_dif <- diff(ts(Spot_Platino$Price))

adf.test(Spot_Bitcoin_dif) # <0.01
adf.test(Spot_Plata_dif) # <0.01
adf.test(Spot_Platino_dif) # <0.01

#Se utiliza arima(p,1,q)


#-------Escoger p y q-----------------------------------

indicators_Plata <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric())
indicators_Bitcoin <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric())
indicators_Platino <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric())

results_Plata <- data.frame(Date = Spot_Plata$Date[85:96], M_results = rep(0, 12), Real = Spot_Plata$Price[85:96])
results_Bitcoin <- data.frame(Date = Spot_Bitcoin$Date[85:96], M_results = rep(0, 12), Real = Spot_Bitcoin$Price[85:96])
results_Platino <- data.frame(Date = Spot_Platino$Date[85:96], M_results = rep(0, 12), Real = Spot_Platino$Price[85:96])

error_Plata <- 1
error_Bitcoin <- 1
error_Platino <- 1
best_order_Plata <- c(0,0,0)
best_order_Platino <- c(0,0,0)
best_order_Bitcoin <- c(0,0,0)


i<- 0
for (p in 1:10) {
  for (q in 1:10) {
    i <- i+1
    current_order <- c(p, 1, q)
    current_model <- arima(Spot_Plata$Price[1:84], order = current_order)
    current_aic <- AIC(current_model)
    current_bic <- BIC(current_model)
    
    indicators_Plata[i,] <- c(p, q, current_aic, current_bic)

    results_Plata$M_results <- forecast(current_model, h=12)$mean
    
    error_medio <- mean(((results_Plata$Real) - (results_Plata$M_results))/results_Plata$Real)
    
    if(error_medio<error_Plata){
      error_Plata <- error_medio
      best_order_Plata<-current_order
    }
  }
}

i <- 0
for (p in 1:5) {
  for (q in 1:5) {
    i <- i+1
    current_order <- c(p, 2, q)
    current_model <- arima(Spot_Bitcoin$Price[1:84], order = current_order)
    current_aic <- AIC(current_model)
    current_bic <- BIC(current_model)
    
    indicators_Bitcoin[i,] <- c(p, q, current_aic, current_bic)
    
    results_Bitcoin$M_results <- forecast(current_model, h=12)$mean
    
    error_medio <- mean(((results_Bitcoin$Real) - (results_Bitcoin$M_results))/results_Bitcoin$Real)
    
    if(error_medio<error_Bitcoin){
      error_Bitcoin <- error_medio
      best_order_Bitcoin<-current_order
    }
  }
}

i<-0
for (p in 1:5) {
  for (q in 1:5) {
    i <- i+1
    current_order <- c(p, 1, q)
    current_model <- arima(Spot_Platino$Price[1:84], order = current_order)
    current_aic <- AIC(current_model)
    current_bic <- BIC(current_model)
    
    indicators_Platino[i,] <- c(p, q, current_aic, current_bic)
    
    results_Platino$M_results <- forecast(current_model, h=12)$mean
    
    error_medio <- mean(((results_Platino$Real) - (results_Platino$M_results))/results_Platino$Real)
    
    if(error_medio<error_Platino){
      error_Platino <- error_medio
      best_order_Platino<-current_order
    }
  }
}

#-------Hacer Predicción----------------------------------------------------

Forecast_Plata <- data.frame(Dates = rep(0,24), Expected_Spot = rep(0,24), Change= rep(0,24))
Forecast_Bitcoin <- data.frame(Dates = rep(0,24), Expected_Spot = rep(0,24), Change= rep(0,24))
Forecast_Platino <- data.frame(Dates = rep(0,24), Expected_Spot = rep(0,24), Change= rep(0,24))

Forecast_Plata$Dates <- c(Spot_Plata$Date[85:96], ymd(c('2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))
Forecast_Bitcoin$Dates <- c(Spot_Bitcoin$Date[85:96], ymd(c('2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))
Forecast_Platino$Dates <- c(Spot_Platino$Date[85:96], ymd(c('2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))

Forecast_Plata$Expected_Spot <- forecast(arima(Spot_Plata$Price[1:84], order = best_order_Plata), h=24)$mean
Forecast_Bitcoin$Expected_Spot <- forecast(arima(Spot_Bitcoin$Price[1:84], order = best_order_Bitcoin), h=24)$mean
Forecast_Platino$Expected_Spot <- forecast(arima(Spot_Platino$Price[1:84], order = best_order_Platino), h=24)$mean


#--------Gráficos----------------------------------------------------------

#Modelo con error medio minimo

Forecast_Spot_Plata <- ggplot() +
  geom_line(data = Forecast_Plata, aes(x = Dates, y = Expected_Spot, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Spot_Plata[73:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de Plata", x = "Tiempo", y = "Precio del Futuro") +
  theme_minimal()
print(Forecast_Spot_Plata)

Forecast_Spot_Bitcoin <- ggplot() +
  geom_line(data = Forecast_Bitcoin, aes(x = Dates, y = Expected_Spot, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Spot_Bitcoin[73:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de Bitcoin", x = "Tiempo", y = "Precio del Futuro") +
  theme_minimal()
print(Forecast_Spot_Bitcoin)

Forecast_Spot_Platino <- ggplot() +
  geom_line(data = Forecast_Platino, aes(x = Dates, y = Expected_Spot, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Spot_Platino[73:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de Platino", x = "Tiempo", y = "Precio del Futuro") +
  theme_minimal()
print(Forecast_Spot_Platino)

#------Prediccion con mejor ajuste a criterio propio-----------------------------------

Forecast_Plata$Expected_Spot <- forecast(arima(Spot_Plata$Price[1:84], order = c(8,1,7)), h=24)$mean
Forecast_Bitcoin$Expected_Spot <- forecast(arima(Spot_Bitcoin$Price[1:84], order = c(5,2,5)), h=24)$mean
Forecast_Platino$Expected_Spot <- forecast(arima(Spot_Platino$Price[1:84], order = c(7,1,5)), h=24)$mean

for (i in 1:24){
  if(i==1){
    Forecast_Plata$Change[1] <- (Forecast_Plata$Expected_Spot[i]-Spot_Plata$Price[96])/Spot_Plata$Price[96]
    Forecast_Bitcoin$Change[1] <- (Forecast_Bitcoin$Expected_Spot[i]-Spot_Bitcoin$Price[96])/Spot_Bitcoin$Price[96]
    Forecast_Platino$Change[1] <- (Forecast_Platino$Expected_Spot[i]-Spot_Platino$Price[96])/Spot_Platino$Price[96]
  }else{
    Forecast_Plata$Change[i] <- (Forecast_Plata$Expected_Spot[i]-Forecast_Plata$Expected_Spot[i-1])/Forecast_Plata$Expected_Spot[i-1]
    Forecast_Bitcoin$Change[i] <- (Forecast_Bitcoin$Expected_Spot[i]-Forecast_Bitcoin$Expected_Spot[i-1])/Forecast_Bitcoin$Expected_Spot[i-1]
    Forecast_Platino$Change[i] <- (Forecast_Platino$Expected_Spot[i]-Forecast_Platino$Expected_Spot[i-1])/Forecast_Platino$Expected_Spot[i-1]
  }
}


Forecast_Spot_Plata <- ggplot() +
  geom_line(data = Forecast_Plata, aes(x = Dates, y = Expected_Spot, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Spot_Plata[73:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de Plata", x = "Tiempo", y = "Precio Spot($)") +
  theme_minimal()
print(Forecast_Spot_Plata)

Forecast_Spot_Bitcoin <- ggplot() +
  geom_line(data = Forecast_Bitcoin, aes(x = Dates, y = Expected_Spot, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Spot_Bitcoin[73:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de Bitcoin", x = "Tiempo", y = "Precio Spot($)") +
  theme_minimal()
print(Forecast_Spot_Bitcoin)

Forecast_Spot_Platino <- ggplot() +
  geom_line(data = Forecast_Platino, aes(x = Dates, y = Expected_Spot, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Spot_Platino[73:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios Spot de Platino", x = "Tiempo", y = "Precio Spot($)") +
  theme_minimal()
print(Forecast_Spot_Platino)



#----------Prediccion Futuros--------------------------------------------------

Var_Plata <- c(Spot_Plata$`Change %`[73:84], Forecast_Plata$Change)
Var_Bitcoin <- c(Spot_Bitcoin$`Change %`[73:84], Forecast_Bitcoin$Change)
Var_Platino <- c(Spot_Platino$`Change %`[73:84], Forecast_Platino$Change)


Proyeccion_Plata <- data.frame(Dates = rep(0,24), Expected_Future = rep(0,24))
Proyeccion_Bitcoin <- data.frame(Dates = rep(0,24), Expected_Future = rep(0,24))
Proyeccion_Platino <- data.frame(Dates = rep(0,24), Expected_Future = rep(0,24))

Proyeccion_Plata$Dates <- c(Spot_Plata$Date[85:96], ymd(c('2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))
Proyeccion_Bitcoin$Dates <- c(Spot_Bitcoin$Date[85:96], ymd(c('2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))
Proyeccion_Platino$Dates <- c(Spot_Platino$Date[85:96], ymd(c('2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))


for (i in 1:24){
  Proyeccion_Plata$Expected_Future[i] <- Forecast_Plata$Expected_Spot[i]*exp((mean(Var_Plata[i:i+11]))*(1/12))
  Proyeccion_Bitcoin$Expected_Future[i] <- Forecast_Bitcoin$Expected_Spot[i]*exp((mean(Var_Bitcoin[i:i+11]))*(1/12))
  Proyeccion_Platino$Expected_Future[i] <- Forecast_Platino$Expected_Spot[i]*exp((mean(Var_Platino[i:i+11]))*(1/12))
}

#Datos Futuros Reales
Futuros_Plata <- read_csv("Silver Futures Historical Data.csv", 
                          col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                           Price = col_number(), Open = col_number(), 
                                           High = col_number(), Low = col_number(), 
                                           Vol. = col_skip(), `Change %` = col_number()))
Futuros_Bitcoin <- read_csv("Bitcoin Futures CME Historical Data.csv", 
                            col_types = cols(Price = col_number(), 
                                             Open = col_number(), High = col_number(), 
                                             Low = col_number(), Vol. = col_skip(), 
                                             `Change %` = col_number()))
Futuros_Bitcoin$Date <- Futuros_Plata$Date[1:82]
Futuros_Plata <- Futuros_Plata %>% arrange(Futuros_Plata$Date)
Futuros_Bitcoin <- Futuros_Bitcoin %>% arrange(Futuros_Bitcoin$Date)

Futuros_Platino <- read_csv("Platinum Futures Historical Data.csv", 
                            col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                             Price = col_number(), Open = col_number(), 
                                             High = col_number(), Low = col_number(), 
                                             Vol. = col_skip(), `Change %` = col_number()))
Futuros_Platino <- Futuros_Platino %>% arrange(Futuros_Platino$Date)


#Graficos

Forecast_Futuros_Plata <- ggplot() +
  geom_line(data = Proyeccion_Plata, aes(x = Dates, y = Expected_Future, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Futuros_Plata[61:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios de Futuros de Plata", x = "Tiempo", y = "Precio del Futuro($)") +
  theme_minimal()
print(Forecast_Futuros_Plata)

Forecast_Futuros_Bitcoin <- ggplot() +
  geom_line(data = Proyeccion_Bitcoin, aes(x = Dates, y = Expected_Future, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Futuros_Bitcoin[47:82,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios de Futuros de Bitcoin", x = "Tiempo", y = "Precio del Futuro($)") +
  theme_minimal()
print(Forecast_Futuros_Bitcoin)

Forecast_Futuros_Platino <- ggplot() +
  geom_line(data = Proyeccion_Platino, aes(x = Dates, y = Expected_Future, color = "Predicción"), linetype = "solid", size = 1) +
  geom_line(data = Futuros_Platino[61:96,], aes(x = Date, y = Price, color = "Real"), linetype = "solid", size = 1) +
  scale_color_manual(values = c("Predicción" = "darkblue", "Real" = "maroon")) +
  labs(title = "Precios deFuturos de Platino", x = "Tiempo", y = "Precio del Futuro($)") +
  theme_minimal()
print(Forecast_Futuros_Platino)


