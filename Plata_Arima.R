
Futuros_Oro <- Datos_históricos_Futuros_oro


Futuros_Oro$Vol. <- gsub(",", ".", Futuros_Oro$Vol.)
Futuros_Oro$`% var.` <- gsub(",", ".", Futuros_Oro$`% var.`)

Futuros_Oro$Vol. <- as.numeric(gsub("K", "e3", gsub("M", "e6", Futuros_Oro$Vol.)))
Futuros_Oro$`% var.` <- as.numeric(gsub("%", "", Futuros_Oro$`% var.`))


#--------Spots de Plata con Arima----------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)

Silver_price <- read_csv("Plata_USD.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                                        Vol. = col_skip()))

Silver_price <- Silver_price %>% arrange(Silver_price$Date)

Historico_precios <- ggplot(data = Silver_price, mapping = aes(x = Date, y = Price)) + geom_line()
print(Historico_precios)

History_S <- Silver_price$Price[1:84]

adf.test(History_S)

History_S_p <- ts(History_S) #Convertir ts
Diff_history_S <- diff(History_S_p) #Aplicar primera derivada
adf.test(Diff_history_S)

grafico_acf <- acf(Diff_history_S, main = "ACF")
grafico_pacf <- pacf(Diff_history_S, main = "PACF")

#-----------------------------------------------------------
results_table <- data.frame(p = rep(0,11), q = rep(0,11), AIC = rep(0,11), BIC = rep(0,11))

for (p in 0:10) {
    for (q in 0:10) {
      current_order <- c(p, 1, q)
      current_model <- arima(History_S, order = current_order)
      current_aic <- AIC(current_model)
      current_bic <- BIC(current_model)
      
      # Append the results to the table
      results_table <- rbind(results_table, c(p, q, current_aic, current_bic))
    }
}

# Rename the columns
colnames(results_table) <- c("p", "q", "AIC", "BIC")

# Display the results table
print(results_table)
max(results_table$AIC) #334
max(results_table$BIC) #376
#-----------------------------------------------------------------------


modelo_arima <- Arima(History_S, order = c(7, 1, 8))
#8,1,7 también
prediccion <- forecast(modelo_arima, h=11)
print(prediccion)
plot(prediccion)

Expected <- c(History_S, prediccion$mean)
Silver_price <- cbind(Silver_price[, 1:2], Forecast = c(History_S, prediccion$mean), Silver_price[, 3:ncol(Silver_price)])
Silver_price <- cbind(Silver_price, Error_porcentaje = (Silver_price$Price - Silver_price$Forecast)*100/Silver_price$Price)


Prediccion_completa <- forecast(modelo_arima, h=24)
plot(Prediccion_completa)


Forecast_S <- data.frame("Date" = rep(0, 36), "Modeled_Price" = rep(0, 36), "Modeled_var"=rep(0,36))
Forecast_S$Date <- c(Silver_price$Date[73:95], ymd(c('2023-12-01', '2024-01-01', '2024-02-01', '2024-03-01', '2024-04-01', '2024-05-01', '2024-06-01', '2024-07-01', '2024-08-01', '2024-09-01', '2024-10-01', '2024-11-01', '2024-12-01')))
Forecast_S$Modeled_Price <- as.numeric(c(Silver_price$Price[73:84], Prediccion_completa$mean))
Forecast_S$Modeled_var[1] <- -0.0359
for (i in 2:36) {
  Forecast_S$Modeled_var[i]= round((Forecast_S$Modeled_Price[i]/Forecast_S$Modeled_Price[i-1])-1, digits = 4)
}


G.Spots <- ggplot()+
  geom_line(data = Forecast_S, aes(x=Date, y=Modeled_Price), color="purple")+
  geom_line(data = Silver_price, aes(x=Date, y=Price), color='red')

print(G.Spots)
#------Futuros de Plata con precios de Arima----------------------------------

Futuros_S <- read_csv("Datos históricos Futuros plata.csv", 
                                           col_types = cols(Fecha = col_date(format = "%d.%m.%Y"), 
                                                            Vol. = col_skip(), `% var.` = col_skip()))


Futuros_S$Fecha <- as.Date(Futuros_S$Fecha)
Futuros_S <- Futuros_S %>% arrange(Futuros_S$Fecha)
Futuros_S$Apertura <- as.numeric(Futuros_S$Apertura)/1000
Futuros_S$Máximo <- as.numeric(Futuros_S$Máximo)/1000
Futuros_S$Mínimo <- as.numeric(Futuros_S$Mínimo)/1000
Futuros_S$Último <- as.numeric(Futuros_S$Último)/1000


FFutures_S <- data.frame("Date" = Forecast_S$Date, "Price" = rep(0,36))
FFutures_S$Price[1:12] <- Futuros_S$Último[73:84]
for (i in 13:36) {
  FFutures_S$Price[i] <- Forecast_S$Modeled_Price[i]*exp(mean(Forecast_S$Modeled_var[(i-12):(i-1)])*(1/12))
}

print(ggplot(data = Futuros_S, mapping = aes(x=Fecha, y=Último)) + geom_point())
G.Futures.S <- ggplot() +
  geom_line(data=Futuros_S, aes(x=Fecha, y=Último), color='purple') + 
  geom_line(data=FFutures_S, aes(x=Date, y=Price), color='red')
print(G.Futures.S)


