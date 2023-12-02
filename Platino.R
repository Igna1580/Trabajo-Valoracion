library(readxl)

Futuros_platino<- read.csv("Datos históricos Futuros platino.csv",  sep = ",")
Spot_platino <- read.csv("Datos históricos XPT_USD.csv",  sep = ",")

Futuros_platino[2:5] <- as.data.frame(lapply(Futuros_platino[2:5], function(x) as.numeric(gsub(",", "\\.", gsub("\\.", "", x)))))
Spot_platino[2:5] <- as.data.frame(lapply(Spot_platino[2:5], function(x) as.numeric(gsub(",", "\\.", gsub("\\.", "", x)))))


Futuros_platino$X..var. <- gsub(as.array("%") , "", Futuros_platino$X..var.)
Futuros_platino$X..var. <- as.numeric(gsub(as.array(",") , ".", Futuros_platino$X..var.))/100
Spot_platino$X..var. <- gsub(as.array("%") , "", Spot_platino$X..var.)
Spot_platino$X..var. <- as.numeric(gsub(as.array(",") , ".", Spot_platino$X..var.))/100