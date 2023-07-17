## Make OD data
## Library
library(dplyr)
library(tidyr)
## Data
OD.all <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_all.csv", header = T, stringsAsFactors = FALSE)
OD.airline <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_airline.csv", header = T, stringsAsFactors = FALSE)
OD.railway <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_railway.csv", header = T, stringsAsFactors = FALSE)
OD.ship <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_ship.csv", header = T, stringsAsFactors = FALSE)
OD.bus <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_bus.csv", header = T, stringsAsFactors = FALSE)
OD.car <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_car.csv", header = T, stringsAsFactors = FALSE)
OD.corres <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD Data/OD_corres.csv", header = T, stringsAsFactors = FALSE)

OD.all <- OD.all %>% 
  tidyr::gather(key = j, value = all, -X)
OD.airline <- OD.airline %>% 
  tidyr::gather(key = j, value = airline, -X)
OD.railway <- OD.railway %>% 
  tidyr::gather(key = j, value = railway, -X)
OD.ship <- OD.ship %>% 
  tidyr::gather(key = j, value = ship, -X)
OD.bus <- OD.bus %>% 
  tidyr::gather(key = j, value = bus, -X)
OD.car <- OD.car %>% 
  tidyr::gather(key = j, value = car, -X)

OD.all <- cbind(OD.all, OD.airline$airline, OD.railway$railway, OD.ship$ship, OD.bus$bus, OD.car$car)
colnames(OD.all) <- c("ODi", "ODj", "all", "airline", "railway", "ship", "bus", "car")

OD.all$all <- as.numeric(gsub("|", "0", OD.all$all))
OD.all$airline <- as.numeric(gsub("|", "0", OD.all$airline))
OD.all$railway <- as.numeric(gsub("|", "0", OD.all$railway))
OD.all$ship <- as.numeric(gsub("|", "0", OD.all$ship))
OD.all$bus <- as.numeric(gsub("|", "0", OD.all$bus))
OD.all$car <- as.numeric(gsub("|", "0", OD.all$car))

OD.all[is.na(OD.all)] <- 0

OD.all <- OD.all[ , -3]
OD.all <- OD.all %>% 
  dplyr::mutate(all = airline + railway + ship + bus + car, 
                all2 = airline + railway)

OD.corres <- OD.corres %>% 
  tidyr::gather(key = j, value = x, -X)
OD.corres$j <- as.numeric(gsub("X", "", OD.corres$j))
colnames(OD.corres) <- c("i", "j", "x")

OD.all <- cbind(OD.corres[ , c(1, 2)], OD.all)

colnames(OD.all)[1] <- "207ƒ][ƒ“i"
colnames(OD.all)[2] <- "207ƒ][ƒ“j"
write.csv(OD.all, "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/207OD.csv")