## Library
library(magrittr)
library(tidyr)

## Read data
airline2 <- read.csv("C:/Users/kayab/yuki/åwu`/åw@[~/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
OD <- read.csv("C:/Users/kayab/yuki/åwu`/åw@[~/Data/OD.csv", header = T, stringsAsFactors = FALSE)

## Transform characters into numeric
airline2$^qñ <- as.numeric(gsub(",", "", airline2$^qñ)) # gsub is used to remove commas in each figure
airline2$^qL[g <- as.numeric(gsub(",", "", airline2$^qL[g))
airline2$·q <- as.numeric(gsub(",", "", airline2$·q))
airline2$ÀÈ <- as.numeric(gsub(",", "", airline2$ÀÈ))

## Some variables necessary to analyse data
airline2 <- airline2 %>% 
  dplyr::mutate(DIS = ^qL[g/^qñ,
                Seat = ÀÈ/^qñ,
                Freq = ·q/(Seat*ÀÈp¦/100), 
                MC = 11*DIS) # I have to revise this! Unit cost should be obtained from annual reports
airline2[which(airline2$qóïÐ == "SFJ"), 26] <- 9*DIS ## SFJÍjbgRXgª9
airline2[which(airline2$qóïÐ == "SKY"), 26] <- 5.5*DIS ## SKYÍjbgRXgª5.5

## Concentrate on Tokyo, Osaka, and Fukuoka
ODnames <- c("", "¬c", "Ö¼", "åã", "_Ë", "ìIl", "ª", "kãB", "²ê")
ODNo <- c(rep(18, 2), rep(25, 4), rep(37, 3))
ODcorrespondence <- data.frame(ODnames = ODnames, ODNo = ODNo)
airline3 <- airline2 %>% 
  dplyr::filter(ó`i == "" | ó`i == "¬c" | ó`i == "Ö¼" | ó`i == "åã" | ó`i == "_Ë" | ó`i == "ìIl" | ó`i == "ª" | ó`i == "kãB" | ó`i == "²ê") %>% 
  dplyr::filter(ó`j == "" | ó`j == "¬c" | ó`j == "Ö¼" | ó`j == "åã" | ó`j == "_Ë" | ó`j == "ìIl" | ó`j == "ª" | ó`j == "kãB" | ó`j == "²ê")
for(k in 1:2){
  for(i in 1:nrow(airline3)){
    for(j in 1:nrow(ODcorrespondence)){
      if(airline3[i, k] == ODcorrespondence[j, 1]){
        airline3[i, k + 26] <- ODcorrespondence[j, 2]
      } else {
        
      }
    }
  }
}

## Read data
airline4 <- read.csv("C:/Users/kayab/yuki/åwu`/åw@[~/Data/airline.csv", header = T)

## Replace airports name with correct ones
airline4$ó`i <- as.character(airline4$ó`i)
airline4$ó`j <- as.character(airline4$ó`j)
airline4$ó`i <- gsub("Hc", "", airline4$ó`i)
airline4$ó`j <- gsub("Hc", "", airline4$ó`j)
airline4$ó`i <- gsub("ÉO", "åã", airline4$ó`i)
airline4$ó`j <- gsub("ÉO", "åã", airline4$ó`j)
airline4$ó`i <- gsub("Ö¼Û", "Ö¼", airline4$ó`i)
airline4$ó`j <- gsub("Ö¼Û", "Ö¼", airline4$ó`j)

## Filter data
airline4 <- airline4 %>% 
  dplyr::filter(ó`i == "" | ó`i == "¬c" | ó`i == "Ö¼" | ó`i == "åã" | ó`i == "_Ë" | ó`i == "ìIl" | ó`i == "ª" | ó`i == "kãB" | ó`i == "²ê") %>% 
  dplyr::filter(ó`j == "" | ó`j == "¬c" | ó`j == "Ö¼" | ó`j == "åã" | ó`j == "_Ë" | ó`j == "ìIl" | ó`j == "ª" | ó`j == "kãB" | ó`j == "²ê")

## Attach airline data to airline3
airline3$ó`i <- as.character(airline3$ó`i)
airline3$ó`j <- as.character(airline3$ó`j)
airline3$qóïÐ <- as.character(airline3$qóïÐ)
airline4$ó`i <- as.character(airline4$ó`i)
airline4$ó`j <- as.character(airline4$ó`j)
airline4$qóïÐ <- as.character(airline4$qóïÐ)

write.csv(airline4, "C:/Users/kayab/yuki/åwu`/åw@[~/Data/airline4.csv")

for(k in 1:2){
  for(i in 1:nrow(airline4)){
    for(j in 1:nrow(ODcorrespondence)){
      if(airline4[i, k + 3] == ODcorrespondence[j, 1]){
        airline4[i, k + 19] <- ODcorrespondence[j, 2]
      } else {
        
      }
    }
  }
}

airlinenames <- c("JAL", "ANA", "SFJ", "SKY", "IBX", "JAC", "WFCEGA", "AMX")
airlineNo <- c(seq(1:8))
airlinecorrespondence <- data.frame(airlinenames = airlinenames, airlineNo = airlineNo)
airlinecorrespondence$airlinenames <- as.character(airlinecorrespondence$airlinenames)


for(i in 1:nrow(airline3)){
  for(j in 1:nrow(airlinecorrespondence)){
    if(airline3[i, 3] == airlinecorrespondence[j, 1]){
      airline3[i, 29] <- airlinecorrespondence[j, 2]
    } else {
      
    }
  }
}
for(i in 1:nrow(airline4)){
  for(j in 1:nrow(airlinecorrespondence)){
    if(airline4[i, 8] == airlinecorrespondence[j, 1]){
      airline4[i, 22] <- airlinecorrespondence[j, 2]
    } else {
      
    }
  }
}
for(i in 1:nrow(airline3)){
  for(j in 1:nrow(airline4)){
    if(airline3[i, 1] == airline4[j, 4] && airline3[i, 2] == airline4[j, 5] && airline3[i, 29] == airline4[j, 22]){
      (airline3[i, 30] <- airline4[j, 9]) & (airline3[i, 31] <- airline4[j, 10]) & (airline3[i, 32] <- airline4[j, 12]) & (airline3[i, 33] <- airline4[j, 13]) & (airline3[i, 34] <- airline4[j, 15])
    } else {
      
    }
  }
}
colnames(airline3)[27] <- "i"
colnames(airline3)[28] <- "j"
colnames(airline3)[29] <- "airlineNo"
colnames(airline3)[30] <- "vÔ"
colnames(airline3)[31] <- "ïp"
colnames(airline3)[32] <- "qóÖ"
colnames(airline3)[33] <- "ØÝÂ\Ô"
colnames(airline3)[34] <- "GACQü"

## Drop NAs 
airline3 <- airline3 %>% 
  dplyr::filter(!is.na(GACQü)) %>%  # GACQü has no particular meaning! it is just appropriate to remove NAs 
  dplyr::filter(!is.na(Freq)) %>% 
  dplyr::arrange(i, j) %>%  
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(ODairline = sum(·q))

## railway data
OD <- OD %>% 
  dplyr::filter(X == 18 | X == 25 | X == 37) %>% 
  dplyr::filter(X.1 == 18 | X.1 == 25 | X.1 == 37) %>% 
  dplyr::filter(X != X.1)
OD <- OD[ , c(1, 2, 3, 4, 6)]
OD$railway <- OD$railway*1000

## attach railway demand data to airline2
for(i in 1:nrow(airline3)){
  for(j in 1:nrow(OD)){
    if(airline3[i, 27]==OD[j, 1] && airline3[i, 28]==OD[j, 2]){
      airline3[i, 36] <- OD[j, 5]
    }
  }
}
colnames(airline3)[36] <- "ODrailway"
## Derive ODs
airline3 <- airline3 %>% 
  dplyr::mutate(OD = ODairline + ODrailway)

## Add accessibility
accessibility <- read.csv("C:/Users/kayab/yuki/åwu`/åw@[~/Data/accessibility.csv", header = T)
accessibility$ANZVreB <- accessibility$ANZVreB/1000
accessibility$w.ó` <- as.character(accessibility$w.ó`)
for(i in 1:nrow(airline3)){
  for(j in 1:nrow(accessibility)){
    if(airline3[i, 1] == accessibility[j, 2] && accessibility[j, 3] == 0){
      airline3[i, 38] <- accessibility[j, 4]
    } else {
      
    }
  }
}
colnames(airline3)[38] <- "ANZVreB"

## Write csv
write.csv(airline3, "C:/Users/kayab/yuki/åwu`/åw@[~/Data/airline3.csv")

## Introduce a new data frame
data2 <- airline3[ , c(27, 28, 1, 2, 3, 7, 30, 31, 32, 33, 38, 34)]
data2 <- data2 %>% 
  dplyr::mutate(S¹_~[ = 0)
## Read data
data2railway <- read.csv("C:/Users/kayab/yuki/åwu`/åw@[~/Data/data2railway.csv", header = T, stringsAsFactors = FALSE)
data2railway <- as.list(data2railway)
data2 <- rbind(data2, data2railway)
for(i in 1:nrow(data2)){
  for(j in 1:nrow(OD)){
    if(data2[i, 1] == OD[j, 1] && data2[i, 2] == OD[j, 2] && data2[i, 13] == 1){
      data2[i, 6] <- OD[j, 5]
    }
  }
}
data2 <- data2 %>% 
  dplyr::arrange(i, j)

## Add OD
data2 <- data2 %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(OD = sum(·q)) %>% 
  dplyr::ungroup() 


## Fitted values of utility
utilitypar <- logit$coefficients
data2 <- data2 %>% 
  dplyr::mutate(utility = utilitypar[1] + vÔ*utilitypar[2] + ïp*utilitypar[3] + log(qóÖ)*utilitypar[4] + 
                  ØÝÂ\Ô*utilitypar[5] + ANZVreB*utilitypar[6] * S¹_~[*utilitypar[7] + 
                  GACQü*utilitypar[8]) %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(sumexputility = sum(exp(utility))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(fittedprob = exp(utility)/sumexputility) %>% 
  dplyr::mutate(observedprob = ·q/OD)

## Add LF, Freq, DIS, and MC
for(i in 1:nrow(data2)){
  for(j in 1:nrow(airline3)){
    if(airline3[j, 1] == data2[i, 3] && airline3[j, 2] == data2[i, 4] && airline3[j, 3] == data2[i, 5]){
      (data2[i, 19] <- airline3[j, 23]) & (data2[i, 20] <- airline3[j, 24]) & (data2[i, 21] <- airline3[j, 4]) & (data2[i, 22] <- airline3[j, 26]) & (data2[i, 23] <- airline3[j, 12]) 
    } else {
      
    }
  }
}
colnames(data2)[19] <- "DIS"
colnames(data2)[20] <- "Seat"
colnames(data2)[21] <- "Freq"
colnames(data2)[22] <- "MC"
colnames(data2)[23] <- "LF"

write.csv(data2, "C:/Users/kayab/yuki/åwu`/åw@[~/Data/data2.csv")

