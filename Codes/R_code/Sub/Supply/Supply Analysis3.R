## Supply Analysis 3
## Preparing for counterfactual simulations

## Library
library(magrittr)
library(tidyr)

## Read data
airline.updated <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
OD <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/OD.csv", header = T, stringsAsFactors = FALSE)

## Transform characters into numeric
airline.updated$‰^q‰ñ” <- as.numeric(gsub(",", "", airline.updated$‰^q‰ñ”)) # gsub is used to remove commas in each figure
airline.updated$‰^qƒLƒƒ[ƒgƒ‹ <- as.numeric(gsub(",", "", airline.updated$‰^qƒLƒƒ[ƒgƒ‹))
airline.updated$—·‹q” <- as.numeric(gsub(",", "", airline.updated$—·‹q”))
airline.updated$ÀÈ” <- as.numeric(gsub(",", "", airline.updated$ÀÈ”))
airline.updated$‰İ•¨ <- as.numeric(gsub(",", "", airline.updated$‰İ•¨))
## Some variables necessary to analyse data
airline.updated <- airline.updated %>% 
  dplyr::mutate(DIS = ‰^qƒLƒƒ[ƒgƒ‹/‰^q‰ñ”,
                Seat = ÀÈ”/‰^q‰ñ”,
                Freq = —·‹q”/(Seat*ÀÈ—˜—p—¦/100), 
                MC = 11*DIS) # I have to revise this! Unit cost should be obtained from annual reports

## q‹ó‰ïĞ‚²‚Æ‚ÉŒÀŠE”ï—p‚ğ’²®‚·‚é
## ‚¿‚á‚ñ‚Æ‘S•”‚â‚é
## ‚Æ‚è‚ ‚¦‚¸Š²ü‚ÍˆÈ‰º
## JAL, ANA, JTA, JAC, IBX, HAC = 11
## ADO, SFJ = 9
## SKY = 5.5
airline.updated[which(airline.updated$q‹ó‰ïĞ == "SFJ"), 26] <- (9/11)*airline.updated[which(airline.updated$q‹ó‰ïĞ == "SFJ"), 26] ## SFJ‚Íƒ†ƒjƒbƒgƒRƒXƒg‚ª9
airline.updated[which(airline.updated$q‹ó‰ïĞ == "ADO"), 26] <- (9/11)*airline.updated[which(airline.updated$q‹ó‰ïĞ == "ADO"), 26]
airline.updated[which(airline.updated$q‹ó‰ïĞ == "SKY"), 26] <- (5.5/11)*airline.updated[which(airline.updated$q‹ó‰ïĞ == "SKY"), 26]

## •\‚Æ‘“yŒğ’ÊÈƒf[ƒ^‚ÅˆÙ‚È‚éƒGƒAƒ‰ƒCƒ“–¼‚ğˆê’v‚³‚¹‚é
## ƒWƒFƒCEƒGƒA¨JAL
## ƒWƒFƒCƒGƒA‚Í”rœ‚µ‚½iÎj

## Š²ü‚ÉƒtƒH[ƒJƒX
ODnames.updated <- c("“Œ‹", "¬“c", "ŠÖ¼", "‘åã", "_ŒË", "“ì‹I”’•l", "•Ÿ‰ª", "–k‹ãB", "²‰ê", "VçÎ", "‹uì", "“ß”e", "å‘ä", "”ŸŠÙ")
ODNo.updated <- c(rep(18, 2), rep(25, 4), rep(37, 3), rep(8, 2), 46, 12, 3)
ODcorrespondence.updated <- data.frame(ODnames = ODnames.updated, ODNo = ODNo.updated)

airline.updated2 <- airline.updated %>% 
  dplyr::filter(‹ó`i == "“Œ‹" | ‹ó`i == "¬“c" | ‹ó`i == "ŠÖ¼" | ‹ó`i == "‘åã" | ‹ó`i == "_ŒË" | ‹ó`i == "“ì‹I”’•l" | ‹ó`i == "•Ÿ‰ª" | ‹ó`i == "–k‹ãB" | ‹ó`i == "²‰ê" | ‹ó`i == "VçÎ" | ‹ó`i == "‹uì" | ‹ó`i == "“ß”e" | ‹ó`i == "å‘ä" | ‹ó`i == "”ŸŠÙ") %>% 
  dplyr::filter(‹ó`j == "“Œ‹" | ‹ó`j == "¬“c" | ‹ó`j == "ŠÖ¼" | ‹ó`j == "‘åã" | ‹ó`j == "_ŒË" | ‹ó`j == "“ì‹I”’•l" | ‹ó`j == "•Ÿ‰ª" | ‹ó`j == "–k‹ãB" | ‹ó`j == "²‰ê" | ‹ó`j == "VçÎ" | ‹ó`j == "‹uì" | ‹ó`j == "“ß”e" | ‹ó`j == "å‘ä" | ‹ó`j == "”ŸŠÙ")
for(k in 1:2){
  for(i in 1:nrow(airline.updated2)){
    for(j in 1:nrow(ODcorrespondence.updated)){
      if(airline.updated2[i, k] == ODcorrespondence.updated[j, 1]){
        airline.updated2[i, k + 26] <- ODcorrespondence.updated[j, 2]
      } else {
        
      }
    }
  }
}

## Read data
## airline_revised.csv‚Íairline.csv‚ÆƒtƒH[ƒ}ƒbƒgi—ñ‚ğ‚»‚ë‚¦‚éj
airline.updated3 <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/airline_revised.csv", header = T)

## Filter data
## Š²ü‚Ì‚İ‚ÉƒtƒH[ƒJƒX
airline.updated3 <- airline.updated3 %>% 
  dplyr::filter(‹ó`i == "“Œ‹" | ‹ó`i == "¬“c" | ‹ó`i == "ŠÖ¼" | ‹ó`i == "‘åã" | ‹ó`i == "_ŒË" | ‹ó`i == "“ì‹I”’•l" | ‹ó`i == "•Ÿ‰ª" | ‹ó`i == "–k‹ãB" | ‹ó`i == "²‰ê" | ‹ó`i == "VçÎ" | ‹ó`i == "‹uì" | ‹ó`i == "“ß”e" | ‹ó`i == "å‘ä" | ‹ó`i == "”ŸŠÙ") %>% 
  dplyr::filter(‹ó`j == "“Œ‹" | ‹ó`j == "¬“c" | ‹ó`j == "ŠÖ¼" | ‹ó`j == "‘åã" | ‹ó`j == "_ŒË" | ‹ó`j == "“ì‹I”’•l" | ‹ó`j == "•Ÿ‰ª" | ‹ó`j == "–k‹ãB" | ‹ó`j == "²‰ê" | ‹ó`j == "VçÎ" | ‹ó`j == "‹uì" | ‹ó`j == "“ß”e" | ‹ó`j == "å‘ä" | ‹ó`j == "”ŸŠÙ")

## Attach airline data to airline.updated2
airline.updated2$‹ó`i <- as.character(airline.updated2$‹ó`i)
airline.updated2$‹ó`j <- as.character(airline.updated2$‹ó`j)
airline.updated2$q‹ó‰ïĞ <- as.character(airline.updated2$q‹ó‰ïĞ)
airline.updated3$‹ó`i <- as.character(airline.updated3$‹ó`i)
airline.updated3$‹ó`j <- as.character(airline.updated3$‹ó`j)
airline.updated3$q‹ó‰ïĞ <- as.character(airline.updated3$q‹ó‰ïĞ)

for(k in 1:2){
  for(i in 1:nrow(airline.updated3)){
    for(j in 1:nrow(ODcorrespondence.updated)){
      if(airline.updated3[i, k + 3] == ODcorrespondence.updated[j, 1]){
        airline.updated3[i, k + 19] <- ODcorrespondence.updated[j, 2]
      } else {
        
      }
    }
  }
}

airlinenames.updated <- c("JAL", "ANA", "SFJ", "SKY", "IBX", "JAC", "ADO", "AMX", "JTA", "HAC", "JEX") ## ‚±‚ê–Ô—…‚·‚éAŠ²ü•ª‚Í‚¨‚¯
airlineNo.updated <- c(seq(1:11))
airlinecorrespondence.updated <- data.frame(airlinenames = airlinenames.updated, airlineNo = airlineNo.updated)
airlinecorrespondence.updated$airlinenames <- as.character(airlinecorrespondence.updated$airlinenames)

for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(airlinecorrespondence.updated)){
    if(airline.updated2[i, 3] == airlinecorrespondence.updated[j, 1]){
      airline.updated2[i, 29] <- airlinecorrespondence.updated[j, 2]
    } else {
      
    }
  }
}
for(i in 1:nrow(airline.updated3)){
  for(j in 1:nrow(airlinecorrespondence.updated)){
    if(airline.updated3[i, 8] == airlinecorrespondence.updated[j, 1]){
      airline.updated3[i, 22] <- airlinecorrespondence.updated[j, 2]
    } else {
      
    }
  }
}
write.csv(airline.updated2, "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/airline.updated2Šm”F.csv")
write.csv(airline.updated3, "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/airline.updated3Šm”F.csv")


for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(airline.updated3)){
    if(airline.updated2[i, 1] == airline.updated3[j, 4] && airline.updated2[i, 2] == airline.updated3[j, 5] && airline.updated2[i, 29] == airline.updated3[j, 22]){
      (airline.updated2[i, 30] <- airline.updated3[j, 9]) & (airline.updated2[i, 31] <- airline.updated3[j, 10]) & (airline.updated2[i, 32] <- airline.updated3[j, 12]) & (airline.updated2[i, 33] <- airline.updated3[j, 13]) & (airline.updated2[i, 34] <- airline.updated3[j, 15])
    } else {
      
    }
  }
}

colnames(airline.updated2)[27] <- "i"
colnames(airline.updated2)[28] <- "j"
colnames(airline.updated2)[29] <- "airlineNo"
colnames(airline.updated2)[30] <- "Š—vŠÔ"
colnames(airline.updated2)[31] <- "”ï—p"
colnames(airline.updated2)[32] <- "q‹ó•Ö”"
colnames(airline.updated2)[33] <- "‘Øİ‰Â”\ŠÔ"
colnames(airline.updated2)[34] <- "ƒGƒAƒ‰ƒCƒ“Q“ü”"
## Drop NAs 
airline.updated2 <- airline.updated2 %>% 
  dplyr::filter(!is.na(ƒGƒAƒ‰ƒCƒ“Q“ü”)) %>%  # ƒGƒAƒ‰ƒCƒ“Q“ü” has no particular meaning! it is just appropriate to remove NAs 
  dplyr::filter(!is.na(Freq)) %>% 
  dplyr::arrange(i, j) %>%  
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(ODairline = sum(—·‹q”))

## railway data
OD <- OD %>% 
  dplyr::filter(X == 18 | X == 25 | X == 37 | X == 8 | X == 46 | X == 12 | X == 3) %>% 
  dplyr::filter(X.1 == 18 | X.1 == 25 | X.1 == 37 | X.1 == 8 | X.1 == 46 | X.1 == 12 | X.1 == 3) %>% 
  dplyr::filter(X != X.1)
OD <- OD[ , c(1, 2, 3, 4, 6)]
OD$railway <- OD$railway*1000

## attach railway demand data to airline.updated
for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(OD)){
    if(airline.updated2[i, 27]==OD[j, 1] && airline.updated2[i, 28]==OD[j, 2]){
      airline.updated2[i, 36] <- OD[j, 5]
    }
  }
}
colnames(airline.updated2)[36] <- "ODrailway"
## Derive ODs
airline.updated2 <- airline.updated2 %>% 
  dplyr::mutate(OD = ODairline + ODrailway)

## Add accessibility
## VçÎ‚Æ“ß”e‚É‘Î‰‚³‚¹‚é
## ƒCƒOƒŒƒX‚É‘Î‰‚³‚¹‚é
## ‚±‚ê‚¾‚Æ‘S˜Hü‚ÉŠg’£‚·‚é‚Ì’n–‚»‚¤Î
accessibility <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/accessibility.csv", header = T)
accessibility$ƒAƒNƒZƒVƒrƒŠƒeƒB <- accessibility$ƒAƒNƒZƒVƒrƒŠƒeƒB/1000
accessibility$‰w.‹ó` <- as.character(accessibility$‰w.‹ó`)
for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(accessibility)){
    if(airline.updated2[i, 1] == accessibility[j, 2] && accessibility[j, 3] == 0){
      airline.updated2[i, 38] <- accessibility[j, 4]
    } else {
      
    }
  }
}
for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(accessibility)){
    if(airline.updated2[i, 2] == accessibility[j, 2] && accessibility[j, 3] == 0){
      airline.updated2[i, 38] <- airline.updated2[i, 38] + accessibility[j, 4]
    } else {
      
    }
  }
}
colnames(airline.updated2)[38] <- "ƒAƒNƒZƒVƒrƒŠƒeƒB"

## Add ˆê•Ö‚ ‚½‚èÀÈ” and ˆê•Ö‚ ‚½‚è‰İ•¨
airline.updated2 <- airline.updated2 %>% 
  dplyr::mutate(“S“¹ƒ_ƒ~[ = 0, 
                ˆê•Ö‚ ‚½‚èÀÈ” = ÀÈ”/‰^q‰ñ”, 
                ˆê•Ö‚ ‚½‚è‰İ•¨ = ‰İ•¨/‰^q‰ñ”)
## Write csv
write.csv(airline.updated2, "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/airline.updated2.csv")

## Introduce a new data frame
data.updated <- airline.updated2[ , c(27, 28, 1, 2, 3, 7, 30, 31, 32, 33, 38, 34, 39, 40, 41)]
## Read data
data2railway <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/data2railway.csv", header = T, stringsAsFactors = FALSE)
data2railway <- as.list(data2railway)

## Counterfactual data
## ‚±‚±‚É‚àˆê•Ö‚ ‚½‚èÀÈ”‚Æˆê•Ö‚ ‚½‚è‰İ•¨‚ğ’Ç‰Á‚¹‚æ
## base1.csv‚ğ’Ç‰Á‚·‚é
## ƒx[ƒX‚ğ‚â‚é‚Æ‚«‚Íbase1‚ğbind‚·‚é
counterfactual1 <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
counterfactual1 <- as.list(counterfactual1)
base1 <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/base1.csv", header = T, stringsAsFactors = FALSE)
base1 <- as.list(base1)

data.updated <- rbind(data.updated, data2railway, counterfactual1) ## bind counterfactual as well!
write.csv(data.updated, "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/data_updatedŠm”F.csv")
for(i in 1:nrow(data.updated)){
  for(j in 1:nrow(OD)){
    if(data.updated[i, 1] == OD[j, 1] && data.updated[i, 2] == OD[j, 2] && data.updated[i, 13] == 1){
      data.updated[i, 6] <- OD[j, 5]
    }
  }
}
data.updated <- data.updated %>% 
  dplyr::arrange(i, j)

## Add OD
data.updated <- data.updated %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(OD = sum(—·‹q”)) %>% 
  dplyr::ungroup() 


## Add LF, Freq, DIS, and MC
for(i in 1:nrow(data.updated)){
  for(j in 1:nrow(airline.updated2)){
    if(airline.updated2[j, 1] == data.updated[i, 3] && airline.updated2[j, 2] == data.updated[i, 4] && airline.updated2[j, 3] == data.updated[i, 5]){
      (data.updated[i, 17] <- airline.updated2[j, 23]) & (data.updated[i, 18] <- airline.updated2[j, 24]) & (data.updated[i, 19] <- airline.updated2[j, 4]) & (data.updated[i, 20] <- airline.updated2[j, 26]) & (data.updated[i, 21] <- airline.updated2[j, 12]) 
    } else {
      
    }
  }
}
colnames(data.updated)[17] <- "DIS"
colnames(data.updated)[18] <- "Seat"
colnames(data.updated)[19] <- "Freq"
colnames(data.updated)[20] <- "MC"
colnames(data.updated)[21] <- "LF"




## Add observedprob
data.updated <- data.updated %>% 
  dplyr::mutate(observedprob = —·‹q”/OD)

write.csv(data.updated, "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/data_updated.csv")

