## Supply Analysis 3
## Preparing for counterfactual simulations

## Library
library(magrittr)
library(tidyr)

## Read data
airline.updated <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
OD <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/OD.csv", header = T, stringsAsFactors = FALSE)

## Transform characters into numeric
airline.updated$運航回数 <- as.numeric(gsub(",", "", airline.updated$運航回数)) # gsub is used to remove commas in each figure
airline.updated$運航キロメートル <- as.numeric(gsub(",", "", airline.updated$運航キロメートル))
airline.updated$旅客数 <- as.numeric(gsub(",", "", airline.updated$旅客数))
airline.updated$座席数 <- as.numeric(gsub(",", "", airline.updated$座席数))
airline.updated$貨物 <- as.numeric(gsub(",", "", airline.updated$貨物))
## Some variables necessary to analyse data
airline.updated <- airline.updated %>% 
  dplyr::mutate(DIS = 運航キロメートル/運航回数,
                Seat = 座席数/運航回数,
                Freq = 旅客数/(Seat*座席利用率/100), 
                MC = 11*DIS) # I have to revise this! Unit cost should be obtained from annual reports

## 航空会社ごとに限界費用を調整する
## ちゃんと全部やる
## とりあえず幹線は以下
## JAL, ANA, JTA, JAC, IBX, HAC = 11
## ADO, SFJ = 9
## SKY = 5.5
airline.updated[which(airline.updated$航空会社 == "SFJ"), 26] <- (9/11)*airline.updated[which(airline.updated$航空会社 == "SFJ"), 26] ## SFJはユニットコストが9
airline.updated[which(airline.updated$航空会社 == "ADO"), 26] <- (9/11)*airline.updated[which(airline.updated$航空会社 == "ADO"), 26]
airline.updated[which(airline.updated$航空会社 == "SKY"), 26] <- (5.5/11)*airline.updated[which(airline.updated$航空会社 == "SKY"), 26]

## 時刻表と国土交通省データで異なるエアライン名を一致させる
## ジェイ・エア→JAL
## ジェイエアは排除した（笑）

## 幹線にフォーカス
ODnames.updated <- c("東京", "成田", "関西", "大阪", "神戸", "南紀白浜", "福岡", "北九州", "佐賀", "新千歳", "丘珠", "那覇", "仙台", "函館")
ODNo.updated <- c(rep(18, 2), rep(25, 4), rep(37, 3), rep(8, 2), 46, 12, 3)
ODcorrespondence.updated <- data.frame(ODnames = ODnames.updated, ODNo = ODNo.updated)

airline.updated2 <- airline.updated %>% 
  dplyr::filter(空港i == "東京" | 空港i == "成田" | 空港i == "関西" | 空港i == "大阪" | 空港i == "神戸" | 空港i == "南紀白浜" | 空港i == "福岡" | 空港i == "北九州" | 空港i == "佐賀" | 空港i == "新千歳" | 空港i == "丘珠" | 空港i == "那覇" | 空港i == "仙台" | 空港i == "函館") %>% 
  dplyr::filter(空港j == "東京" | 空港j == "成田" | 空港j == "関西" | 空港j == "大阪" | 空港j == "神戸" | 空港j == "南紀白浜" | 空港j == "福岡" | 空港j == "北九州" | 空港j == "佐賀" | 空港j == "新千歳" | 空港j == "丘珠" | 空港j == "那覇" | 空港j == "仙台" | 空港j == "函館")
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
## airline_revised.csvはairline.csvとフォーマット（列をそろえる）
airline.updated3 <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline_revised.csv", header = T)

## Filter data
## 幹線のみにフォーカス
airline.updated3 <- airline.updated3 %>% 
  dplyr::filter(空港i == "東京" | 空港i == "成田" | 空港i == "関西" | 空港i == "大阪" | 空港i == "神戸" | 空港i == "南紀白浜" | 空港i == "福岡" | 空港i == "北九州" | 空港i == "佐賀" | 空港i == "新千歳" | 空港i == "丘珠" | 空港i == "那覇" | 空港i == "仙台" | 空港i == "函館") %>% 
  dplyr::filter(空港j == "東京" | 空港j == "成田" | 空港j == "関西" | 空港j == "大阪" | 空港j == "神戸" | 空港j == "南紀白浜" | 空港j == "福岡" | 空港j == "北九州" | 空港j == "佐賀" | 空港j == "新千歳" | 空港j == "丘珠" | 空港j == "那覇" | 空港j == "仙台" | 空港j == "函館")

## Attach airline data to airline.updated2
airline.updated2$空港i <- as.character(airline.updated2$空港i)
airline.updated2$空港j <- as.character(airline.updated2$空港j)
airline.updated2$航空会社 <- as.character(airline.updated2$航空会社)
airline.updated3$空港i <- as.character(airline.updated3$空港i)
airline.updated3$空港j <- as.character(airline.updated3$空港j)
airline.updated3$航空会社 <- as.character(airline.updated3$航空会社)

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

airlinenames.updated <- c("JAL", "ANA", "SFJ", "SKY", "IBX", "JAC", "ADO", "AMX", "JTA", "HAC", "JEX") ## これ網羅する、幹線分はおけ
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
write.csv(airline.updated2, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline.updated2確認.csv")
write.csv(airline.updated3, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline.updated3確認.csv")


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
colnames(airline.updated2)[30] <- "所要時間"
colnames(airline.updated2)[31] <- "費用"
colnames(airline.updated2)[32] <- "航空便数"
colnames(airline.updated2)[33] <- "滞在可能時間"
colnames(airline.updated2)[34] <- "エアライン参入数"
## Drop NAs 
airline.updated2 <- airline.updated2 %>% 
  dplyr::filter(!is.na(エアライン参入数)) %>%  # エアライン参入数 has no particular meaning! it is just appropriate to remove NAs 
  dplyr::filter(!is.na(Freq)) %>% 
  dplyr::arrange(i, j) %>%  
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(ODairline = sum(旅客数))

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
## 新千歳と那覇に対応させる
## イグレスに対応させる
## これだと全路線に拡張するの地獄そう笑
accessibility <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/accessibility.csv", header = T)
accessibility$アクセシビリティ <- accessibility$アクセシビリティ/1000
accessibility$駅.空港 <- as.character(accessibility$駅.空港)
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
colnames(airline.updated2)[38] <- "アクセシビリティ"

## Add 一便あたり座席数 and 一便あたり貨物
airline.updated2 <- airline.updated2 %>% 
  dplyr::mutate(鉄道ダミー = 0, 
                一便あたり座席数 = 座席数/運航回数, 
                一便あたり貨物 = 貨物/運航回数)
## Write csv
write.csv(airline.updated2, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline.updated2.csv")

## Introduce a new data frame
data.updated <- airline.updated2[ , c(27, 28, 1, 2, 3, 7, 30, 31, 32, 33, 38, 34, 39, 40, 41)]
## Read data
data2railway <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data2railway.csv", header = T, stringsAsFactors = FALSE)
data2railway <- as.list(data2railway)

## Counterfactual data
## ここにも一便あたり座席数と一便あたり貨物を追加せよ
## base1.csvを追加する
## ベースをやるときはbase1をbindする
counterfactual1 <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
counterfactual1 <- as.list(counterfactual1)
base1 <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/base1.csv", header = T, stringsAsFactors = FALSE)
base1 <- as.list(base1)

data.updated <- rbind(data.updated, data2railway, counterfactual1) ## bind counterfactual as well!
write.csv(data.updated, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data_updated確認.csv")
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
  dplyr::mutate(OD = sum(旅客数)) %>% 
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
  dplyr::mutate(observedprob = 旅客数/OD)

write.csv(data.updated, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data_updated.csv")

