## IV estimation
## Distance can be an IV since it is correlated with price
## We assume that distance is uncorrelated with unobserved qualities of products

## Library
library(dplyr)
library(tidyr)
library(AER)
## Data
data.for.IV <- demand
railway.distance <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/railwaycost.csv", header = T)
airline.distance <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
## Attach railway distance
for(i in 1:nrow(data.for.IV)){
  for(j in 1:nrow(railway.distance)){
    if(data.for.IV[i, 11] == railway.distance[j, 3] && data.for.IV[i, 12] == railway.distance[j, 4] && data.for.IV[i, 21] == 1){
      data.for.IV[i, 26] <- railway.distance[j, 5]
    } else {
      
    }
  }
}
## Tidy airline distance
airline.distance$運航回数 <- as.numeric(gsub(",", "", airline2$運航回数)) # gsub is used to remove commas in each figure
airline.distance$運航キロメートル <- as.numeric(gsub(",", "", airline2$運航キロメートル))
airline.distance <- airline.distance %>% 
  dplyr::mutate(distance.per = 運航キロメートル/運航回数) %>% 
  dplyr::group_by(空港i, 空港j) %>% 
  dplyr::mutate(distance = mean(distance.per)) %>% 
  dplyr::ungroup()
## Raplace names
airline.distance$空港i <- gsub("東京", "羽田", airline.distance$空港i)
airline.distance$空港j <- gsub("東京", "羽田", airline.distance$空港j)
airline.distance$空港i <- gsub("関西", "関西国際", airline.distance$空港i)
airline.distance$空港j <- gsub("関西", "関西国際", airline.distance$空港j)

## Attach airline distance
data.for.IV$最寄りi <- as.character(data.for.IV$最寄りi)
data.for.IV$最寄りj <- as.character(data.for.IV$最寄りj)
for(i in 1:nrow(data.for.IV)){
  for(j in 1:nrow(airline.distance)){
    if(data.for.IV[i, 11] == airline.distance[j, 1] && data.for.IV[i, 12] == airline.distance[j, 2] && data.for.IV[i, 21] == 0){
      data.for.IV[i, 26] <- airline.distance[j, 24]
    } else {
      
    }
  }
}
colnames(data.for.IV)[26] <- "distance"
write.csv(data.for.IV, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/dataforIV.csv")
## 
IV <- data.for.IV[ , c(24, 25, 16, 17, 18 ,19, 20, 21, 22, 26)] ## Probably have to revise it! We have to leave share and out.share
IV <- IV %>% 
  dplyr::mutate(y = log(share) -log(out.share))
## Write csv file
write.csv(IV, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/IV.csv")
## Estimate parameters of utility function
## Delete rows including NAs
for(i in nrow(IV):1){
  if(is.na(IV[i, 10])){
    IV <- IV[-i, ]
  } else {
    
  }
}
IV <- IV[-which(IV$y == Inf), ]
IV <- IV[-which(is.na(IV$y) | is.na(IV$所要時間) | is.na(IV$費用) | is.na(IV$航空便数) | is.na(IV$滞在可能時間) 
                        | is.na(IV$アクセシビリティ) | is.na(IV$鉄道ダミー) | is.na(IV$エアライン参入数)), ]

IVreg <- ivreg(data = IV, y ~ 所要時間 + 費用 + log(航空便数*365) + 滞在可能時間 + アクセシビリティ + 鉄道ダミー + エアライン参入数 | 
                 所要時間 + log(航空便数*365) + 滞在可能時間 + アクセシビリティ + 鉄道ダミー + エアライン参入数 + distance) ## いったん航空便数を365倍で処理
IV_summary <- stargazer::stargazer(IVreg, logit, type = "html")

IVpar <- IVreg$coefficients
