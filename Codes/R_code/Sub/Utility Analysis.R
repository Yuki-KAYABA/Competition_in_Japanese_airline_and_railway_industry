## Library
library(magrittr)
library(tidyr)

## Read data
airline <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline.csv", header = T)
ODforairline <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/ODforairline.csv", header = T)
data <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data.csv", header = T)
station <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/station.csv", header = T)
airport <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airport.csv", header = T)
combine <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/combine.csv", header = T)
railway <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/railway.csv", header = T)
railwaycost <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/railwaycost.csv", header = T)
OD <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/OD.csv", header = T, stringsAsFactors = FALSE)
accessibility.station <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/accessibility_station.csv", header = T)
accessibility.airport <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/accessibility_airport.csv", header = T)
OD.207 <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/207OD.csv", header = T)
## Combine airports
for(k in 1:2){
  for(i in 1:174){
    for(j in 1:8){
      if(airline[i, k+1] == combine[j, 1]){
        airline[i, k+3] <- combine[j, 3]
      } else {
        
      }
    }
  }
}
## 航空運賃が往復で入っているので片道運賃にする
airline$費用 <- airline$費用/2

## Combine ODs
airline <- airline %>% 
  dplyr::group_by(空港i, 空港j) %>% 
  dplyr::mutate(i = min(i),
                j = min(j),
                freq = 365*sum(航空便数), ## 航空便数は丸める部分に関しては総和で処理する、でここで365倍する
                cost = sum(航空便数 * 費用)/sum(航空便数), ## 単純平均から加重平均に差し替えた
                duration = sum(航空便数 * 所要時間)/sum(航空便数), ## これも加重平均
                stay = max(滞在可能時間), ## これは平均から最大値に変えた
                entry = mean(エアライン参入社数)) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        vars = c('i', 'j', 'duration', 'cost', 'freq', 'stay', 'entry'))
colnames(airline) <- c("空港i", "空港j", "i", "j", "duration", "cost", "freq", "stay", "entry")

# Insert airline data into the whole one
for(i in 1:nrow(data)){
  for(j in 1:nrow(airline)){
    if(data[i, 20]==0 && data[i, 6]==airline[j, 3] && data[i, 7]==airline[j, 4]){ ## 20:鉄道ダミー, 6:47ゾーンi, 7:47ゾーンj
      data[i , c(15, 16, 17, 18, 21)] <- airline[j, c(5, 6, 7, 8, 9)]
    } else {
      
    }
  }
}

# Transform 所要時間 and 滞在可能時間 and cost into numeric
railway$所要時間.1 <-as.numeric(as.character(railway$所要時間.1))
railway$滞在可能時間.1 <- as.numeric(as.character(railway$滞在可能時間.1))
railwaycost$cost <- as.numeric(as.character(railwaycost$cost))

# Attach appropriate figures to each station 
##  for(k in 1:2){
##    for(i in 1:nrow(railway)){
##      for(j in 1:nrow(station)){
##        if(railway[i, k + 2]==station[j, 2]){ ## railwayの3，4列目は駅名
##          railway[i, k + 23] <- station[j, 1] ## railwayの24，25列目に番号を挿入している、いらなくない？
##        } else {
##          
##        }
##      }
##    }
##  }
##  for(k in 1:2){
##    for(i in 1:nrow(railwaycost)){
##      for(j in 1:nrow(station)){
##        if(railwaycost[i, k + 2]==station[j, 2]){
##          railwaycost[i, k + 6] <- station[j, 1]
##        } else {
##          
##        }
##      }
##    }
##  }

## Substitute 0 into NAs so that the following code will run
##for(k in 1:2){
##  for(i in 1:nrow(railway)){
##    if(is.na(railway[i, k + 23])){
##      railway[i, k + 23] <- 0
##    } else {
##      
##    }
##  }
##}
##for(k in 1:2){
##  for(i in 1:nrow(railwaycost)){
##    if(is.na(railwaycost[i, k + 6])){
##      railwaycost[i, k + 6] <- 0
##    } else {
##      
##    }
##  }
##}

## Insert railways duration, stay, and cost data  
for(i in 1:nrow(data)){
  for(j in 1:nrow(railway)){
    if(data[i, 20]==1 && data[i, 10]==railway[j, 3] && data[i, 11]==railway[j, 4]){
      (data[i , 15] <- railway[j, 22]) & (data[i, 18] <- railway[j, 23]) 
    } else {
      
    }
  }
}
for(i in 1:nrow(data)){
  for(j in 1:nrow(railwaycost)){
    if(data[i, 20]==1 && data[i, 10]==railwaycost[j, 3] && data[i, 11]==railwaycost[j, 4]){
      data[i , 16] <- railwaycost[j, 6]*2 # I have to revise this! I just get data only on 乗車券運賃
    } else {
      
    }
  }
}

## Insert 1s into 航空便数 if railway dummy is equal to 1, so that ln(航空便数) will be zero when the dummy is 1
for(i in 1:nrow(data)){
  if(data[i, 20]==1){
    data[i, c(17, 21)] <- c(1, 0)
  } else {
    
  }
}

## イグレスを反映
for(i in 1:nrow(data)){
  for(j in 1:nrow(accessibility.station)){
    if(data[i, 20] == 1 && data[i, 9] == accessibility.station[j, 1]){
      data[i, 22] <- accessibility.station[j, 2]
    } else {
      
    }
  }
}
for(i in 1:nrow(data)){
  for(j in 1:nrow(accessibility.airport)){
    if(data[i, 20] == 0 && data[i, 9] == accessibility.airport[j, 1]){
      data[i, 22] <- accessibility.airport[j, 2]
    } else {
      
    }
  }
}
data$アクセシビリティ <- data$アクセシビリティ + data$V22

## ここまで==========================================
## Some variables are recognized as factor, so we have to transform them into numeric
data$x_ij.k <-as.numeric(as.character(data$x_ij.k))
data$ODij <- as.numeric(as.character(data$ODij))


## Nothing has changed utill this row ====================================================================
## Revise OD data so that we can take outside goods into consideration
## This takes times according to your computer
for(i in 1:nrow(data)){
  for(j in 1:nrow(OD.207)){
    if(data[i, 4] == OD.207[j, 2] && data[i, 5] == OD.207[j, 3]){
      data[i, 14] <- OD.207[j, 11]
    } else {
      
    }
  }
}
for(i in 1:nrow(data)){
  for(j in 1:nrow(OD.207)){
    if(data[i, 4] == OD.207[j, 2] && data[i, 5] == OD.207[j, 3] && data[i, 20] == 1){
      data[i, 13] <- OD.207[j, 7]
    } else {
      
    }
  }
}
for(i in 1:nrow(data)){
  for(j in 1:nrow(OD.207)){
    if(data[i, 4] == OD.207[j, 2] && data[i, 5] == OD.207[j, 3] && data[i, 20] == 0){
      data[i, 13] <- OD.207[j, 6]
    } else {
      
    }
  }
}
## Compute share and outside share
data <- data %>% 
  dplyr::mutate(prob = x_ij.k/ODij) %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(out.share = 1 - sum(prob)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(share = ifelse(x_ij.k/ODij == 0, 1e-3, ifelse(x_ij.k/ODij == 1, 1 - 1e-3, x_ij.k/ODij)))

## Remove Inf
## Inf is generated when OD is zero despite xij is somehow strictly positive
data$share <- ifelse(is.infinite(data$prob), 1, data$share)

## Save data
write.csv(data, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data1.csv")

## If succesful, data for utility analysis is named as demand
demand <- data[ , c(24, 25, 15, 16, 17 ,18, 19, 20, 21)] ## Probably have to revise it! We have to leave share and out.share
demand <- demand %>% 
  dplyr::mutate(y = log(share) -log(out.share))
## Write csv file
write.csv(demand, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/demand.csv")
## Remove some variables
rm(airline, ODforairline, data, station, airport, combine, railway, railwaycost, OD, demand, accessibility.airport, accessibility.station)
demand <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/demand.csv", header = T)
## Estimate parameters of utility function
## Delete rows including NAs
for(i in nrow(demand):1){
  if(is.na(demand[i, 10])){
    demand <- demand[-i, ]
  } else {
    
  }
}
demand <- demand[-which(demand$y == Inf), ]
demand <- demand[-which(is.na(demand$y) | is.na(demand$所要時間) | is.na(demand$費用) | is.na(demand$航空便数) | is.na(demand$滞在可能時間) 
                        | is.na(demand$アクセシビリティ) | is.na(demand$鉄道ダミー) | is.na(demand$エアライン参入数)), ]
logit <- lm(data = demand, y ~ 所要時間 + 費用 + log(航空便数) + 滞在可能時間 + アクセシビリティ + 鉄道ダミー + エアライン参入数, 
            na.action = na.omit) ## いったん航空便数を365倍で処理
logit_summary <- stargazer::stargazer(logit, type = "html")

## Maximum likelihood estimation with sign restriction
## Write a function
fr <- function(x){
  LL <- 0
  for(i in 1:nrow(demand)){
    X <- x[1] + x[2]*demand$所要時間[i] - exp(x[3])*demand$費用[i] + exp(x[4])*log(demand$航空便数[i]) + 
      x[5]*demand$滞在可能時間[i] + x[6]*demand$アクセシビリティ[i] + x[7]*demand$鉄道ダミー[i] + x[8]*demand$エアライン参入数[i]
    Y <- demand$y[i]
    LLL <- (Y - X)^2
    LL <- LL + LLL
  }
  return(LL)
}

logit2 <- optim(c(rep(0, 8)), fr, method = "BFGS", hessian = TRUE, control = list(fnscale = 1))
par.sign <- logit2$par
par.sign[3] <- -exp(par.sign[3])
par.sign[4] <- exp(par.sign[4])
## What I have to do is:
## Remove NAs so that the function will run
## Check if desctiption fnscale = 1 is correct
## Revise demand$航空便数*365 since it attaches 365 in the rows of railways

## IV estimation

## Consider quadratic form
logit3 <- lm(data = demand, y ~ 所要時間 + 費用 + I(費用^2) + log(航空便数) + 滞在可能時間 + アクセシビリティ + 鉄道ダミー + エアライン参入数, 
             na.action = na.omit) ## いったん航空便数を365倍で処理
logit3_summary <- stargazer::stargazer(logit3, type = "html")

utilitypar.quad <- logit3$coefficients

peak <- -utilitypar.quad[3]/(utilitypar.quad[4]*2)

## Drop Variables
## Drop エアライン参入数
logit4 <- lm(data = demand, y ~ 所要時間 + 費用 + log(航空便数) + 滞在可能時間 + アクセシビリティ + 鉄道ダミー, 
             na.action = na.omit) ## いったん航空便数を365倍で処理
logit5 <- lm(data = demand, y ~ 費用 + log(航空便数))

logit_summary <- stargazer::stargazer(logit, type = "html")


