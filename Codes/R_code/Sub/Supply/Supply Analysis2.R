## Library
library(magrittr)
library(tidyr)

## New data
data3 <- data2

## Attach new airlineNo
airlineNo2 <- c(1, 2, 3, 4, 5)
airlinenames2 <- c("JAL", "ANA", "SFJ", "SKY", "JAC")
airlinecorrespondence2 <- data.frame(airlineNo = airlineNo2, airlinenames = airlinenames2)
for(i in 1:nrow(data3)){
  for(j in 1:nrow(airlinecorrespondence2)){
    if(data3[i, 5] == airlinecorrespondence2[j, 2]){
      data3[i, 24] <- airlinecorrespondence2[j, 1]
    }
  }
}
colnames(data3)[24] <- "airlineNo"
# s[3]が所要時間、s[2]が費用←内生変数

## Write functions
## prob is a function which returns Pr
prob <- function(a){
  exputility <- rep(0, nrow(data3))
  sumexputility <- rep(0, nrow(data3))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(data3)){
    exputility[k] <- (a*exp(utilitypar[1] + utilitypar[2]*data3$所要時間[k] + utilitypar[3]*Price[k]
                            + utilitypar[4]*log(Frequency[k]) + utilitypar[5]*data3$滞在可能時間[k]
                            + utilitypar[6]*data3$アクセシビリティ[k] + utilitypar[7]*data3$鉄道ダミー[k]
                            + utilitypar[8]*data3$エアライン参入数[k]))
  }
  sumexputility <- data.frame(i = data3$i, j = data3$j, exputility = exputility)
  sumexputility <- sumexputility %>% 
    dplyr::group_by(i, j) %>% 
    dplyr::mutate(sumexputility = sum(exputility)) %>% 
    dplyr::ungroup()
  sumexputility <- sumexputility$sumexputility
  sumexputility <- as.numeric(sumexputility)
  output <- exputility/sumexputility
  return(output)
}

## Write a function which returns first order derivative
partialderiv <- function(a){
  exputility <- c(rep(0, nrow(data3)))
  sumexputility <- c(rep(0, nrow(data3)))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(data3)){
    exputility[k] <- (a*exp(utilitypar[1] + utilitypar[2]*data3$所要時間[k] + utilitypar[3]*Price[k]
                            + utilitypar[4]*log(Frequency[k]) + utilitypar[5]*data3$滞在可能時間[k]
                            + utilitypar[6]*data3$アクセシビリティ[k] + utilitypar[7]*data3$鉄道ダミー[k]
                            + utilitypar[8]*data3$エアライン参入数[k]))
  }
  sumexputility <- data.frame(i = data3$i, j = data3$j, exputility = exputility)
  sumexputility <- sumexputility %>% 
    dplyr::group_by(i, j) %>% 
    dplyr::mutate(sumexputility = sum(exputility)) %>% 
    dplyr::ungroup()
  sumexputility <- sumexputility[ , 4]
  exputility <- as.numeric(exputility)
  sumexputility <- as.numeric(sumexputility$sumexputility)
  output <- matrix(0, nrow = nrow(data3), ncol = nrow(data3))
  for(i in 1:nrow(data3)){
    for(j in 1:nrow(data3)){
      if(data3[i, 1] == data3[j, 1] && data3[i, 2] == data3[j, 2] && data3[i, 5] == data3[j, 5]){
        output[i + nrow(data3)*(j - 1)] <- (-1)*(utilitypar[3]*exputility[i]*exputility[j])/((sumexputility[i])^2)
      } else {
        
      }
    }
  }
  for(i in 1:nrow(data3)){
    output[i + nrow(data3)*(i - 1)] <- (utilitypar[3]*exputility[i]*(sumexputility[i] - exputility[i]))/((sumexputility[i])^2)
  }
  return(output)
}

## Define vectors of endogenous variables called Price and Frequency
Price <- as.numeric(data3$費用) # As a initial value we give real data
Frequency <- as.numeric(data3$航空便数)
Pricerenewal <- c(rep(0, nrow(data3)))
Frequencyrenewal <- c(rep(0, nrow(data3)))
Demand <- as.numeric(data3$旅客数)

## Stopping creiteria
distance <- 1000
iter <- 1

while(iter < 3){
  Pr <- prob(1) # "a" should be revised
  for(i in 1:nrow(data3)){
    if(data3[i, 5] == "JAL"){ ### "JAL" MUST be replaced with airlineNo!!
      Demand[i] <- data3[i, 14]*Pr[i]
      Frequencyrenewal[i] <- (Demand[i]/(data3[i, 20]*data3[i, 23]/100))/365
    } else {
      Frequencyrenewal[i] <- Frequency[i]
    }
  }
  Frequency <- as.numeric(Frequency)
  Frequencyrenewal <- as.numeric(Frequencyrenewal)
  distance <- sqrt(sum(Frequency - Frequencyrenewal)^2)
  Frequency <- Frequencyrenewal
  iter <- iter +1
}

resultaa <- data.frame(Frequency = Frequency, Pr = Pr, ObservedFreq = data3$航空便数, ObservedPr = data3$observedprob)

## Profit maximization

## You can delete this part later =================
## Library
library(magrittr)
library(tidyr)
## Define vectors of endogenous variables called Price and Frequency
Price <- as.numeric(data3$費用) # As a initial value we give real data
Pricerenewal <- c(rep(0, nrow(data3)))
## ================================================



## Drop airlines other than interested
Unit <- diag(nrow(data3)) ## This is necessary for removing airlines out of interest
for(i in 1:nrow(data3)){
  if(data3[i, 5] != "JAL"){
    Unit[i + nrow(data3)*(i - 1)] <-0
  }
}

## Optimization
## Stopping Criteria
distance2 <- 1000
iter2 <- 1
sign <- -5
## First Order Condition
while((distance > 1e-4) | (sign < 0)){
  ## First we calculate Freq
  ## Stopping creiteria
  distance <- 1000
  iter <- 1
  
  while(iter < 3){
    Pr <- prob(1) # "a" should be revised
    for(i in 1:nrow(data3)){
      if(data3[i, 5] == "JAL"){ ### "JAL" MUST be replaced with airlineNo!!
        Demand[i] <- data3[i, 14]*Pr[i]
        Frequencyrenewal[i] <- (Demand[i]/(data3[i, 20]*data3[i, 23]/100))/365
      } else {
        Frequencyrenewal[i] <- Frequency[i]
      }
    }
    Frequency <- as.numeric(Frequency)
    Frequencyrenewal <- as.numeric(Frequencyrenewal)
    distance <- sqrt(sum(Frequency - Frequencyrenewal)^2)
    Frequency <- Frequencyrenewal
    iter <- iter +1
  }
  
  ## Next we calculate Price
  Derivmatrix <- partialderiv(1) ## I have to revise this later
  Demand <- as.numeric(Demand)
  MC <- as.matrix(data3$MC)
  MC <- ifelse(is.na(MC), 0, MC)
  Price <- as.matrix(Price)
  FOC <- Unit %*% (Demand + Derivmatrix %*% (Price - MC))
  airlinedummy <- rowSums(Unit)
  colindex <- 0
  for(i in 1:length(airlinedummy)){
    if(airlinedummy[i] == 1){
      colindex <- c(colindex, i)
    } else {
      
    }
  }
  colindex <- colindex[c(-1)]
  airlineDemand <- Demand[colindex]
  airlineMC <- MC[colindex]
  airlineDerivmatrix <- Derivmatrix[colindex, colindex]
  airlinePrice <- airlineMC + (-1)*solve(airlineDerivmatrix) %*% airlineDemand
  airlinePrice <- airlinePrice[ , 1]
  Pricerenewal <- Price
  for(i in 1:length(colindex)){
    Pricerenewal[colindex[i]] <- airlinePrice[i]
  }
  Price <- Pricerenewal
  distance2 <- sqrt(sum(FOC)^2)
  sign <- sum(sign(Price)) - as.numeric(length(Price))
  iter2 <- iter2 + 1 
}

resultbd <- data.frame(simulatedPrice = Pricerenewal[, 1], observedPrice = data3$費用, 
                       Frequency = Frequency, Pr = Pr, ObservedFreq = data3$航空便数, 
                       ObservedPr = data3$observedprob)
write.csv(resultbd, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/resultbd.csv")

## Del ======================================
A <- solve(Derivmatrix)
Derivmatrix <- partialderiv(1)
airlinedummy <- rowSums(Unit)
colindex <- 0
for(i in 1:length(airlinedummy)){
  if(airlinedummy[i] == 1){
    colindex <- c(colindex, i)
  } else {
    
  }
}
colindex <- colindex[c(-1)]
airlineDemand <- Demand[colindex]
airlineMC <- MC[colindex]
airlineDerivmatrix <- Derivmatrix[colindex, colindex]
B <- solve(airlineDerivmatrix)
write.csv(B, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/InvairlineDerivmatrix.csv")
write.csv(airlineDerivmatrix, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/airlineDerivmatrix.csv")


