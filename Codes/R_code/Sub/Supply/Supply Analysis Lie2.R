## Set the numbers of iteration
it <- 50
it2 <- 10 ## 500?
it3 <- 50 ## 100?

## Utility parameters
parameter <- utilitypar ## utilitypar or abcdef or IVpar of par.sign

## Library
library(magrittr)
library(tidyr)

## New data
datalie2 <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/datalie2.csv", header = T)

## Attach new airlineNo
airlineNo2 <- c(1, 2, 3, 4, 5)
airlinenames2 <- c("JAL", "ANA", "SFJ", "SKY", "JAC")
airlinecorrespondence2 <- data.frame(airlineNo = airlineNo2, airlinenames = airlinenames2)
for(i in 1:nrow(datalie2)){
  for(j in 1:nrow(airlinecorrespondence2)){
    if(datalie2[i, 5] == airlinecorrespondence2[j, 2]){
      datalie2[i, 24] <- airlinecorrespondence2[j, 1]
    }
  }
}
datalie2[, 24] <- ifelse(is.na(datalie2[ , 24]), 0, datalie2[ , 24])
colnames(datalie2)[24] <- "airlineNo"
# s[3]が所要時間、s[2]が費用←内生変数

## Prepare adjustment parameter alpha
calculate.alpha <- function(x, y){
  X <- x %*% t(y) - diag(y)
  L <- X
  L[upper.tri(L)] <- 0
  U <- X
  U[lower.tri(U, diag = T)] <- 0
  iteration <- 1000
  xx <- matrix(rep(1, length(x)), nrow = length(x), ncol = 1) ## Initial value
  for(k in 1:iteration){
    xx <- (-1)*solve(L) %*% U %*% xx
  }
  output <- xx[, 1]/xx[1,1]
  return(output)
}
## Make alpha
datalie2$Freq <- ifelse(is.na(datalie2$Freq), 1, datalie2$Freq)
datalie2 <- datalie2 %>% 
  dplyr::mutate(utility2 = parameter[1] + parameter[2]*所要時間 + parameter[3]*費用
                + parameter[4]*log(Freq) + parameter[5]*滞在可能時間 ## 1日の便数ならFreqではなく航空便数!
                + parameter[6]*アクセシビリティ + parameter[7]*鉄道ダミー
                + parameter[8]*エアライン参入数) %>% 
  dplyr::mutate(exputility = exp(utility2))
datalie2 <- datalie2 %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(alpha = calculate.alpha(observedprob, exputility)) %>% 
  dplyr::ungroup()

## Write functions
## prob is a function which returns Pr
prob <- function(a){
  exputility <- rep(0, nrow(datalie2))
  sumexputility <- rep(0, nrow(datalie2))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (a[k]*exp(parameter[1] + parameter[2]*datalie2$所要時間[k] + parameter[3]*Price[k]
                               + parameter[4]*log(Frequency[k]) + parameter[5]*datalie2$滞在可能時間[k]
                               + parameter[6]*datalie2$アクセシビリティ[k] + parameter[7]*datalie2$鉄道ダミー[k]
                               + parameter[8]*datalie2$エアライン参入数[k]))
  }
  sumexputility <- data.frame(i = datalie2$i, j = datalie2$j, exputility = exputility)
  sumexputility <- sumexputility %>% 
    dplyr::group_by(i, j) %>% 
    dplyr::mutate(sumexputility = sum(exputility)) %>% 
    dplyr::ungroup()
  sumexputility <- sumexputility$sumexputility
  sumexputility <- as.numeric(sumexputility)
  output <- exputility/sumexputility
  return(output)
}

## Objective is a function used when maximizing profits 
Objective <- function(p){
  exputility <- rep(0, nrow(datalie2))
  sumexputility <- rep(0, nrow(datalie2))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (datalie2$alpha[k]*exp(parameter[1] + parameter[2]*datalie2$所要時間[k] + parameter[3]*Price[k]
                                            + parameter[4]*log(Frequency[k]) + parameter[5]*datalie2$滞在可能時間[k]
                                            + parameter[6]*datalie2$アクセシビリティ[k] + parameter[7]*datalie2$鉄道ダミー[k]
                                            + parameter[8]*datalie2$エアライン参入数[k]))
  }
  exputility[c(which(datalie2$airlineNo == n))] <- exputility[c(which(datalie2$airlineNo == n))]*exp(-parameter[3]*Price[c(which(datalie2$airlineNo == n))])
  exputility[c(which(datalie2$airlineNo == n))] <- exputility[c(which(datalie2$airlineNo == n))]*exp(-parameter[3]*p)
  sumexputility <- data.frame(i = datalie2$i, j = datalie2$j, exputility = exputility)
  sumexputility <- sumexputility %>% 
    dplyr::group_by(i, j) %>% 
    dplyr::mutate(sumexputility = sum(exputility)) %>% 
    dplyr::ungroup()
  sumexputility <- sumexputility$sumexputility
  sumexputility <- as.numeric(sumexputility)
  demand2 <- rep(0, nrow(datalie2))
  for(i in 1:nrow(datalie2)){
    demand2[i] <- (datalie2[i, 14])*(exputility[i]/sumexputility[i])
  }
  demand2 <- demand2[c(which(datalie2$airlineNo == n))]
  demand2 <- as.numeric(demand2)
  MC2 <- datalie2$MC
  MC2 <- MC[c(which(datalie2$airlineNo == n))]
  output <- sum((p - MC2)*demand2)
  return(output)
}



## Write a function which returns first order derivative
partialderiv <- function(a){
  exputility <- c(rep(0, nrow(datalie2)))
  sumexputility <- c(rep(0, nrow(datalie2)))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (a[k]*exp(parameter[1] + parameter[2]*datalie2$所要時間[k] + parameter[3]*Price[k]
                               + parameter[4]*log(Frequency[k]) + parameter[5]*datalie2$滞在可能時間[k]
                               + parameter[6]*datalie2$アクセシビリティ[k] + parameter[7]*datalie2$鉄道ダミー[k]
                               + parameter[8]*datalie2$エアライン参入数[k]))
  }
  sumexputility <- data.frame(i = datalie2$i, j = datalie2$j, exputility = exputility)
  sumexputility <- sumexputility %>% 
    dplyr::group_by(i, j) %>% 
    dplyr::mutate(sumexputility = sum(exputility)) %>% 
    dplyr::ungroup()
  sumexputility <- sumexputility[ , 4]
  exputility <- as.numeric(exputility)
  sumexputility <- as.numeric(sumexputility$sumexputility)
  output <- matrix(0, nrow = nrow(datalie2), ncol = nrow(datalie2))
  for(i in 1:nrow(datalie2)){
    for(j in 1:nrow(datalie2)){
      if(datalie2[i, 1] == datalie2[j, 1] && datalie2[i, 2] == datalie2[j, 2] && datalie2[i, 5] == datalie2[j, 5]){
        output[i + nrow(datalie2)*(j - 1)] <- (-1)*(parameter[3]*exputility[i]*exputility[j])/((sumexputility[i])^2)
      } else {
        
      }
    }
  }
  for(i in 1:nrow(datalie2)){
    output[i + nrow(datalie2)*(i - 1)] <- (parameter[3]*exputility[i]*(sumexputility[i] - exputility[i]))/((sumexputility[i])^2)
  }
  return(output)
}
write.csv(datalie2, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/datalie3.csv")

## Define vectors of endogenous variables called Price and Frequency
Price <- as.numeric(datalie2$費用/2) # As a initial value we give real data and devided by two so that it represents one-way
Frequency <- as.numeric(datalie2$Freq) # ここ書き換えた
Frequency <- ifelse(is.na(Frequency), 1, Frequency)
Pricerenewal <- c(rep(0, nrow(datalie2)))
Frequencyrenewal <- c(rep(0, nrow(datalie2)))
Demand <- as.numeric(datalie2$旅客数) ## ここ書き換えた（また書き換えた）
Profit <- c(rep(0, 5))
Profitrenewal <- c(rep(0, 5))

## =======================================================================================
## Stopping criteria
distance3 <- 1000
iter3 <- 1
while(((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)){ ## ((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)
  for(n in 1:5){ ## 分析対象のエアライン数
    ## Drop airlines other than interested
    Unit <- diag(nrow(datalie2)) ## This is necessary for removing airlines out of interest
    for(i in 1:nrow(datalie2)){
      if(datalie2[i, 24] != n){
        Unit[i + nrow(datalie2)*(i - 1)] <-0
      }
    }
    
    ## Optimization
    ## First we calculate Freq
    ## Stopping creiteria
    distance <- 1000
    iter <- 1
    while(((distance > 100) & (iter < it)) | (iter < 3)){
      Pr <- prob(datalie2$alpha) 
      for(i in 1:nrow(datalie2)){
        if(datalie2[i, 24] == n){ # AielineNo
          Demand[i] <- datalie2[i, 14]*Pr[i]                                            ### ここ書き換えた（また）
          Frequencyrenewal[i] <- (Demand[i]/(datalie2[i, 20]*datalie2[i, 23]/100))          ### ここ書き換えた（また）
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
    profit.maximization <- optim(par = Price[c(which(datalie2$airlineNo == n))], fn = Objective, control = list(fnscale = -1))
    Price[c(which(datalie2$airlineNo == n))] <- profit.maximization$par
    Profitrenewal[n] <- profit.maximization$value
  }
  
  distance3 <- sqrt(sum((Profitrenewal - Profit)^2))
  iter3 <- iter3 + 1
  Profit <- Profitrenewal
}

Demand <- unlist(Demand)
simulation.results <- data.frame(Price_simulation = Price, Price_observed = datalie2$費用, 
                                 Frequency_simulation = Frequency, Frequency_observed = datalie2$Freq, 
                                 Probability_simulation = Pr, Probability_observed = datalie2$observedprob, 
                                 Demand_simulation = Demand, Demand_observed = datalie2$旅客数)
simulation.results <- cbind(datalie2[ , c(1, 2, 3, 4, 5)], simulation.results)
simulation.results$Price_observed <- simulation.results$Price_observed/2
write.csv(simulation.results, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/simulation_results.csv")

## あとでやる
## Demand[c(which(datalie2$航空会社 == "JR"))] <- datalie2[c(which(datalie2$航空会社 == "JR")), 14]*Pr[c(which(datalie2$航空会社 == "JR"))]
## Demand <- unlist(Demand)


