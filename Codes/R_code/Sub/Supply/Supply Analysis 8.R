## This code should be used when you want to simulate the model using utility function constructed by
## 費用, log(航空便数), アクセシビリティ, 鉄道ダミー

## Set the numbers of iteration
it <- 10000
it2 <- 10000 ## 500?
it3 <- 10000 ## 100?

## Utility parameters
liepar <- logitpar
liepar[3] <- 0.01
parameter <- IVpar2 ## utilitypar or abcdef or IVpar of par.sign or utilitypar.quad

## Library
library(magrittr)
library(tidyr)

## New data
datalie2 <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data_updated.csv", header = T)

## Read functions
.myfunc.env = new.env()
source("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Codes/Functions.R")
attach( .myfunc.env )

## Attach new airlineNo
airlineNo2 <- c(seq(1:8))
airlinenames2 <- c("JAL", "ANA", "SFJ", "SKY", "JAC", "ADO", "IBX", "JTA")
airlinecorrespondence2 <- data.frame(airlineNo = airlineNo2, airlinenames = airlinenames2)
for(i in 1:nrow(datalie2)){
  for(j in 1:nrow(airlinecorrespondence2)){
    if(datalie2[i, 6] == airlinecorrespondence2[j, 2]){
      datalie2[i, 22] <- airlinecorrespondence2[j, 1]
    }
  }
}
datalie2[, 22] <- ifelse(is.na(datalie2[ , 22]), 0, datalie2[ , 22])
colnames(datalie2)[22] <- "airlineNo"
# s[3]が所要時間、s[2]が費用←内生変数

## Make alpha
datalie2$Freq <- ifelse(is.na(datalie2$Freq), 1, datalie2$Freq)
datalie2 <- datalie2 %>% 
  dplyr::mutate(utility2 = parameter[1] + parameter[2]*費用 
                + parameter[3]*log(Freq) + parameter[4]*アクセシビリティ + parameter[5]*鉄道ダミー) %>% 
  dplyr::mutate(exputility = exp(utility2))
datalie2 <- datalie2 %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(alpha = calculate.alpha(observedprob, exputility)) %>% 
  dplyr::ungroup()

## Define vectors of endogenous variables called Price and Frequency
Price <- as.numeric(datalie2$費用/2) # ここ要検討。JRまで2で割ってよいのか
Frequency <- as.numeric(datalie2$Freq) # ここ書き換えた
Frequency <- ifelse(is.na(Frequency), 1, Frequency)
Pricerenewal <- c(rep(0, nrow(datalie2)))
Frequencyrenewal <- c(rep(0, nrow(datalie2)))
Demand <- as.numeric(datalie2$旅客数) ## ここ書き換えた（また書き換えた）
Profit <- c(rep(0, 8))
Profitrenewal <- c(rep(0, 8))
Frequency.matrix <- matrix(0, nrow = 100000, ncol = nrow(datalie2)) 
Frequency.matrix[1, ] <- Frequency
Price.matrix <- matrix(0, nrow = 100000, ncol = nrow(datalie2))
Price.matrix[1, ] <- Price
Profit.matrix <- matrix(0, nrow = 100000, ncol = 8) ## 参入数

## =======================================================================================
## Stopping criteria
distance3 <- 1000
iter3 <- 1
## count <- 1
count.price <- 1
count.profit <- 1

## Simulation =======================================================================================================
system.time(
  while(((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)){
    for(n in 1:8){ ## エアライン数に応じて変える
      profit.maximization <- optim(par = Price[c(which(datalie2$airlineNo == n & Frequency > 1e-4))], fn = Objective2, control = list(fnscale = -1))
      Pricerenewal[c(which(datalie2$airlineNo == n & Frequency > 1e-4))] <- profit.maximization$par
      Pricerenewal[c(which(datalie2$airlineNo == n & Frequency <= 1e-4))] <- 0
      ## distance2 <- sqrt(sum(Price[c(which(datalie2$airlineNo == n & Frequency > 1e-4))] - Pricerenewal[c(which(datalie2$airlineNo == n & Frequency > 1e-4))])^2)
      Price.matrix[count.price + 1, ] <- Pricerenewal
      Profitrenewal[n] <- profit.maximization$value
      Price <- Pricerenewal
      iter2 <- iter2 + 1
      count.price <- count.price + 1
    }
    Profit.matrix[count.profit, ] <- Profitrenewal
    distance3 <- sqrt(sum((Profitrenewal - Profit)^2))
    iter3 <- iter3 + 1
    Profit <- Profitrenewal
    count.profit <- count.profit + 1
  }
)
## ===================================================================================================================

Frequency.matrix <- Frequency.matrix[c(seq(1:(count))), ]
Price.matrix <- Price.matrix[c(seq(1:(count.price))), ]
Profit.matrix <- Profit.matrix[c(seq(1:(count.profit - 1))), ]
Demand <- unlist(Demand)
simulation.results <- data.frame(Price_simulation = Price, Price_observed = datalie2$費用, 
                                 Frequency_simulation = Frequency, Frequency_observed = datalie2$Freq, 
                                 Probability_simulation = Pr, Probability_observed = datalie2$observedprob, 
                                 Demand_simulation = Demand, Demand_observed = datalie2$旅客数)
simulation.results <- cbind(datalie2[ , c(2, 3, 4, 5, 6)], simulation.results)
simulation.results$Price_observed <- simulation.results$Price_observed/2
simulation.results$Demand_simulation[c(which(simulation.results$航空会社 == "JR"))] <- simulation.results$Demand_observed[c(which(simulation.results$航空会社 == "JR"))]*simulation.results$Probability_simulation[c(which(simulation.results$航空会社 == "JR"))]/simulation.results$Probability_observed[c(which(simulation.results$航空会社 == "JR"))] 

write.csv(simulation.results, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/simulation_results.csv")

## index
index <- subset(datalie2, , c(i, j)) %>% unique() %>% as.matrix() 
## save graph
for(k in 1:nrow(index)){
  Frequency.series(as.numeric(index[k, 1]), as.numeric(index[k, 2]))
  Price.series(as.numeric(index[k, 1]), as.numeric(index[k, 2]))
}
Profit.series(0)

