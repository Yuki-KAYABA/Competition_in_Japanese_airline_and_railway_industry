## Set the numbers of iteration
it <- 3
it2 <- 10 ## 500?
it3 <- 5 ## 100?

## Lie parameters
abcdef <- c(-1.18e-2, -5.87e-5, 5.80e-1, 5.50e-4, -8.55e-1, 3.37e0, 6.06e-1)

## Library
library(magrittr)
library(tidyr)

## New data
datalie2 <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/datalie2.csv", header = T)

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
# s[3]�����v���ԁAs[2]����p�������ϐ�

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
  dplyr::mutate(utility2 = utilitypar[1] + utilitypar[2]*���v���� + utilitypar[3]*��p
                + utilitypar[4]*log(Freq) + utilitypar[5]*�؍݉\���� ## 1���̕֐��Ȃ�Freq�ł͂Ȃ��q��֐�!
                + utilitypar[6]*�A�N�Z�V�r���e�B + utilitypar[7]*�S���_�~�[
                + utilitypar[8]*�G�A���C���Q����) %>% 
  dplyr::mutate(exputility = exp(utility2)) %>% 
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
    exputility[k] <- (a[k]*exp(utilitypar[1] + utilitypar[2]*datalie2$���v����[k] + utilitypar[3]*Price[k]
                            + utilitypar[4]*log(Frequency[k]) + utilitypar[5]*datalie2$�؍݉\����[k]
                            + utilitypar[6]*datalie2$�A�N�Z�V�r���e�B[k] + utilitypar[7]*datalie2$�S���_�~�[[k]
                            + utilitypar[8]*datalie2$�G�A���C���Q����[k]))
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

## Write a function which returns first order derivative
partialderiv <- function(a){
  exputility <- c(rep(0, nrow(datalie2)))
  sumexputility <- c(rep(0, nrow(datalie2)))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (a[k]*exp(utilitypar[1] + utilitypar[2]*datalie2$���v����[k] + utilitypar[3]*Price[k]
                            + utilitypar[4]*log(Frequency[k]) + utilitypar[5]*datalie2$�؍݉\����[k]
                            + utilitypar[6]*datalie2$�A�N�Z�V�r���e�B[k] + utilitypar[7]*datalie2$�S���_�~�[[k]
                            + utilitypar[8]*datalie2$�G�A���C���Q����[k]))
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
        output[i + nrow(datalie2)*(j - 1)] <- (-1)*(utilitypar[3]*exputility[i]*exputility[j])/((sumexputility[i])^2)
      } else {
        
      }
    }
  }
  for(i in 1:nrow(datalie2)){
    output[i + nrow(datalie2)*(i - 1)] <- (utilitypar[3]*exputility[i]*(sumexputility[i] - exputility[i]))/((sumexputility[i])^2)
  }
  return(output)
}
write.csv(datalie2, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/datalie3.csv")
## Define vectors of endogenous variables called Price and Frequency
Price <- as.numeric(datalie2$��p) # As a initial value we give real data
Frequency <- as.numeric(datalie2$Freq) # ��������������
Frequency <- ifelse(is.na(Frequency), 1, Frequency)
Pricerenewal <- c(rep(0, nrow(datalie2)))
Frequencyrenewal <- c(rep(0, nrow(datalie2)))
Demand <- as.numeric(datalie2$���q��) ## ���������������i�܂������������j
Profit <- c(rep(0, 5))
Profitrenewal <- c(rep(0, 5))

## =======================================================================================
## Stopping criteria
distance3 <- 1000
iter3 <- 1
while(((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)){ ## ((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)
  for(n in 1:5){ ## ���͑Ώۂ̃G�A���C����
    ## Drop airlines other than interested
    Unit <- diag(nrow(datalie2)) ## This is necessary for removing airlines out of interest
    for(i in 1:nrow(datalie2)){
      if(datalie2[i, 24] != n){
        Unit[i + nrow(datalie2)*(i - 1)] <-0
      }
    }
    
    ## Optimization
    ## Stopping Criteria
    distance2 <- 1000
    iter2 <- 1
    sign <- -5
    ## First Order Condition
    while(((distance2 > 10) & (iter2 < it2)) | (sign < 0)){  ## ((distance2 > 10) & (iter2 < it2)) | (sign < 0)
      ## First we calculate Freq
      ## Stopping creiteria
      distance <- 1000
      iter <- 1
      while((distance > 100) & (iter < it)){
        Pr <- prob(datalie2$alpha) 
        for(i in 1:nrow(datalie2)){
          if(datalie2[i, 24] == n){ # AielineNo
            Demand[i] <- datalie2[i, 14]*Pr[i]                                            ### ���������������i�܂��j
            Frequencyrenewal[i] <- (Demand[i]/(datalie2[i, 20]*datalie2[i, 23]/100))          ### ���������������i�܂��j
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
      Derivmatrix <- partialderiv(datalie2$alpha)
      Demand <- as.numeric(Demand)
      MC <- as.matrix(datalie2$MC)
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
      lambda <- 10 ## regularization parameter
      airlinePrice <- airlineMC + (-1)*solve(airlineDerivmatrix + lambda*diag(length(colindex))) %*% airlineDemand
      airlinePrice <- airlinePrice[ , 1]
      Pricerenewal <- Price
      for(i in 1:length(colindex)){
        Pricerenewal[colindex[i]] <- airlinePrice[i]
      }
      Price <- Pricerenewal
      distance2 <- sqrt(sum(FOC)^2)
      sign <- sum(sign(Price)) - as.numeric(length(Price))
      iter2 <- iter2 + 1 
      Profitrenewal[n] <- t((airlinePrice - airlineMC)) %*% airlineDemand
    }
    
  }
  
  distance3 <- sqrt(sum((Profitrenewal - Profit)^2))
  iter3 <- iter3 + 1
  Profit <- Profitrenewal
}




simulation.results <- data.frame(Price_simulation = Pricerenewal, Price_observed = datalie2$��p, 
                                 Frequency_simulation = Frequency, Frequency_observed = datalie2$�q��֐�, 
                                 Probability_simulation = Pr, Probability_observed = datalie2$observedprob, 
                                 Demand_simulation = Demand, Demand_observed = datalie2$���q��/365)
simulation.results <- cbind(datalie2[ , c(1, 2, 3, 4, 5)], simulation.results)
write.csv(simulation.results, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/simulation_results.csv")


