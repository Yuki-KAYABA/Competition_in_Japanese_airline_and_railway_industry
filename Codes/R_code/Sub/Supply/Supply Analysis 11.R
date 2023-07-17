## This code should be used when you want to simulate the model using utility function constructed by
## Intercept, ��p, log(�q��֐�), �A�N�Z�V�r���e�B, �S���_�~�[, ��ւ�������Ȑ�, ��ւ�����ݕ�, ���v����

## Set the numbers of iteration
it <- 10000
it2 <- 10000 ## 500?
it3 <- 10000 ## 100?

## Utility parameters
parameter <- IVreg7$coefficients ## utilitypar or abcdef or IVpar of par.sign or utilitypar.quad

## Library
library(magrittr)
library(tidyr)

## New data
datalie2 <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data_updated.csv", header = T)

## Attach new airlineNo
airlineNo2 <- c(seq(1:10)) ## �G�A���C�����ς�������͂���
airlinenames2 <- c("JAL", "ANA", "SFJ", "SKY", "JAC", "ADO", "IBX", "JTA", "HAC", "JEX") ## �G�A���C�����ς�������͂���
airlinecorrespondence2 <- data.frame(airlineNo = airlineNo2, airlinenames = airlinenames2)
for(i in 1:nrow(datalie2)){
  for(j in 1:nrow(airlinecorrespondence2)){
    if(datalie2[i, 6] == airlinecorrespondence2[j, 2]){
      datalie2[i, 24] <- airlinecorrespondence2[j, 1]
    }
  }
}
datalie2[, 24] <- ifelse(is.na(datalie2[ , 24]), 0, datalie2[ , 24])
colnames(datalie2)[24] <- "airlineNo"
# s[3]�����v���ԁAs[2]����p�������ϐ�

## Prepare adjustment parameter alpha
calculate.alpha <- function(x, y){
  X <<- x %*% t(y) - diag(y)
  L <<- X
  L[upper.tri(L)] <<- 0
  U <<- X
  U[lower.tri(U, diag = T)] <<- 0
  iteration <- 1000
  xx <<- matrix(rep(1, length(x)), nrow = length(x), ncol = 1) ## Initial value
  for(k in 1:iteration){
    xx <<- (-1)*solve(L) %*% U %*% xx
  }
  output <<- xx[, 1]/xx[1,1]
  return(output)
}
## ����
## parameter[1] + parameter[2]*��p + parameter[3]*log(Freq) + parameter[4]*�A�N�Z�V�r���e�B + parameter[5]*�S���_�~�[ + parameter[6]*��ւ�������Ȑ� + parameter[7]*��ւ�����ݕ� + parameter[8]*���v����
## Make alpha
datalie2$Freq <- ifelse(is.na(datalie2$Freq), 1, datalie2$Freq)
datalie2 <- datalie2 %>% 
  dplyr::mutate(utility2 = parameter[1] + parameter[2]*��p 
                + parameter[3]*log(Freq) + parameter[4]*�A�N�Z�V�r���e�B + parameter[5]*�S���_�~�[ 
                + parameter[6]*��ւ�������Ȑ� + parameter[7]*��ւ�����ݕ� + parameter[8]*���v����) %>% 
  dplyr::mutate(exputility = exp(utility2))
datalie2 <- datalie2 %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(alpha = calculate.alpha(observedprob, exputility)) %>% 
  dplyr::ungroup()
## When you want to set alpha to 1, excute the following:
#datalie2 <- datalie2 %>% 
#  dplyr::mutate(alpha = 1)
write.csv(datalie2, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/datalie3.csv")

## Write functions
## prob is a function which returns Pr
prob <- function(a){
  exputility <- rep(0, nrow(datalie2))
  sumexputility <- rep(0, nrow(datalie2))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (a[k]*exp(parameter[1] + parameter[2]*Price[k] + parameter[3]*log(Frequency[k]) 
                               + parameter[4]*datalie2$�A�N�Z�V�r���e�B[k] + parameter[5]*datalie2$�S���_�~�[[k] 
                               + parameter[6]*datalie2$��ւ�������Ȑ�[k] + parameter[7]*datalie2$��ւ�����ݕ�[k] + parameter[8]*datalie2$���v����[k]))
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
## �ϐ��̑����ɍ��킹�ĐF�X���������Ă���̂Œ���
Objective <- function(p){
  exputility <- rep(0, nrow(datalie2))
  sumexputility <- rep(0, nrow(datalie2))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (datalie2$alpha[k]*exp(parameter[1] + parameter[2]*Price[k] + parameter[3]*log(Frequency[k]) 
                                            + parameter[4]*datalie2$�A�N�Z�V�r���e�B[k] + parameter[5]*datalie2$�S���_�~�[[k]
                                            + parameter[6]*datalie2$��ւ�������Ȑ�[k] + parameter[7]*datalie2$��ւ�����ݕ�[k] + parameter[8]*datalie2$���v����[k]))
  }
  exputility[c(which(datalie2$airlineNo == n))] <- exputility[c(which(datalie2$airlineNo == n))]*exp(-parameter[2]*Price[c(which(datalie2$airlineNo == n))])
  exputility[c(which(datalie2$airlineNo == n))] <- exputility[c(which(datalie2$airlineNo == n))]*exp(parameter[2]*p)
  sumexputility <- data.frame(i = datalie2$i, j = datalie2$j, exputility = exputility)
  sumexputility <- sumexputility %>% 
    dplyr::group_by(i, j) %>% 
    dplyr::mutate(sumexputility = sum(exputility)) %>% 
    dplyr::ungroup()
  sumexputility <- sumexputility$sumexputility
  sumexputility <- as.numeric(sumexputility)
  demand2 <- rep(0, nrow(datalie2))
  for(i in 1:nrow(datalie2)){
    demand2[i] <- (datalie2[i, 17])*(exputility[i]/sumexputility[i]) ## �񐔏C������
  }
  demand2 <- demand2[c(which(datalie2$airlineNo == n))] ## Freq������������
  demand2 <- as.numeric(demand2)
  MC2 <- datalie2$MC
  MC2 <- MC2[c(which(datalie2$airlineNo == n))] ## Freq������������
  output <- sum((p - MC2)*demand2)
  return(output)
}
## Define vectors of endogenous variables called Price and Frequency
Price <- as.numeric(datalie2$��p/2) # �����v�����BJR�܂�2�Ŋ����Ă悢�̂�
Frequency <- as.numeric(datalie2$Freq) # ��������������
Frequency <- ifelse(is.na(Frequency), 1, Frequency)
Pricerenewal <- c(rep(0, nrow(datalie2)))
Frequencyrenewal <- c(rep(0, nrow(datalie2)))
Demand <- as.numeric(datalie2$���q��) ## ���������������i�܂������������j
Profit <- c(rep(0, 10))
Profitrenewal <- c(rep(0, 10))
Frequency.matrix <- matrix(0, nrow = 100000, ncol = nrow(datalie2)) 
Frequency.matrix[1, ] <- Frequency
Price.matrix <- matrix(0, nrow = 100000, ncol = nrow(datalie2))
Price.matrix[1, ] <- Price
Profit.matrix <- matrix(0, nrow = 100000, ncol = 10) ## �Q����

## =======================================================================================
## Stopping criteria
distance3 <- 1000
iter3 <- 1
count <- 1
count.price <- 1
count.profit <- 1

system.time(
  while(((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)){ ## ((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)
    for(n in 1:10){ ## ���͑Ώۂ̃G�A���C����
      ## Drop airlines other than interested
      Unit <- diag(nrow(datalie2)) ## This is necessary for removing airlines out of interest
      for(i in 1:nrow(datalie2)){
        if(datalie2[i, 24] != n){
          Unit[i + nrow(datalie2)*(i - 1)] <-0
        }
      }
      
      ## Optimization
      distance2 <- 1000
      iter2 <- 1
      while(((distance2 > 1e-4) & (iter2 < it2)) | (iter2 < 3)){
        ## First we calculate Freq
        ## Stopping creiteria
        distance <- 1000
        iter <- 1
        while(((distance > 1e-4) & (iter < it)) | (iter < 3)){
          Pr <- prob(datalie2$alpha) 
          for(i in 1:nrow(datalie2)){
            if(datalie2[i, 24] == n){ # AirlineNo
              Demand[i] <- datalie2[i, 17]*Pr[i]                                            ### ���������������i��j
              Frequencyrenewal[i] <- (Demand[i]/(datalie2[i, 19]*datalie2[i, 22]/100))          ### ���������������i��j
            } else {
              Frequencyrenewal[i] <- Frequency[i]
            }
          }
          Frequency <- as.numeric(Frequency)
          Frequencyrenewal <- as.numeric(Frequencyrenewal)
          ## �N��1�ւ���������ꍇ��1�ւƂ������Ƃɂ���
          Frequencyrenewal <- ifelse(Frequencyrenewal < 1, 1, Frequencyrenewal)
          distance <- sqrt(sum(Frequency - Frequencyrenewal)^2)
          Frequency.matrix[count + 1, ] <- Frequencyrenewal
          Frequency <- Frequencyrenewal
          iter <- iter + 1
          count <- count + 1
        }
        
        ## Next we calculate Price
        profit.maximization <- optim(par = Price[c(which(datalie2$airlineNo == n))], fn = Objective, control = list(fnscale = -1))
        Pricerenewal[c(which(datalie2$airlineNo == n))] <- profit.maximization$par
        distance2 <- sqrt(sum(Price[c(which(datalie2$airlineNo == n))] - Pricerenewal[c(which(datalie2$airlineNo == n))])^2)
        Price.matrix[count.price + 1, ] <- Pricerenewal
        Profitrenewal[n] <- profit.maximization$value
        Price <- Pricerenewal
        iter2 <- iter2 + 1
        count.price <- count.price + 1
      }
      
    }
    Profit.matrix[count.profit, ] <- Profitrenewal
    distance3 <- sqrt(sum((Profitrenewal - Profit)^2))
    iter3 <- iter3 + 1
    Profit <- Profitrenewal
    count.profit <- count.profit + 1
  }
)

Frequency.matrix <- Frequency.matrix[c(seq(1:(count))), ]
Price.matrix <- Price.matrix[c(seq(1:(count.price))), ]
Profit.matrix <- Profit.matrix[c(seq(1:(count.profit - 1))), ]
Demand <- unlist(Demand)
simulation.results <- data.frame(Price_simulation = Price, Price_observed = datalie2$��p, 
                                 Frequency_simulation = Frequency, Frequency_observed = datalie2$Freq, 
                                 Probability_simulation = Pr, Probability_observed = datalie2$observedprob, 
                                 Demand_simulation = Demand, Demand_observed = datalie2$���q��)
simulation.results <- cbind(datalie2[ , c(2, 3, 4, 5, 6)], simulation.results)
simulation.results$Price_observed <- simulation.results$Price_observed/2
simulation.results$Demand_simulation[c(which(simulation.results$�q���� == "JR"))] <- simulation.results$Demand_observed[c(which(simulation.results$�q���� == "JR"))]*simulation.results$Probability_simulation[c(which(simulation.results$�q���� == "JR"))]/simulation.results$Probability_observed[c(which(simulation.results$�q���� == "JR"))] 

write.csv(simulation.results, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/simulation_results.csv")

## Write some functions
## Plot Frequency
Frequency.series <- function(xi, xj){ ## n���G�A���C��No, (i, j)���n��, 0�Ŏw��Ȃ�
  x <- Frequency.matrix
  x <-  x[, c(which(datalie2$i == xi & datalie2$j == xj))] # ����
  rownames(x) <- 1:nrow(x)
  colnames(x) <- 1:ncol(x)
  y <- reshape2::melt(x)
  colnames(y) <- c("count", "route", "frequency")
  
  graph <- ggplot2::ggplot(y, ggplot2::aes(x = count, y = frequency, color = as.factor(route)))
  graph <- graph + ggplot2::geom_line()
  graph <- graph + ggsci::scale_color_nejm()
  output <- ggplot2::ggsave(file = paste("�q��֐� ", xi, "-", xj, ".png", sep = ""), plot(graph))
  return(output)
}
## paste(i, j, sep = "-")
## Plot Price
Price.series <- function(xi, xj){ ## n���G�A���C��No, (i, j)���n��, 0�Ŏw��Ȃ�
  x <- Price.matrix
  x <-  x[, c(which(datalie2$i == xi & datalie2$j == xj))] # ����
  rownames(x) <- 1:nrow(x)
  colnames(x) <- 1:ncol(x)
  y <- reshape2::melt(x)
  colnames(y) <- c("count", "route", "price")
  
  graph <- ggplot2::ggplot(y, ggplot2::aes(x = count, y = price, color = as.factor(route)))
  graph <- graph + ggplot2::geom_line()
  graph <- graph + ggsci::scale_color_nejm()
  output <- ggplot2::ggsave(file = paste("�^�� ", xi, "-", xj, ".png", sep = ""), plot(graph))
  return(output)
}
## Profit
Profit.series <- function(n){ ## n�ɉ�����悤���֌W�Ȃ�
  x <- Profit.matrix
  rownames(x) <- 1:nrow(x)
  colnames(x) <- 1:ncol(x)
  y <- reshape2::melt(x)
  colnames(y) <- c("count", "airline", "profit")
  
  graph <- ggplot2::ggplot(y, ggplot2::aes(x = count, y = profit, color = as.factor(airline)))
  graph <- graph + ggplot2::geom_line()
  graph <- graph + ggsci::scale_color_nejm()
  output <- ggplot2::ggsave(file = "����.png", plot(graph))
  return(output)
}


## Substitution matrix
Price.Substitution.Matrix <- function(xi, xj){
  library(magrittr)
  x <- datalie2 %>% 
    dplyr::filter(i == xi, j == xj) %>% 
    dplyr::mutate(prob2 = 1 - observedprob)
  xx <- -parameter[2]*t(x$��p*x$observedprob)
  mat <- matrix(0, nrow = ncol(xx), ncol = ncol(xx))
  for(i in 1:nrow(x)){
    mat[i, ] <- xx
  }
  diag(mat) <- parameter[2]*(x$��p*x$prob2)
  output <- mat
  return(output)
}
Price.Substitution.Matrix(25, 37)

Frequency.Substitution.Matrix <- function(xi, xj){
  library(magrittr)
  x <- datalie2 %>% 
    dplyr::filter(i == xi, j == xj) %>% 
    dplyr::mutate(prob2 = 1 - observedprob)
  xx <- -parameter[3]*as.matrix(x$observedprob)
  xx <- t(xx)
  mat <- matrix(0, nrow = ncol(xx), ncol = ncol(xx))
  for(i in 1:nrow(x)){
    mat[i, ] <- xx
  }
  diag(mat) <- parameter[3]*x$prob2
  output <- mat
  return(output)
}
Frequency.Substitution.Matrix(18, 37)

## index
index <- subset(datalie2, , c(i, j)) %>% unique() %>% as.matrix() 
## save graph
for(k in 1:nrow(index)){
  Frequency.series(as.numeric(index[k, 1]), as.numeric(index[k, 2]))
  Price.series(as.numeric(index[k, 1]), as.numeric(index[k, 2]))
}
Profit.series(0)

#profit.counterfactual <- data.frame(Airline = airlinenames2, Profit = Profit)
#write.csv(profit.counterfactual, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/profit_counterfactual.csv")