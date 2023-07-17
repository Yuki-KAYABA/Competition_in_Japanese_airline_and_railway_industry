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
  dplyr::mutate(utility2 = parameter[1] + parameter[2]*費用 
                + parameter[3]*log(Freq) + parameter[4]*アクセシビリティ + parameter[5]*鉄道ダミー) %>% 
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
    exputility[k] <- (a[k]*exp(parameter[1] + parameter[2]*Price[k] + parameter[3]*log(Frequency[k]) 
                               + parameter[4]*datalie2$アクセシビリティ[k] + parameter[5]*datalie2$鉄道ダミー[k]))
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
    exputility[k] <- (datalie2$alpha[k]*exp(parameter[1] + parameter[2]*Price[k] + parameter[3]*log(Frequency[k]) 
                                            + parameter[4]*datalie2$アクセシビリティ[k] + parameter[5]*datalie2$鉄道ダミー[k]))
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
    demand2[i] <- (datalie2[i, 15])*(exputility[i]/sumexputility[i]) ## 列数修正した
  }
  demand2 <- demand2[c(which(datalie2$airlineNo == n))] ## Freq条件書き足し
  demand2 <- as.numeric(demand2)
  MC2 <- datalie2$MC
  MC2 <- MC2[c(which(datalie2$airlineNo == n))] ## Freq条件書き足し
  output <- sum((p - MC2)*demand2)
  return(output)
}

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
count <- 1
count.price <- 1
count.profit <- 1

system.time(
  while(((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)){ ## ((distance3 > 1e-4) & (iter3 < it3)) | (iter3 < 3)
    for(n in 1:8){ ## 分析対象のエアライン数
      ## Drop airlines other than interested
      Unit <- diag(nrow(datalie2)) ## This is necessary for removing airlines out of interest
      for(i in 1:nrow(datalie2)){
        if(datalie2[i, 22] != n){
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
            if(datalie2[i, 22] == n){ # AirlineNo
              Demand[i] <- datalie2[i, 15]*Pr[i]                                            ### ここ書き換えた（列）
              Frequencyrenewal[i] <- (Demand[i]/(datalie2[i, 17]*datalie2[i, 20]/100))          ### ここ書き換えた（列）
            } else {
              Frequencyrenewal[i] <- Frequency[i]
            }
          }
          Frequency <- as.numeric(Frequency)
          Frequencyrenewal <- as.numeric(Frequencyrenewal)
          distance <- sqrt(sum(Frequency - Frequencyrenewal)^2)
          Frequency.matrix[count + 1, ] <- Frequencyrenewal
          Frequency <- Frequencyrenewal
          iter <- iter +1
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
simulation.results <- data.frame(Price_simulation = Price, Price_observed = datalie2$費用, 
                                 Frequency_simulation = Frequency, Frequency_observed = datalie2$Freq, 
                                 Probability_simulation = Pr, Probability_observed = datalie2$observedprob, 
                                 Demand_simulation = Demand, Demand_observed = datalie2$旅客数)
simulation.results <- cbind(datalie2[ , c(2, 3, 4, 5, 6)], simulation.results)
simulation.results$Price_observed <- simulation.results$Price_observed/2
simulation.results$Demand_simulation[c(which(simulation.results$航空会社 == "JR"))] <- simulation.results$Demand_observed[c(which(simulation.results$航空会社 == "JR"))]*simulation.results$Probability_simulation[c(which(simulation.results$航空会社 == "JR"))]/simulation.results$Probability_observed[c(which(simulation.results$航空会社 == "JR"))] 

write.csv(simulation.results, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/simulation_results.csv")

## Write some functions
## Plot Frequency
Frequency.series <- function(xi, xj){ ## nがエアラインNo, (i, j)が地域, 0で指定なし
  x <- Frequency.matrix
  x <-  x[, c(which(datalie2$i == xi & datalie2$j == xj))] # 直す
  rownames(x) <- 1:nrow(x)
  colnames(x) <- 1:ncol(x)
  y <- reshape2::melt(x)
  colnames(y) <- c("count", "route", "frequency")
  
  graph <- ggplot2::ggplot(y, ggplot2::aes(x = count, y = frequency, color = as.factor(route)))
  graph <- graph + ggplot2::geom_line()
  graph <- graph + ggsci::scale_color_nejm()
  output <- ggplot2::ggsave(file = paste("航空便数 ", xi, "-", xj, ".png", sep = ""), plot(graph))
  return(output)
}
## paste(i, j, sep = "-")
## Plot Price
Price.series <- function(xi, xj){ ## nがエアラインNo, (i, j)が地域, 0で指定なし
  x <- Price.matrix
  x <-  x[, c(which(datalie2$i == xi & datalie2$j == xj))] # 直す
  rownames(x) <- 1:nrow(x)
  colnames(x) <- 1:ncol(x)
  y <- reshape2::melt(x)
  colnames(y) <- c("count", "route", "price")
  
  graph <- ggplot2::ggplot(y, ggplot2::aes(x = count, y = price, color = as.factor(route)))
  graph <- graph + ggplot2::geom_line()
  graph <- graph + ggsci::scale_color_nejm()
  output <- ggplot2::ggsave(file = paste("運賃 ", xi, "-", xj, ".png", sep = ""), plot(graph))
  return(output)
}
## Profit
Profit.series <- function(n){ ## nに何入れようが関係ない
  x <- Profit.matrix
  rownames(x) <- 1:nrow(x)
  colnames(x) <- 1:ncol(x)
  y <- reshape2::melt(x)
  colnames(y) <- c("count", "airline", "profit")
  
  graph <- ggplot2::ggplot(y, ggplot2::aes(x = count, y = profit, color = as.factor(airline)))
  graph <- graph + ggplot2::geom_line()
  graph <- graph + ggsci::scale_color_nejm()
  output <- ggplot2::ggsave(file = "利潤.png", plot(graph))
  return(output)
}


## Substitution matrix
Price.Substitution.Matrix <- function(xi, xj){
  library(magrittr)
  x <- datalie2 %>% 
    dplyr::filter(i == xi, j == xj) %>% 
    dplyr::mutate(prob2 = 1 - observedprob)
  xx <- -parameter[2]*t(x$費用*x$observedprob)
  mat <- matrix(0, nrow = ncol(xx), ncol = ncol(xx))
  for(i in 1:nrow(x)){
    mat[i, ] <- xx
  }
  diag(mat) <- parameter[2]*(x$費用*x$prob2)
  output <- mat
  return(output)
}
Price.Substitution.Matrix(18, 37)

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

