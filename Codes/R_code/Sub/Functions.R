## Functions

## Revision of funcion "Objective"
library(magrittr)
Objective2 <- function(p){
  ## Library
  library(magrittr)
  
  distance <<- 1000
  iter <<- 1
  while(((distance > 1e-4) & (iter < it)) | (iter < 3)){
    Pr <<- prob(datalie2$alpha) 
    for(i in 1:nrow(datalie2)){
      if(datalie2[i, 22] == n){ # AirlineNo
        Demand[i] <<- datalie2[i, 15]*Pr[i]                                            ### ここ書き換えた（列）
        Frequencyrenewal[i] <<- (Demand[i]/(datalie2[i, 17]*datalie2[i, 20]/100))          ### ここ書き換えた（列）
      } else {
        Frequencyrenewal[i] <<- Frequency[i]
      }
    }
    Frequency <<- as.numeric(Frequency)
    Frequencyrenewal <<- as.numeric(Frequencyrenewal)
    distance <<- sqrt(sum(Frequency - Frequencyrenewal)^2)
    ## Frequency.matrix[count + 1, ] <- Frequencyrenewal # いらなくなる
    Frequency <<- Frequencyrenewal
    iter <<- iter + 1
    ## count <<- count + 1 ## いらなくなるかも
  }
  exputility <- rep(0, nrow(datalie2))
  sumexputility <- rep(0, nrow(datalie2))
  Frequency <- ifelse(Frequency == 0, 1e-4, Frequency)
  for(k in 1:nrow(datalie2)){
    exputility[k] <- (datalie2$alpha[k]*exp(parameter[1] + parameter[2]*Price[k] + parameter[3]*log(Frequency[k]) 
                                            + parameter[4]*datalie2$アクセシビリティ[k] + parameter[5]*datalie2$鉄道ダミー[k]))
  }
  exputility[c(which(datalie2$airlineNo == n))] <- exputility[c(which(datalie2$airlineNo == n))]*exp(-parameter[2]*Price[c(which(datalie2$airlineNo == n))])
  exputility[c(which(datalie2$airlineNo == n & Frequency > 1e-4))] <- exputility[c(which(datalie2$airlineNo == n & Frequency > 1e-4))]*exp(parameter[2]*p) ## Freq条件書き足し
  ## exputility[c(which(datalie2$airlineNo == n))] <- exputility[c(which(datalie2$airlineNo == n))]*exp(parameter[2]*p)
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
  demand2 <- demand2[c(which(datalie2$airlineNo == n & Frequency > 1e-4))] ## Freq条件書き足し
  demand2 <- as.numeric(demand2)
  MC2 <- datalie2$MC
  MC2 <- MC2[c(which(datalie2$airlineNo == n & Frequency > 1e-4))] ## Freq条件書き足し
  output <- sum((p - MC2)*demand2)
  ## Price[c(which(datalie2$airlineNo == n & Frequency <= 1e-4))] <- 0
  return(output)
}

## ここあとで消す==========================================================================
## profit.maximization2 <- optim(par = Price[c(which(datalie2$airlineNo == n & Frequency > 1e-4))], fn = Objective, control = list(fnscale = -1))
## profit.maximization2
## ========================================================================================

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


## Price substitution matrix
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

## Frequency Substitution matrix
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

## Objective3
## 航空便数0のときPrice取り除かない
Objective3 <- function(p){
  ## Library
  library(magrittr)
  distance <<- 1000
  iter <<- 1
  while(((distance > 1e-4) & (iter < it)) | (iter < 3)){
    Pr <<- prob(datalie2$alpha) 
    for(i in 1:nrow(datalie2)){
      if(datalie2[i, 22] == n){ # AirlineNo
        Demand[i] <<- datalie2[i, 15]*Pr[i]                                            ### ここ書き換えた（列）
        Frequencyrenewal[i] <<- (Demand[i]/(datalie2[i, 17]*datalie2[i, 20]/100))          ### ここ書き換えた（列）
      } else {
        Frequencyrenewal[i] <<- Frequency[i]
      }
    }
    Frequency <<- as.numeric(Frequency)
    Frequencyrenewal <<- as.numeric(Frequencyrenewal)
    distance <<- sqrt(sum(Frequency - Frequencyrenewal)^2)
    ## Frequency.matrix[count + 1, ] <- Frequencyrenewal # いらなくなる
    Frequency <<- Frequencyrenewal
    iter <<- iter + 1
    ## count <<- count + 1 ## いらなくなるかも
  }
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
  ## Price[c(which(datalie2$airlineNo == n & Frequency <= 1e-4))] <- 0
  return(output)
}
