## Library
library(magrittr)
library(tidyr)

## Read data
airline <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline.csv", header = T)
ODforairline <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/ODforairline.csv", header = T)
data <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data.csv", header = T)
station <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/station.csv", header = T)
airport <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airport.csv", header = T)
combine <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/combine.csv", header = T)
railway <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/railway.csv", header = T)
railwaycost <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/railwaycost.csv", header = T)
OD <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/OD.csv", header = T, stringsAsFactors = FALSE)
accessibility.station <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/accessibility_station.csv", header = T)
accessibility.airport <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/accessibility_airport.csv", header = T)
OD.207 <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/207OD.csv", header = T)
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
## �q��^���������œ����Ă���̂ŕГ��^���ɂ���
airline$��p <- airline$��p/2

## Combine ODs
airline <- airline %>% 
  dplyr::group_by(��`i, ��`j) %>% 
  dplyr::mutate(i = min(i),
                j = min(j),
                freq = 365*sum(�q��֐�), ## �q��֐��͊ۂ߂镔���Ɋւ��Ă͑��a�ŏ�������A�ł�����365�{����
                cost = sum(�q��֐� * ��p)/sum(�q��֐�), ## �P�����ς�����d���ςɍ����ւ���
                duration = sum(�q��֐� * ���v����)/sum(�q��֐�), ## ��������d����
                stay = max(�؍݉\����), ## ����͕��ς���ő�l�ɕς���
                entry = mean(�G�A���C���Q���А�)) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        vars = c('i', 'j', 'duration', 'cost', 'freq', 'stay', 'entry'))
colnames(airline) <- c("��`i", "��`j", "i", "j", "duration", "cost", "freq", "stay", "entry")

# Insert airline data into the whole one
for(i in 1:nrow(data)){
  for(j in 1:nrow(airline)){
    if(data[i, 20]==0 && data[i, 6]==airline[j, 3] && data[i, 7]==airline[j, 4]){ ## 20:�S���_�~�[, 6:47�]�[��i, 7:47�]�[��j
      data[i , c(15, 16, 17, 18, 21)] <- airline[j, c(5, 6, 7, 8, 9)]
    } else {
      
    }
  }
}

# Transform ���v���� and �؍݉\���� and cost into numeric
railway$���v����.1 <-as.numeric(as.character(railway$���v����.1))
railway$�؍݉\����.1 <- as.numeric(as.character(railway$�؍݉\����.1))
railwaycost$cost <- as.numeric(as.character(railwaycost$cost))

# Attach appropriate figures to each station 
##  for(k in 1:2){
##    for(i in 1:nrow(railway)){
##      for(j in 1:nrow(station)){
##        if(railway[i, k + 2]==station[j, 2]){ ## railway��3�C4��ڂ͉w��
##          railway[i, k + 23] <- station[j, 1] ## railway��24�C25��ڂɔԍ���}�����Ă���A����Ȃ��Ȃ��H
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
      data[i , 16] <- railwaycost[j, 6]*2 # I have to revise this! I just get data only on ��Ԍ��^��
    } else {
      
    }
  }
}

## Insert 1s into �q��֐� if railway dummy is equal to 1, so that ln(�q��֐�) will be zero when the dummy is 1
for(i in 1:nrow(data)){
  if(data[i, 20]==1){
    data[i, c(17, 21)] <- c(1, 0)
  } else {
    
  }
}

## �C�O���X�𔽉f
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
data$�A�N�Z�V�r���e�B <- data$�A�N�Z�V�r���e�B + data$V22

## �����܂�==========================================
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
write.csv(data, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data1.csv")

## If succesful, data for utility analysis is named as demand
demand <- data[ , c(24, 25, 15, 16, 17 ,18, 19, 20, 21)] ## Probably have to revise it! We have to leave share and out.share
demand <- demand %>% 
  dplyr::mutate(y = log(share) -log(out.share))
## Write csv file
write.csv(demand, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/demand.csv")
## Remove some variables
rm(airline, ODforairline, data, station, airport, combine, railway, railwaycost, OD, demand, accessibility.airport, accessibility.station)
demand <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/demand.csv", header = T)
## Estimate parameters of utility function
## Delete rows including NAs
for(i in nrow(demand):1){
  if(is.na(demand[i, 10])){
    demand <- demand[-i, ]
  } else {
    
  }
}
demand <- demand[-which(demand$y == Inf), ]
demand <- demand[-which(is.na(demand$y) | is.na(demand$���v����) | is.na(demand$��p) | is.na(demand$�q��֐�) | is.na(demand$�؍݉\����) 
                        | is.na(demand$�A�N�Z�V�r���e�B) | is.na(demand$�S���_�~�[) | is.na(demand$�G�A���C���Q����)), ]
logit <- lm(data = demand, y ~ ���v���� + ��p + log(�q��֐�) + �؍݉\���� + �A�N�Z�V�r���e�B + �S���_�~�[ + �G�A���C���Q����, 
            na.action = na.omit) ## ��������q��֐���365�{�ŏ���
logit_summary <- stargazer::stargazer(logit, type = "html")

## Maximum likelihood estimation with sign restriction
## Write a function
fr <- function(x){
  LL <- 0
  for(i in 1:nrow(demand)){
    X <- x[1] + x[2]*demand$���v����[i] - exp(x[3])*demand$��p[i] + exp(x[4])*log(demand$�q��֐�[i]) + 
      x[5]*demand$�؍݉\����[i] + x[6]*demand$�A�N�Z�V�r���e�B[i] + x[7]*demand$�S���_�~�[[i] + x[8]*demand$�G�A���C���Q����[i]
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
## Revise demand$�q��֐�*365 since it attaches 365 in the rows of railways

## IV estimation

## Consider quadratic form
logit3 <- lm(data = demand, y ~ ���v���� + ��p + I(��p^2) + log(�q��֐�) + �؍݉\���� + �A�N�Z�V�r���e�B + �S���_�~�[ + �G�A���C���Q����, 
             na.action = na.omit) ## ��������q��֐���365�{�ŏ���
logit3_summary <- stargazer::stargazer(logit3, type = "html")

utilitypar.quad <- logit3$coefficients

peak <- -utilitypar.quad[3]/(utilitypar.quad[4]*2)

## Drop Variables
## Drop �G�A���C���Q����
logit4 <- lm(data = demand, y ~ ���v���� + ��p + log(�q��֐�) + �؍݉\���� + �A�N�Z�V�r���e�B + �S���_�~�[, 
             na.action = na.omit) ## ��������q��֐���365�{�ŏ���
logit5 <- lm(data = demand, y ~ ��p + log(�q��֐�))

logit_summary <- stargazer::stargazer(logit, type = "html")

