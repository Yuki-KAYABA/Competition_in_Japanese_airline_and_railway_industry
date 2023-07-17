## IV estimation
## Distance can be an IV since it is correlated with price
## We assume that distance is uncorrelated with unobserved qualities of products

## Library
library(dplyr)
library(tidyr)
library(AER)
## Data
data.for.IV <- demand
railway.distance <- read.csv("C:/Users/kayab/yuki/ëÂäwçuã`/ëÂäwâ@É[É~/Data/railwaycost.csv", header = T)
airline.distance <- read.csv("C:/Users/kayab/yuki/ëÂäwçuã`/ëÂäwâ@É[É~/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
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
airline.distance$â^çqâÒêî <- as.numeric(gsub(",", "", airline2$â^çqâÒêî)) # gsub is used to remove commas in each figure
airline.distance$â^çqÉLÉçÉÅÅ[ÉgÉã <- as.numeric(gsub(",", "", airline2$â^çqÉLÉçÉÅÅ[ÉgÉã))
airline.distance <- airline.distance %>% 
  dplyr::mutate(distance.per = â^çqÉLÉçÉÅÅ[ÉgÉã/â^çqâÒêî) %>% 
  dplyr::group_by(ãÛç`i, ãÛç`j) %>% 
  dplyr::mutate(distance = mean(distance.per)) %>% 
  dplyr::ungroup()
## Raplace names
airline.distance$ãÛç`i <- gsub("ìåãû", "âHìc", airline.distance$ãÛç`i)
airline.distance$ãÛç`j <- gsub("ìåãû", "âHìc", airline.distance$ãÛç`j)
airline.distance$ãÛç`i <- gsub("ä÷êº", "ä÷êºçëç€", airline.distance$ãÛç`i)
airline.distance$ãÛç`j <- gsub("ä÷êº", "ä÷êºçëç€", airline.distance$ãÛç`j)

## Attach airline distance
data.for.IV$ç≈äÒÇËi <- as.character(data.for.IV$ç≈äÒÇËi)
data.for.IV$ç≈äÒÇËj <- as.character(data.for.IV$ç≈äÒÇËj)
for(i in 1:nrow(data.for.IV)){
  for(j in 1:nrow(airline.distance)){
    if(data.for.IV[i, 11] == airline.distance[j, 1] && data.for.IV[i, 12] == airline.distance[j, 2] && data.for.IV[i, 21] == 0){
      data.for.IV[i, 26] <- airline.distance[j, 24]
    } else {
      
    }
  }
}
colnames(data.for.IV)[26] <- "distance"
write.csv(data.for.IV, "C:/Users/kayab/yuki/ëÂäwçuã`/ëÂäwâ@É[É~/Data/dataforIV.csv")
## 
IV <- data.for.IV[ , c(24, 25, 16, 17, 18 ,19, 20, 21, 22, 26)] ## Probably have to revise it! We have to leave share and out.share
IV <- IV %>% 
  dplyr::mutate(y = log(share) -log(out.share))
## Write csv file
write.csv(IV, "C:/Users/kayab/yuki/ëÂäwçuã`/ëÂäwâ@É[É~/Data/IV.csv")
## Estimate parameters of utility function
## Delete rows including NAs
for(i in nrow(IV):1){
  if(is.na(IV[i, 10])){
    IV <- IV[-i, ]
  } else {
    
  }
}
IV <- IV[-which(IV$y == Inf), ]
IV <- IV[-which(is.na(IV$y) | is.na(IV$èäóvéûä‘) | is.na(IV$îÔóp) | is.na(IV$çqãÛï÷êî) | is.na(IV$ëÿç›â¬î\éûä‘) 
                        | is.na(IV$ÉAÉNÉZÉVÉrÉäÉeÉB) | is.na(IV$ìSìπÉ_É~Å[) | is.na(IV$ÉGÉAÉâÉCÉìéQì¸êî)), ]

IVreg <- ivreg(data = IV, y ~ èäóvéûä‘ + îÔóp + log(çqãÛï÷êî*365) + ëÿç›â¬î\éûä‘ + ÉAÉNÉZÉVÉrÉäÉeÉB + ìSìπÉ_É~Å[ + ÉGÉAÉâÉCÉìéQì¸êî | 
                 èäóvéûä‘ + log(çqãÛï÷êî*365) + ëÿç›â¬î\éûä‘ + ÉAÉNÉZÉVÉrÉäÉeÉB + ìSìπÉ_É~Å[ + ÉGÉAÉâÉCÉìéQì¸êî + distance) ## Ç¢Ç¡ÇΩÇÒçqãÛï÷êîÇ365î{Ç≈èàóù
IV_summary <- stargazer::stargazer(IVreg, logit, type = "html")

IVpar <- IVreg$coefficients
