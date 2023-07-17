## IV estimation
## Distance can be an IV since it is correlated with price
## We assume that distance is uncorrelated with unobserved qualities of products

## Library
library(dplyr)
library(tidyr)
library(AER)
## Data
data.for.IV <- demand
railway.distance <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/railwaycost.csv", header = T)
airline.distance <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
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
airline.distance$�^�q�� <- as.numeric(gsub(",", "", airline2$�^�q��)) # gsub is used to remove commas in each figure
airline.distance$�^�q�L�����[�g�� <- as.numeric(gsub(",", "", airline2$�^�q�L�����[�g��))
airline.distance <- airline.distance %>% 
  dplyr::mutate(distance.per = �^�q�L�����[�g��/�^�q��) %>% 
  dplyr::group_by(��`i, ��`j) %>% 
  dplyr::mutate(distance = mean(distance.per)) %>% 
  dplyr::ungroup()
## Raplace names
airline.distance$��`i <- gsub("����", "�H�c", airline.distance$��`i)
airline.distance$��`j <- gsub("����", "�H�c", airline.distance$��`j)
airline.distance$��`i <- gsub("�֐�", "�֐�����", airline.distance$��`i)
airline.distance$��`j <- gsub("�֐�", "�֐�����", airline.distance$��`j)

## Attach airline distance
data.for.IV$�Ŋ��i <- as.character(data.for.IV$�Ŋ��i)
data.for.IV$�Ŋ��j <- as.character(data.for.IV$�Ŋ��j)
for(i in 1:nrow(data.for.IV)){
  for(j in 1:nrow(airline.distance)){
    if(data.for.IV[i, 11] == airline.distance[j, 1] && data.for.IV[i, 12] == airline.distance[j, 2] && data.for.IV[i, 21] == 0){
      data.for.IV[i, 26] <- airline.distance[j, 24]
    } else {
      
    }
  }
}
colnames(data.for.IV)[26] <- "distance"
write.csv(data.for.IV, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/dataforIV.csv")
## 
IV <- data.for.IV[ , c(24, 25, 16, 17, 18 ,19, 20, 21, 22, 26)] ## Probably have to revise it! We have to leave share and out.share
IV <- IV %>% 
  dplyr::mutate(y = log(share) -log(out.share))
## Write csv file
write.csv(IV, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/IV.csv")
## Estimate parameters of utility function
## Delete rows including NAs
for(i in nrow(IV):1){
  if(is.na(IV[i, 10])){
    IV <- IV[-i, ]
  } else {
    
  }
}
IV <- IV[-which(IV$y == Inf), ]
IV <- IV[-which(is.na(IV$y) | is.na(IV$���v����) | is.na(IV$��p) | is.na(IV$�q��֐�) | is.na(IV$�؍݉\����) 
                        | is.na(IV$�A�N�Z�V�r���e�B) | is.na(IV$�S���_�~�[) | is.na(IV$�G�A���C���Q����)), ]

IVreg <- ivreg(data = IV, y ~ ���v���� + ��p + log(�q��֐�*365) + �؍݉\���� + �A�N�Z�V�r���e�B + �S���_�~�[ + �G�A���C���Q���� | 
                 ���v���� + log(�q��֐�*365) + �؍݉\���� + �A�N�Z�V�r���e�B + �S���_�~�[ + �G�A���C���Q���� + distance) ## ��������q��֐���365�{�ŏ���
IV_summary <- stargazer::stargazer(IVreg, logit, type = "html")

IVpar <- IVreg$coefficients