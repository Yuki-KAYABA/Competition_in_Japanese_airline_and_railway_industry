## Calculate consumer surplus

## Making Tables

## Library
library(magrittr)
## data
base <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/20181231/3�i�x�[�X�j/simulation_results.csv", header = T)
simulation <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/20181231/4�i�������z�j/simulation_results.csv", header = T)

## ����Ȃ��̗��Ƃ��Ă��猋�����ă\�[�g
simulation <- simulation[ , c(7, 9, 11, 13)]
colnames(simulation) <- c("�^���i�������z�j", "�q��֐��i�������z�j", "�V�F�A�i�������z�j", "���v�i�������z�j")
colnames(base)[4:14] <- c("�o����`/�w", "������`/�w", "�q��E�S�����", 
                          "�^���i�x�[�X�j", "�^���i���сj", "�q��֐��i�x�[�X�j", "�q��֐��i���сj", 
                          "�V�F�A�i�x�[�X�j", "�V�F�A�i���сj", "���v�i�x�[�X�j", "���v�i���сj")
result <- cbind(base, simulation)
result <- result[ , c(2:6, 8, 7, 15, 10, 9, 16, 12, 11, 17, 14, 13, 18)]
result[which(result[ , 5] == "JR"), 6] <- result[which(result[ , 5] == "JR"), 6]*2
result[which(result[ , 5] == "JR"), 7] <- result[which(result[ , 5] == "JR"), 6]
result[which(result[ , 5] == "JR"), 8] <- result[which(result[ , 5] == "JR"), 6]

## �������z��JR�^���������Ă��Ȃ��̂œ����
add.railway.price <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
for(i in 1:nrow(result)){
  for(j in 1:nrow(add.railway.price)){
    if(result[i, 5] == "JR" && result[i, 1] == add.railway.price[j, 1] && result[i, 2] == add.railway.price[j, 2]){
      result[i, 8] <- add.railway.price[j, 8]
    } else {
      
    }
  }
}


base_data <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data_base.csv", header = T)
counterfactual_data <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data_counterfactual.csv", header = T)
write.csv(result, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/result�m�F.csv")

base_data <- cbind(base_data, result)

base_data <- base_data[ , -c(1, 2, 3)]
counterfactual_data <- counterfactual_data[ , -c(1, 2, 3)]
write.csv(base_data, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/base_data�m�F2.csv")

counterfactual_data <- cbind(counterfactual_data, result)

## pamameter
a <- -IVreg7$coefficients[2] 
## Start from base
base_data <- base_data[-c(which(base_data$`���v�i�x�[�X�j`<1e-4)), ] ## ���v0�z��r��
base_data <- base_data %>% 
  dplyr::mutate(exputility.base = exp(parameter[1] + parameter[2]*`�^���i�x�[�X�j`
                + parameter[3]*log(`�q��֐��i�x�[�X�j`) + parameter[4]*�A�N�Z�V�r���e�B + parameter[5]*�S���_�~�[ 
                + parameter[6]*��ւ�������Ȑ� + parameter[7]*��ւ�����ݕ� + parameter[8]*���v����)) %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(sumexputility.base = sum(exputility.base)) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        var = c('sumexputility.base')) %>% 
  dplyr::mutate(CS.base = log(1 + var)/a)

base_data

counterfactual_data <- counterfactual_data[-c(which(counterfactual_data$`���v�i�������z�j`<1e-4)), ] ## ���v0�z��r��
counterfactual_data <- counterfactual_data %>% 
  dplyr::mutate(exputility.counterfactual = exp(parameter[1] + parameter[2]*`�^���i�������z�j`
                                                + parameter[3]*log(`�q��֐��i�������z�j`) + parameter[4]*�A�N�Z�V�r���e�B + parameter[5]*�S���_�~�[ 
                                                + parameter[6]*��ւ�������Ȑ� + parameter[7]*��ւ�����ݕ� + parameter[8]*���v����)) %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(sumexputility.counterfactual = sum(exputility.counterfactual)) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        var = c('sumexputility.counterfactual')) %>% 
  dplyr::mutate(CS.counterfactual = log(1 + var)/a)
  
CS <- cbind(base_data, counterfactual_data)
CS <- CS[ , c(1, 2, 4, 8)]
CS <- CS %>% 
  dplyr::mutate(CS.difference = CS.counterfactual - CS.base)

## Table
index4 <- c(3, 8, 12, 18, 25, 37, 46)
index4.names <- c("����", "�D�y", "���", "����", "���", "����", "�ߔe")
correspondence <- data.frame(i = index4, ���O = index4.names)

for (k in 1:2){
  for(i in 1:nrow(CS)){
    for(j in 1:nrow(correspondence)){
      if(CS[i, k] == correspondence[j, 1]){
        CS[i, k + 5] <- correspondence[j, 2]
      } else {
        
      }
    }
  }
}
CS <- CS[ , c(1:2, 6:7, 3:5)]
CS <- CS %>% 
  dplyr::mutate(�ω��� = (CS.counterfactual/CS.base - 1)*100)
CS[ , 5:7] <- round(CS[ , 5:7])
CS[ , 8] <- round(CS[ , 8], 2)
CS$CS.base <- as.integer(CS$CS.base)
CS$CS.counterfactual <- as.integer(CS$CS.counterfactual)
CS$CS.difference <- as.integer(CS$CS.difference)
colnames(CS) <- c("i", "j", "�o���n", "�����n", "����җ]��i�x�[�X�j", "����җ]��i�������z�j", "����җ]��i�����j", "����җ]��i�ω����j")
xtable::xtable(CS[ , 3:8], caption = "����җ]��̕ω�", where = "h", col.just = rep("r", dim(CS)[2]))
write.csv(CS, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/comsumer surplus.csv")