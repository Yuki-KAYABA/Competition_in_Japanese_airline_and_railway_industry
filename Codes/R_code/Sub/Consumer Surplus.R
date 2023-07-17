## Calculate consumer surplus

## Making Tables

## Library
library(magrittr)
## data
base <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/20181231/3（ベース）/simulation_results.csv", header = T)
simulation <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/20181231/4（反実仮想）/simulation_results.csv", header = T)

## いらないの落としてから結合してソート
simulation <- simulation[ , c(7, 9, 11, 13)]
colnames(simulation) <- c("運賃（反実仮想）", "航空便数（反実仮想）", "シェア（反実仮想）", "需要（反実仮想）")
colnames(base)[4:14] <- c("出発空港/駅", "到着空港/駅", "航空・鉄道会社", 
                          "運賃（ベース）", "運賃（実績）", "航空便数（ベース）", "航空便数（実績）", 
                          "シェア（ベース）", "シェア（実績）", "需要（ベース）", "需要（実績）")
result <- cbind(base, simulation)
result <- result[ , c(2:6, 8, 7, 15, 10, 9, 16, 12, 11, 17, 14, 13, 18)]
result[which(result[ , 5] == "JR"), 6] <- result[which(result[ , 5] == "JR"), 6]*2
result[which(result[ , 5] == "JR"), 7] <- result[which(result[ , 5] == "JR"), 6]
result[which(result[ , 5] == "JR"), 8] <- result[which(result[ , 5] == "JR"), 6]

## 反実仮想のJR運賃が入っていないので入れる
add.railway.price <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
for(i in 1:nrow(result)){
  for(j in 1:nrow(add.railway.price)){
    if(result[i, 5] == "JR" && result[i, 1] == add.railway.price[j, 1] && result[i, 2] == add.railway.price[j, 2]){
      result[i, 8] <- add.railway.price[j, 8]
    } else {
      
    }
  }
}


base_data <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data_base.csv", header = T)
counterfactual_data <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/data_counterfactual.csv", header = T)
write.csv(result, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/result確認.csv")

base_data <- cbind(base_data, result)

base_data <- base_data[ , -c(1, 2, 3)]
counterfactual_data <- counterfactual_data[ , -c(1, 2, 3)]
write.csv(base_data, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/base_data確認2.csv")

counterfactual_data <- cbind(counterfactual_data, result)

## pamameter
a <- -IVreg7$coefficients[2] 
## Start from base
base_data <- base_data[-c(which(base_data$`需要（ベース）`<1e-4)), ] ## 需要0奴を排除
base_data <- base_data %>% 
  dplyr::mutate(exputility.base = exp(parameter[1] + parameter[2]*`運賃（ベース）`
                + parameter[3]*log(`航空便数（ベース）`) + parameter[4]*アクセシビリティ + parameter[5]*鉄道ダミー 
                + parameter[6]*一便あたり座席数 + parameter[7]*一便あたり貨物 + parameter[8]*所要時間)) %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(sumexputility.base = sum(exputility.base)) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        var = c('sumexputility.base')) %>% 
  dplyr::mutate(CS.base = log(1 + var)/a)

base_data

counterfactual_data <- counterfactual_data[-c(which(counterfactual_data$`需要（反実仮想）`<1e-4)), ] ## 需要0奴を排除
counterfactual_data <- counterfactual_data %>% 
  dplyr::mutate(exputility.counterfactual = exp(parameter[1] + parameter[2]*`運賃（反実仮想）`
                                                + parameter[3]*log(`航空便数（反実仮想）`) + parameter[4]*アクセシビリティ + parameter[5]*鉄道ダミー 
                                                + parameter[6]*一便あたり座席数 + parameter[7]*一便あたり貨物 + parameter[8]*所要時間)) %>% 
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
index4.names <- c("函館", "札幌", "仙台", "東京", "大阪", "福岡", "那覇")
correspondence <- data.frame(i = index4, 名前 = index4.names)

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
  dplyr::mutate(変化率 = (CS.counterfactual/CS.base - 1)*100)
CS[ , 5:7] <- round(CS[ , 5:7])
CS[ , 8] <- round(CS[ , 8], 2)
CS$CS.base <- as.integer(CS$CS.base)
CS$CS.counterfactual <- as.integer(CS$CS.counterfactual)
CS$CS.difference <- as.integer(CS$CS.difference)
colnames(CS) <- c("i", "j", "出発地", "到着地", "消費者余剰（ベース）", "消費者余剰（反実仮想）", "消費者余剰（増減）", "消費者余剰（変化率）")
xtable::xtable(CS[ , 3:8], caption = "消費者余剰の変化", where = "h", col.just = rep("r", dim(CS)[2]))
write.csv(CS, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/comsumer surplus.csv")
