## Making Tables

## Library
library(magrittr)
## data
#base <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/20181231/3（ベース）/simulation_results.csv", header = T)
#simulation <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/20181231/4（反実仮想）/simulation_results.csv", header = T)
base <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/20181231/6（ベース）（仮）（途中停止）/simulation_results.csv", header = T)
simulation <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/20181231/7（反実仮想）（仮）/simulation_results.csv", header = T)
## 反実仮想のJR運賃が入っていないので入れる

add.railway.price <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
for(i in 1:nrow(simulation)){
  for(j in 1:nrow(add.railway.price)){
    if(simulation[i, 6] == "JR" && simulation[i, 2] == add.railway.price[j, 1] && simulation[i, 3] == add.railway.price[j, 2]){
      simulation[i, 7] <- add.railway.price[j, 8]
    } else {
      
    }
  }
}

## いらないの落としてから結合してソート
simulation <- simulation[ , c(7, 9, 11, 13)]
colnames(simulation) <- c("運賃（反実仮想）", "航空便数（反実仮想）", "シェア（反実仮想）", "需要（反実仮想）")
colnames(base)[4:14] <- c("出発空港/駅", "到着空港/駅", "航空・鉄道会社", 
                          "運賃（ベース）", "運賃（実績）", "航空便数（ベース）", "航空便数（実績）", 
                          "シェア（ベース）", "シェア（実績）", "需要（ベース）", "需要（実績）")
result <- cbind(base, simulation)
result <- result[ , c(2:6, 8, 7, 15, 10, 9, 16, 12, 11, 17, 14, 13, 18)]

## 数字を綺麗にする
## 運賃→整数値に
result[ , 6:8] <- round(result[ , 6:8])
## 航空便数→小数点以下1桁，1日当たりに
result[ , 9:11] <- round(result[ , 9:11]/365, 1)
## シェア→小数点以下3桁
result[ , 12:14] <- round(result[ , 12:14], 3)
## 需要→整数値，千人単位
result[ , 15:17] <- round(result[ , 15:17]/1000)

## JRの運賃2倍
result[which(result[ , 5] == "JR"), 6] <- result[which(result[ , 5] == "JR"), 6]*2
result[which(result[ , 5] == "JR"), 7] <- result[which(result[ , 5] == "JR"), 6]
## JR部分の航空便数に"－"を挿入
for(i in 9:11){
  result[which(result[ , 5] == "JR"), i] <- "－"
}

## 需要0となった部分の運賃に"－"を挿入
for(i in 1:2)
result[which(result[ , i + 15] == 0), i + 6] <- "－"

##
result$`運賃（実績）` <- as.integer(result$`運賃（実績）`)
result$`運賃（ベース）` <- as.integer(result$`運賃（ベース）`)
result$`運賃（反実仮想）` <- as.integer(result$`運賃（反実仮想）`)
result$`シェア（実績）` <- as.character(result$`シェア（実績）`)
result$`シェア（ベース）` <- as.character(result$`シェア（ベース）`)
result$`シェア（反実仮想）` <- as.character(result$`シェア（反実仮想）`)
result$`需要（実績）` <- as.character(result$`需要（実績）`)
result$`需要（ベース）` <- as.character(result$`需要（ベース）`)
result$`需要（反実仮想）` <- as.character(result$`需要（反実仮想）`)


index.for.table <- subset(result, , c(i, j)) %>% unique() %>% as.matrix() 
## 地域と運賃・航空便数・シェア・需要を引数とするようなテーブル生成関数を書く
## indexでは行列形式で(i, j)のindexを指定
## var1は4次元ベクトルで1なら含める，0なら含めない
## var2は3次元ベクトルで実績，ベース，シミュレーションを含める・含めないを指定
making.table <- function(index, var1, var2){
  col.index <- c(rep(0, 2), rep(1, 3), var1 %x% var2, rep(0, 3))
  result <- result %>% 
    dplyr::mutate(indexi = 0, indexj = 0)
  for(i in 1:nrow(result)){
    for(j in 1:length(index)){
      if(result[i, 1] == index[j]){
        result$indexi[i] <- 1
      } else {
        
      }
        
    }
  }
  for(i in 1:nrow(result)){
    for(j in 1:length(index)){
      if(result[i, 2] == index[j]){
        result$indexj[i] <- 1
      } else {
        
      }
        
    }
  }
  result <- result %>% 
    dplyr::mutate(index = indexi*indexj)
  result <- result[which(result$index == 1), ]
  result <- result[ , which(col.index == 1)]
  rownames(result) <- NULL
  output <- xtable::xtable(result, file = "Estimation_results3.tex", 
                         caption = "推定結果", 
                         where = "h", 
                         col.just = rep("r", dim(result)[2]))
  return(output)
}

aa <- matrix(c(18, 25, 18, 37, 25, 37, 25, 18, 37, 18, 37, 25), nrow = 6, ncol = 2)
ab <- c(3, 8, 12, 18, 25, 37, 46)
ac <- c(3, 8)
v1 <- c(1, 1, 0, 1)
v2 <- c(1, 1, 1)
kakuninn <- making.table(ab, v1, v2)
kakuninn

