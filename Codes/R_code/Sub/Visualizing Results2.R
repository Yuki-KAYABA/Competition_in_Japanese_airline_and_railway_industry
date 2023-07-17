## Visualizing Results2
## Library
library(magrittr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(scales)
library(gridExtra)
library(RColorBrewer)

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

## Indexing
index <- aggregate(x = Results["i"], by = list(Results$i), mean)
index <- index[ , 1]

names <- c("函館", "札幌", "仙台","東京", "大阪", "福岡", "那覇")
index <- cbind(index, names)
a1 <- c(rep(index[1, 1], (nrow(index) - 1)))
a2 <- index[-1, 1]
aa <- cbind(a1, a2)
for(i in 2:(nrow(index) - 1)){
  a <- c(rep(index[i, 1], nrow(index) - i))
  b <- index[-c(seq(1:i)), 1]
  aanew <- cbind(a, b)
  aa <- rbind(aa, aanew)
}
bb <- aa[ , c(2, 1)]
index2 <- matrix(0, nrow(index) * (nrow(index) - 1), 2)
for(i in 1:nrow(aa)){
  index2[2 * i - 1, ] <- aa[i, ]
  index2[2 * i, ] <- bb[i, ]
}

## Share
graph.share <- function(ODi, ODj){
  result <- result %>% 
    dplyr::filter(i == ODi, j == ODj) %>% 
    dplyr::group_by(`航空・鉄道会社`) %>% 
    dplyr::mutate(observed.share = sum(`シェア（実績）`), 
                  simulated.share.base = sum(`シェア（ベース）`), 
                  simulated.share.counterfactual = sum(`シェア（反実仮想）`))
  df <- subset(result, , c(`航空・鉄道会社`, observed.share, simulated.share.base, simulated.share.counterfactual))
  xx <- unique(df)
  xx$`航空・鉄道会社` <- as.character(xx$`航空・鉄道会社`) 
  title <- paste(index[c(which(index == ODi)), 2], "－", index[c(which(index == ODj)), 2], "シェア", sep = "")
  ## Draw a graph
  x <- data.frame(シミュレーション = c(rep("実績", nrow(xx)), rep("ベース", nrow(xx)), rep("反実仮想", nrow(xx))), 
                  航空鉄道会社 = c(xx$`航空・鉄道会社`, xx$`航空・鉄道会社`, xx$`航空・鉄道会社`), 
                  旅客シェア = c(xx$observed.share, xx$simulated.share.base, xx$simulated.share.counterfactual))
  x <- transform(x, シミュレーション = factor(シミュレーション, levels = c("実績", "ベース", "反実仮想")))
  g <- ggplot(x, aes(x = シミュレーション, y = 旅客シェア, fill = 航空鉄道会社)) #+ labs(title = title)
  g <- g + geom_bar(stat = "identity", position = "fill", width = 0.5, aes(fill = 航空鉄道会社))
  g <- g + scale_y_continuous(labels = percent) + theme_bw(base_family = "Japan1HeiMin") + theme_gray()
  g <- g + scale_fill_grey()
  g <- g + xlab(NULL)
  # g <- g + scale_fill_nejm()
  # g <- g + scale_color_brewer(palette = "Greys")
  # g <- g + scale_fill_brewer(palette = "Greys")
  # output <- plot(g)
  output <- ggsave(paste(title, ".pdf", sep = ""), width = 4.47, height = 2.89, g, family = "Japan1HeiMin")
  return(output)
}
graph.share(18, 25)

## Fare
graph.fare <- function(ODi, ODj){
  Results <- Results %>% 
    dplyr::filter(i == ODi, j == ODj) %>% 
    dplyr::mutate(label = paste(空港i, "-", 空港j, ":", 航空会社))
  yy <- t(cbind(Results$Price_observed, Results$Price_simulation))
  dim(yy) <- c(nrow(Results)*2, 1)
  yy <- yy[ , 1]
  title <- paste(index[c(which(index == ODi)), 2], "－", index[c(which(index == ODj)), 2], "運賃", sep = "")
  x <- data.frame(経路 = c(rep(Results$label, each = 2)), 
                    sample = c(rep(c("実績", "ベース"), nrow(Results))), 
                    運賃 = yy)
  g <- ggplot(x, aes(x = 経路, y = 運賃, fill = sample))
  g <- g + geom_bar(stat = "identity", position = "dodge") #+ labs(title = title)
  g <- g + scale_fill_grey() + theme_bw(base_family = "Japan1HeiMin")
  g <- g + scale_x_discrete(labels = NULL)
  g <- g + xlab(NULL)
  output <- ggsave(paste(title, ".pdf", sep = ""), g, width = 10, height = 5)
  return(output)
}
graph.fare(18, 25)
graph.share(18, 25)

##Demand

## Output
index3 <- subset(Results, , c(i, j)) %>% unique() %>% as.matrix() 

## Share
for(i in 1:nrow(index3)){
  graph.share(as.numeric(index3[i, 1]), as.numeric(index3[i, 2]))
}

graph.share(8, 18)
graph.share(18, 8)
graph.share(8, 25)
graph.share(25, 8)
graph.share(8, 37)
graph.share(37, 8)
graph.share(18, 25)
graph.share(25, 18)
graph.share(18, 37)
graph.share(37, 18)
graph.share(18, 46)
graph.share(46, 18)
graph.share(25, 37)
graph.share(37, 25)
graph.share(25, 46)
graph.share(46, 25)
graph.share(37, 46)
graph.share(46, 37)

Share.1 <- grid.arrange(Share1, Share2, Share3, Share4, Share5, Share6, ncol = 2)
Share.2 <- grid.arrange(Share7, Share8, Share9, Share10, Share11, Share12, ncol = 2)
Share.3 <- grid.arrange(Share13, Share14, Share15, Share16, Share17, Share18, ncol = 2)
ggsave(file = "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/Share1.pdf", plot = Share.1, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/Share2.pdf", plot = Share.2, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/Share3.pdf", plot = Share.3, width=10,height=14.14286)


## Fare
Fare1 <- graph.fare(8, 18)
Fare2 <- graph.fare(18, 8)
Fare3 <- graph.fare(8, 25)
Fare4 <- graph.fare(25, 8)
Fare5 <- graph.fare(8, 37)
Fare6 <- graph.fare(37, 8)
Fare7 <- graph.fare(18, 25)
Fare8 <- graph.fare(25, 18)
Fare9 <- graph.fare(18, 37)
Fare10 <- graph.fare(37, 18)
Fare11 <- graph.fare(18, 46)
Fare12 <- graph.fare(46, 18)
Fare13 <- graph.fare(25, 37)
Fare14 <- graph.fare(37, 25)
Fare15 <- graph.fare(25, 46)
Fare16 <- graph.fare(46, 25)
Fare17 <- graph.fare(37, 46)
Fare18 <- graph.fare(46, 37)


Fare.1 <- grid.arrange(Fare1, Fare2, Fare3, Fare4, Fare5, Fare6, ncol = 2)
Fare.2 <- grid.arrange(Fare7, Fare8, Fare9, Fare10, Fare11, Fare12, ncol = 2)
Fare.3 <- grid.arrange(Fare13, Fare14, Fare15, Fare16, Fare17, Fare18, ncol = 2)
ggsave(file = "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/Fare1.pdf", plot = Fare.1, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/Fare2.pdf", plot = Fare.2, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/Results/Fare3.pdf", plot = Fare.3, width=10,height=14.14286)


