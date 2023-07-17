## Library
library(magrittr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(scales)
library(gridExtra)
library(RColorBrewer)
## Read Results
Results <- read.csv("C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/simulation_results.csv", header = T)
Results[which(Results[ , 6] == "JR"), 7] <- Results[which(Results[ , 6] == "JR"), 7]*2
Results[which(Results[ , 6] == "JR"), 7] <- Results[which(Results[ , 6] == "JR"), 8]
## Indexing
index <- aggregate(x = Results["i"], by = list(Results$i), mean)
index <- index[ , 1]

names <- c("”ŸŠÙ", "D–y", "å‘ä","“Œ‹", "‘åã", "•Ÿ‰ª", "“ß”e")
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
  Results <- Results %>% 
    dplyr::filter(i == ODi, j == ODj) %>% 
    dplyr::group_by(q‹ó‰ïĞ) %>% 
    dplyr::mutate(simulated.share = sum(Probability_simulation), 
                  observed.share = sum(Probability_observed))
  df <- subset(Results, , c(q‹ó‰ïĞ, observed.share, simulated.share))
  xx <- unique(df)
  xx$q‹ó‰ïĞ <- as.character(xx$q‹ó‰ïĞ) 
  title <- paste(index[c(which(index == ODi)), 2], "|", index[c(which(index == ODj)), 2], "ƒVƒFƒA", sep = "")
  ## Draw a graph
  x <- data.frame(ƒVƒ~ƒ…ƒŒ[ƒVƒ‡ƒ“ = c(rep("ÀÑ", nrow(xx)), rep("ƒx[ƒX", nrow(xx))), 
                  q‹ó“S“¹‰ïĞ = c(xx$q‹ó‰ïĞ, xx$q‹ó‰ïĞ), 
                  —·‹qƒVƒFƒA = c(xx$observed.share, xx$simulated.share))
  x <- transform(x, ƒVƒ~ƒ…ƒŒ[ƒVƒ‡ƒ“ = factor(ƒVƒ~ƒ…ƒŒ[ƒVƒ‡ƒ“, levels = c("ÀÑ", "ƒx[ƒX")))
  g <- ggplot(x, aes(x = ƒVƒ~ƒ…ƒŒ[ƒVƒ‡ƒ“, y = —·‹qƒVƒFƒA, fill = q‹ó“S“¹‰ïĞ)) #+ labs(title = title)
  g <- g + geom_bar(stat = "identity", position = "fill", width = 0.4, aes(fill = q‹ó“S“¹‰ïĞ))
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
    dplyr::mutate(label = paste(‹ó`i, "-", ‹ó`j, ":", q‹ó‰ïĞ))
  yy <- t(cbind(Results$Price_observed, Results$Price_simulation))
  dim(yy) <- c(nrow(Results)*2, 1)
  yy <- yy[ , 1]
  title <- paste(index[c(which(index == ODi)), 2], "|", index[c(which(index == ODj)), 2], "‰^’À", sep = "")
  x <- data.frame(Œo˜H = c(rep(Results$label, each = 2)), 
                  sample = c(rep(c("ÀÑ", "ƒx[ƒX"), nrow(Results))), 
                  ‰^’À = yy)
  g <- ggplot(x, aes(x = Œo˜H, y = ‰^’À, fill = sample))
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
ggsave(file = "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/Share1.pdf", plot = Share.1, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/Share2.pdf", plot = Share.2, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/Share3.pdf", plot = Share.3, width=10,height=14.14286)


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
ggsave(file = "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/Fare1.pdf", plot = Fare.1, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/Fare2.pdf", plot = Fare.2, width=10,height=14.14286)
ggsave(file = "C:/Users/kayab/yuki/‘åŠwu‹`/‘åŠw‰@ƒ[ƒ~/Data/Results/Fare3.pdf", plot = Fare.3, width=10,height=14.14286)


