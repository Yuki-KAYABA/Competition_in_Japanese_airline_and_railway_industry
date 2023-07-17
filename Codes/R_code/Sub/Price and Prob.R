## 価格と選択確率の関係を探る
library(ggplot2)
library(magrittr)
XX1 <- demand

g <- ggplot2::ggplot(XX1, ggplot2::aes(x = エアライン参入数, y = share1))
g <- g + ggplot2::geom_point(aes(colour = factor(鉄道ダミー))) + ylab("選択確率")
plot(g)

XX2 <- demand
XX2 <- XX2[ , c(11, 5)]

g2 <- ggplot2::ggplot(XX2, ggplot2::aes(x = 費用, y = y))
g2 <- g2 + ggplot2::geom_point()
plot(g2)
## 鉄道のみ
XX3 <- demand[ , c(3, 5, 9)] %>% 
  dplyr::filter(鉄道ダミー == 1)
g3 <- ggplot2::ggplot(XX3, ggplot2::aes(x = 費用, y = share))
g3 <- g3 + ggplot2::geom_point()
plot(g3)


XX4 <- demand[ , c(3, 5, 9)] %>% 
  dplyr::filter(鉄道ダミー == 0)
g4 <- ggplot2::ggplot(XX4, ggplot2::aes(x = 費用, y = share))
g4 <- g4 + ggplot2::geom_point()
plot(g4)