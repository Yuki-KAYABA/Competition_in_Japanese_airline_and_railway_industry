## ���i�ƑI���m���̊֌W��T��
library(ggplot2)
library(magrittr)
XX1 <- demand

g <- ggplot2::ggplot(XX1, ggplot2::aes(x = �G�A���C���Q����, y = share1))
g <- g + ggplot2::geom_point(aes(colour = factor(�S���_�~�[))) + ylab("�I���m��")
plot(g)

XX2 <- demand
XX2 <- XX2[ , c(11, 5)]

g2 <- ggplot2::ggplot(XX2, ggplot2::aes(x = ��p, y = y))
g2 <- g2 + ggplot2::geom_point()
plot(g2)
## �S���̂�
XX3 <- demand[ , c(3, 5, 9)] %>% 
  dplyr::filter(�S���_�~�[ == 1)
g3 <- ggplot2::ggplot(XX3, ggplot2::aes(x = ��p, y = share))
g3 <- g3 + ggplot2::geom_point()
plot(g3)


XX4 <- demand[ , c(3, 5, 9)] %>% 
  dplyr::filter(�S���_�~�[ == 0)
g4 <- ggplot2::ggplot(XX4, ggplot2::aes(x = ��p, y = share))
g4 <- g4 + ggplot2::geom_point()
plot(g4)