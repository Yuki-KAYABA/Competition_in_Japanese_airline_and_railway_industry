## Making Tables

## Library
library(magrittr)
## data
#base <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/20181231/3�i�x�[�X�j/simulation_results.csv", header = T)
#simulation <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/20181231/4�i�������z�j/simulation_results.csv", header = T)
base <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/20181231/6�i�x�[�X�j�i���j�i�r����~�j/simulation_results.csv", header = T)
simulation <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/Results/20181231/7�i�������z�j�i���j/simulation_results.csv", header = T)
## �������z��JR�^���������Ă��Ȃ��̂œ����

add.railway.price <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
for(i in 1:nrow(simulation)){
  for(j in 1:nrow(add.railway.price)){
    if(simulation[i, 6] == "JR" && simulation[i, 2] == add.railway.price[j, 1] && simulation[i, 3] == add.railway.price[j, 2]){
      simulation[i, 7] <- add.railway.price[j, 8]
    } else {
      
    }
  }
}

## ����Ȃ��̗��Ƃ��Ă��猋�����ă\�[�g
simulation <- simulation[ , c(7, 9, 11, 13)]
colnames(simulation) <- c("�^���i�������z�j", "�q��֐��i�������z�j", "�V�F�A�i�������z�j", "���v�i�������z�j")
colnames(base)[4:14] <- c("�o����`/�w", "������`/�w", "�q��E�S�����", 
                          "�^���i�x�[�X�j", "�^���i���сj", "�q��֐��i�x�[�X�j", "�q��֐��i���сj", 
                          "�V�F�A�i�x�[�X�j", "�V�F�A�i���сj", "���v�i�x�[�X�j", "���v�i���сj")
result <- cbind(base, simulation)
result <- result[ , c(2:6, 8, 7, 15, 10, 9, 16, 12, 11, 17, 14, 13, 18)]

## �������Y��ɂ���
## �^���������l��
result[ , 6:8] <- round(result[ , 6:8])
## �q��֐��������_�ȉ�1���C1���������
result[ , 9:11] <- round(result[ , 9:11]/365, 1)
## �V�F�A�������_�ȉ�3��
result[ , 12:14] <- round(result[ , 12:14], 3)
## ���v�������l�C��l�P��
result[ , 15:17] <- round(result[ , 15:17]/1000)

## JR�̉^��2�{
result[which(result[ , 5] == "JR"), 6] <- result[which(result[ , 5] == "JR"), 6]*2
result[which(result[ , 5] == "JR"), 7] <- result[which(result[ , 5] == "JR"), 6]
## JR�����̍q��֐���"�|"��}��
for(i in 9:11){
  result[which(result[ , 5] == "JR"), i] <- "�|"
}

## ���v0�ƂȂ��������̉^����"�|"��}��
for(i in 1:2)
result[which(result[ , i + 15] == 0), i + 6] <- "�|"

##
result$`�^���i���сj` <- as.integer(result$`�^���i���сj`)
result$`�^���i�x�[�X�j` <- as.integer(result$`�^���i�x�[�X�j`)
result$`�^���i�������z�j` <- as.integer(result$`�^���i�������z�j`)
result$`�V�F�A�i���сj` <- as.character(result$`�V�F�A�i���сj`)
result$`�V�F�A�i�x�[�X�j` <- as.character(result$`�V�F�A�i�x�[�X�j`)
result$`�V�F�A�i�������z�j` <- as.character(result$`�V�F�A�i�������z�j`)
result$`���v�i���сj` <- as.character(result$`���v�i���сj`)
result$`���v�i�x�[�X�j` <- as.character(result$`���v�i�x�[�X�j`)
result$`���v�i�������z�j` <- as.character(result$`���v�i�������z�j`)


index.for.table <- subset(result, , c(i, j)) %>% unique() %>% as.matrix() 
## �n��Ɖ^���E�q��֐��E�V�F�A�E���v�������Ƃ���悤�ȃe�[�u�������֐�������
## index�ł͍s��`����(i, j)��index���w��
## var1��4�����x�N�g����1�Ȃ�܂߂�C0�Ȃ�܂߂Ȃ�
## var2��3�����x�N�g���Ŏ��сC�x�[�X�C�V�~�����[�V�������܂߂�E�܂߂Ȃ����w��
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
                         caption = "���茋��", 
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
