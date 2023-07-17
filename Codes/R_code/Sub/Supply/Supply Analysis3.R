## Supply Analysis 3
## Preparing for counterfactual simulations

## Library
library(magrittr)
library(tidyr)

## Read data
airline.updated <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline2.csv", header = T, skip = 2, stringsAsFactors = FALSE)
OD <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/OD.csv", header = T, stringsAsFactors = FALSE)

## Transform characters into numeric
airline.updated$�^�q�� <- as.numeric(gsub(",", "", airline.updated$�^�q��)) # gsub is used to remove commas in each figure
airline.updated$�^�q�L�����[�g�� <- as.numeric(gsub(",", "", airline.updated$�^�q�L�����[�g��))
airline.updated$���q�� <- as.numeric(gsub(",", "", airline.updated$���q��))
airline.updated$���Ȑ� <- as.numeric(gsub(",", "", airline.updated$���Ȑ�))
airline.updated$�ݕ� <- as.numeric(gsub(",", "", airline.updated$�ݕ�))
## Some variables necessary to analyse data
airline.updated <- airline.updated %>% 
  dplyr::mutate(DIS = �^�q�L�����[�g��/�^�q��,
                Seat = ���Ȑ�/�^�q��,
                Freq = ���q��/(Seat*���ȗ��p��/100), 
                MC = 11*DIS) # I have to revise this! Unit cost should be obtained from annual reports

## �q���Ђ��ƂɌ��E��p�𒲐�����
## �����ƑS�����
## �Ƃ肠���������͈ȉ�
## JAL, ANA, JTA, JAC, IBX, HAC = 11
## ADO, SFJ = 9
## SKY = 5.5
airline.updated[which(airline.updated$�q���� == "SFJ"), 26] <- (9/11)*airline.updated[which(airline.updated$�q���� == "SFJ"), 26] ## SFJ�̓��j�b�g�R�X�g��9
airline.updated[which(airline.updated$�q���� == "ADO"), 26] <- (9/11)*airline.updated[which(airline.updated$�q���� == "ADO"), 26]
airline.updated[which(airline.updated$�q���� == "SKY"), 26] <- (5.5/11)*airline.updated[which(airline.updated$�q���� == "SKY"), 26]

## �����\�ƍ��y��ʏȃf�[�^�ňقȂ�G�A���C��������v������
## �W�F�C�E�G�A��JAL
## �W�F�C�G�A�͔r�������i�΁j

## �����Ƀt�H�[�J�X
ODnames.updated <- c("����", "���c", "�֐�", "���", "�_��", "��I���l", "����", "�k��B", "����", "�V���", "�u��", "�ߔe", "���", "����")
ODNo.updated <- c(rep(18, 2), rep(25, 4), rep(37, 3), rep(8, 2), 46, 12, 3)
ODcorrespondence.updated <- data.frame(ODnames = ODnames.updated, ODNo = ODNo.updated)

airline.updated2 <- airline.updated %>% 
  dplyr::filter(��`i == "����" | ��`i == "���c" | ��`i == "�֐�" | ��`i == "���" | ��`i == "�_��" | ��`i == "��I���l" | ��`i == "����" | ��`i == "�k��B" | ��`i == "����" | ��`i == "�V���" | ��`i == "�u��" | ��`i == "�ߔe" | ��`i == "���" | ��`i == "����") %>% 
  dplyr::filter(��`j == "����" | ��`j == "���c" | ��`j == "�֐�" | ��`j == "���" | ��`j == "�_��" | ��`j == "��I���l" | ��`j == "����" | ��`j == "�k��B" | ��`j == "����" | ��`j == "�V���" | ��`j == "�u��" | ��`j == "�ߔe" | ��`j == "���" | ��`j == "����")
for(k in 1:2){
  for(i in 1:nrow(airline.updated2)){
    for(j in 1:nrow(ODcorrespondence.updated)){
      if(airline.updated2[i, k] == ODcorrespondence.updated[j, 1]){
        airline.updated2[i, k + 26] <- ODcorrespondence.updated[j, 2]
      } else {
        
      }
    }
  }
}

## Read data
## airline_revised.csv��airline.csv�ƃt�H�[�}�b�g�i������낦��j
airline.updated3 <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline_revised.csv", header = T)

## Filter data
## �����݂̂Ƀt�H�[�J�X
airline.updated3 <- airline.updated3 %>% 
  dplyr::filter(��`i == "����" | ��`i == "���c" | ��`i == "�֐�" | ��`i == "���" | ��`i == "�_��" | ��`i == "��I���l" | ��`i == "����" | ��`i == "�k��B" | ��`i == "����" | ��`i == "�V���" | ��`i == "�u��" | ��`i == "�ߔe" | ��`i == "���" | ��`i == "����") %>% 
  dplyr::filter(��`j == "����" | ��`j == "���c" | ��`j == "�֐�" | ��`j == "���" | ��`j == "�_��" | ��`j == "��I���l" | ��`j == "����" | ��`j == "�k��B" | ��`j == "����" | ��`j == "�V���" | ��`j == "�u��" | ��`j == "�ߔe" | ��`j == "���" | ��`j == "����")

## Attach airline data to airline.updated2
airline.updated2$��`i <- as.character(airline.updated2$��`i)
airline.updated2$��`j <- as.character(airline.updated2$��`j)
airline.updated2$�q���� <- as.character(airline.updated2$�q����)
airline.updated3$��`i <- as.character(airline.updated3$��`i)
airline.updated3$��`j <- as.character(airline.updated3$��`j)
airline.updated3$�q���� <- as.character(airline.updated3$�q����)

for(k in 1:2){
  for(i in 1:nrow(airline.updated3)){
    for(j in 1:nrow(ODcorrespondence.updated)){
      if(airline.updated3[i, k + 3] == ODcorrespondence.updated[j, 1]){
        airline.updated3[i, k + 19] <- ODcorrespondence.updated[j, 2]
      } else {
        
      }
    }
  }
}

airlinenames.updated <- c("JAL", "ANA", "SFJ", "SKY", "IBX", "JAC", "ADO", "AMX", "JTA", "HAC", "JEX") ## ����ԗ�����A�������͂���
airlineNo.updated <- c(seq(1:11))
airlinecorrespondence.updated <- data.frame(airlinenames = airlinenames.updated, airlineNo = airlineNo.updated)
airlinecorrespondence.updated$airlinenames <- as.character(airlinecorrespondence.updated$airlinenames)

for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(airlinecorrespondence.updated)){
    if(airline.updated2[i, 3] == airlinecorrespondence.updated[j, 1]){
      airline.updated2[i, 29] <- airlinecorrespondence.updated[j, 2]
    } else {
      
    }
  }
}
for(i in 1:nrow(airline.updated3)){
  for(j in 1:nrow(airlinecorrespondence.updated)){
    if(airline.updated3[i, 8] == airlinecorrespondence.updated[j, 1]){
      airline.updated3[i, 22] <- airlinecorrespondence.updated[j, 2]
    } else {
      
    }
  }
}
write.csv(airline.updated2, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline.updated2�m�F.csv")
write.csv(airline.updated3, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline.updated3�m�F.csv")


for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(airline.updated3)){
    if(airline.updated2[i, 1] == airline.updated3[j, 4] && airline.updated2[i, 2] == airline.updated3[j, 5] && airline.updated2[i, 29] == airline.updated3[j, 22]){
      (airline.updated2[i, 30] <- airline.updated3[j, 9]) & (airline.updated2[i, 31] <- airline.updated3[j, 10]) & (airline.updated2[i, 32] <- airline.updated3[j, 12]) & (airline.updated2[i, 33] <- airline.updated3[j, 13]) & (airline.updated2[i, 34] <- airline.updated3[j, 15])
    } else {
      
    }
  }
}

colnames(airline.updated2)[27] <- "i"
colnames(airline.updated2)[28] <- "j"
colnames(airline.updated2)[29] <- "airlineNo"
colnames(airline.updated2)[30] <- "���v����"
colnames(airline.updated2)[31] <- "��p"
colnames(airline.updated2)[32] <- "�q��֐�"
colnames(airline.updated2)[33] <- "�؍݉\����"
colnames(airline.updated2)[34] <- "�G�A���C���Q����"
## Drop NAs 
airline.updated2 <- airline.updated2 %>% 
  dplyr::filter(!is.na(�G�A���C���Q����)) %>%  # �G�A���C���Q���� has no particular meaning! it is just appropriate to remove NAs 
  dplyr::filter(!is.na(Freq)) %>% 
  dplyr::arrange(i, j) %>%  
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(ODairline = sum(���q��))

## railway data
OD <- OD %>% 
  dplyr::filter(X == 18 | X == 25 | X == 37 | X == 8 | X == 46 | X == 12 | X == 3) %>% 
  dplyr::filter(X.1 == 18 | X.1 == 25 | X.1 == 37 | X.1 == 8 | X.1 == 46 | X.1 == 12 | X.1 == 3) %>% 
  dplyr::filter(X != X.1)
OD <- OD[ , c(1, 2, 3, 4, 6)]
OD$railway <- OD$railway*1000

## attach railway demand data to airline.updated
for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(OD)){
    if(airline.updated2[i, 27]==OD[j, 1] && airline.updated2[i, 28]==OD[j, 2]){
      airline.updated2[i, 36] <- OD[j, 5]
    }
  }
}
colnames(airline.updated2)[36] <- "ODrailway"
## Derive ODs
airline.updated2 <- airline.updated2 %>% 
  dplyr::mutate(OD = ODairline + ODrailway)

## Add accessibility
## �V��΂Ɠߔe�ɑΉ�������
## �C�O���X�ɑΉ�������
## ���ꂾ�ƑS�H���Ɋg������̒n��������
accessibility <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/accessibility.csv", header = T)
accessibility$�A�N�Z�V�r���e�B <- accessibility$�A�N�Z�V�r���e�B/1000
accessibility$�w.��` <- as.character(accessibility$�w.��`)
for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(accessibility)){
    if(airline.updated2[i, 1] == accessibility[j, 2] && accessibility[j, 3] == 0){
      airline.updated2[i, 38] <- accessibility[j, 4]
    } else {
      
    }
  }
}
for(i in 1:nrow(airline.updated2)){
  for(j in 1:nrow(accessibility)){
    if(airline.updated2[i, 2] == accessibility[j, 2] && accessibility[j, 3] == 0){
      airline.updated2[i, 38] <- airline.updated2[i, 38] + accessibility[j, 4]
    } else {
      
    }
  }
}
colnames(airline.updated2)[38] <- "�A�N�Z�V�r���e�B"

## Add ��ւ�������Ȑ� and ��ւ�����ݕ�
airline.updated2 <- airline.updated2 %>% 
  dplyr::mutate(�S���_�~�[ = 0, 
                ��ւ�������Ȑ� = ���Ȑ�/�^�q��, 
                ��ւ�����ݕ� = �ݕ�/�^�q��)
## Write csv
write.csv(airline.updated2, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline.updated2.csv")

## Introduce a new data frame
data.updated <- airline.updated2[ , c(27, 28, 1, 2, 3, 7, 30, 31, 32, 33, 38, 34, 39, 40, 41)]
## Read data
data2railway <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data2railway.csv", header = T, stringsAsFactors = FALSE)
data2railway <- as.list(data2railway)

## Counterfactual data
## �����ɂ���ւ�������Ȑ��ƈ�ւ�����ݕ���ǉ�����
## base1.csv��ǉ�����
## �x�[�X�����Ƃ���base1��bind����
counterfactual1 <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/counterfactual1.csv", header = T, stringsAsFactors = FALSE)
counterfactual1 <- as.list(counterfactual1)
base1 <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/base1.csv", header = T, stringsAsFactors = FALSE)
base1 <- as.list(base1)

data.updated <- rbind(data.updated, data2railway, counterfactual1) ## bind counterfactual as well!
write.csv(data.updated, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data_updated�m�F.csv")
for(i in 1:nrow(data.updated)){
  for(j in 1:nrow(OD)){
    if(data.updated[i, 1] == OD[j, 1] && data.updated[i, 2] == OD[j, 2] && data.updated[i, 13] == 1){
      data.updated[i, 6] <- OD[j, 5]
    }
  }
}
data.updated <- data.updated %>% 
  dplyr::arrange(i, j)

## Add OD
data.updated <- data.updated %>% 
  dplyr::group_by(i, j) %>% 
  dplyr::mutate(OD = sum(���q��)) %>% 
  dplyr::ungroup() 


## Add LF, Freq, DIS, and MC
for(i in 1:nrow(data.updated)){
  for(j in 1:nrow(airline.updated2)){
    if(airline.updated2[j, 1] == data.updated[i, 3] && airline.updated2[j, 2] == data.updated[i, 4] && airline.updated2[j, 3] == data.updated[i, 5]){
      (data.updated[i, 17] <- airline.updated2[j, 23]) & (data.updated[i, 18] <- airline.updated2[j, 24]) & (data.updated[i, 19] <- airline.updated2[j, 4]) & (data.updated[i, 20] <- airline.updated2[j, 26]) & (data.updated[i, 21] <- airline.updated2[j, 12]) 
    } else {
      
    }
  }
}
colnames(data.updated)[17] <- "DIS"
colnames(data.updated)[18] <- "Seat"
colnames(data.updated)[19] <- "Freq"
colnames(data.updated)[20] <- "MC"
colnames(data.updated)[21] <- "LF"




## Add observedprob
data.updated <- data.updated %>% 
  dplyr::mutate(observedprob = ���q��/OD)

write.csv(data.updated, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/data_updated.csv")
