## IV Estimation2
## Add observable quality of airline

## Library
library(magrittr)

## 
airline.as.IV <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline_updated.csv", header = T)
airport.number <- read.csv("C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airportNo47.csv", header = T)

airline.as.IV <- merge(airline.as.IV, airport.number, by.x = c("��`i"), 
                       by.y = "��`", all.x = T)
airline.as.IV <- merge(airline.as.IV, airport.number, by.x = c("��`j"), 
                       by.y = "��`", all.x = T)

## Drop rows with NA
airline.as.IV <- airline.as.IV[-which(is.na(airline.as.IV$No.x) | is.na(airline.as.IV$No.y)), ]
airline.as.IV <- airline.as.IV %>% 
  dplyr::group_by()

## Delete commas
airline.as.IV$�l�L�����[�g�� <- as.numeric(gsub(",", "", airline.as.IV$�l�L�����[�g��))
airline.as.IV$���ȃL�����[�g�� <- as.numeric(gsub(",", "", airline.as.IV$���ȃL�����[�g��))
airline.as.IV$�ݕ� <- as.numeric(gsub(",", "", airline.as.IV$�ݕ�))
airline.as.IV$���ߎ�ו� <- as.numeric(gsub(",", "", airline.as.IV$���ߎ�ו�))
airline.as.IV$�X�֕� <- as.numeric(gsub(",", "", airline.as.IV$�X�֕�))
airline.as.IV$���q <- as.numeric(gsub(",", "", airline.as.IV$���q))
airline.as.IV$�X�֕� <- as.numeric(gsub(",", "", airline.as.IV$�X�֕�))
airline.as.IV$�ݕ�.1 <- as.numeric(gsub(",", "", airline.as.IV$�ݕ�.1))
airline.as.IV$���ߎ�ו�.1 <- as.numeric(gsub(",", "", airline.as.IV$���ߎ�ו�.1))
airline.as.IV$�X�֕�.1 <- as.numeric(gsub(",", "", airline.as.IV$�X�֕�.1))
airline.as.IV$�v <- as.numeric(gsub(",", "", airline.as.IV$�v))
airline.as.IV$���p�\.�g���L�����[�g�� <- as.numeric(gsub(",", "", airline.as.IV$���p�\.�g���L�����[�g��))

airline.as.IV <- airline.as.IV %>% 
  dplyr::mutate(��ւ�������Ȑ� = ���Ȑ�/�^�q��, 
                ��ւ�����ݕ� = �ݕ�/�^�q��, 
                ��ւ����蒴�ߎ�ו� = ���ߎ�ו�/�^�q��, 
                �H������ = �^�q�L�����[�g��/�^�q��)

airline.as.IV <- airline.as.IV[, c(28:33)]

airline.as.IV <- airline.as.IV %>% 
  dplyr::group_by(No.x, No.y) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        vars = c('��ւ�������Ȑ�', '��ւ�����ݕ�', '��ւ����蒴�ߎ�ו�', '�H������'))
colnames(airline.as.IV) <- c("i", "j", "��ւ�������Ȑ�", "��ւ�����ݕ�", "��ւ����蒴�ߎ�ו�", "�H������")

write.csv(airline.as.IV, "C:/Users/kayab/yuki/��w�u�`/��w�@�[�~/Data/airline_as_IV�m�F.csv")

