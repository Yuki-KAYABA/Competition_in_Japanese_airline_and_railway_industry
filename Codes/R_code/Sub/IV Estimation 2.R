## IV Estimation2
## Add observable quality of airline

## Library
library(magrittr)

## 
airline.as.IV <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline_updated.csv", header = T)
airport.number <- read.csv("C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airportNo47.csv", header = T)

airline.as.IV <- merge(airline.as.IV, airport.number, by.x = c("空港i"), 
                       by.y = "空港", all.x = T)
airline.as.IV <- merge(airline.as.IV, airport.number, by.x = c("空港j"), 
                       by.y = "空港", all.x = T)

## Drop rows with NA
airline.as.IV <- airline.as.IV[-which(is.na(airline.as.IV$No.x) | is.na(airline.as.IV$No.y)), ]
airline.as.IV <- airline.as.IV %>% 
  dplyr::group_by()

## Delete commas
airline.as.IV$人キロメートル <- as.numeric(gsub(",", "", airline.as.IV$人キロメートル))
airline.as.IV$座席キロメートル <- as.numeric(gsub(",", "", airline.as.IV$座席キロメートル))
airline.as.IV$貨物 <- as.numeric(gsub(",", "", airline.as.IV$貨物))
airline.as.IV$超過手荷物 <- as.numeric(gsub(",", "", airline.as.IV$超過手荷物))
airline.as.IV$郵便物 <- as.numeric(gsub(",", "", airline.as.IV$郵便物))
airline.as.IV$旅客 <- as.numeric(gsub(",", "", airline.as.IV$旅客))
airline.as.IV$郵便物 <- as.numeric(gsub(",", "", airline.as.IV$郵便物))
airline.as.IV$貨物.1 <- as.numeric(gsub(",", "", airline.as.IV$貨物.1))
airline.as.IV$超過手荷物.1 <- as.numeric(gsub(",", "", airline.as.IV$超過手荷物.1))
airline.as.IV$郵便物.1 <- as.numeric(gsub(",", "", airline.as.IV$郵便物.1))
airline.as.IV$計 <- as.numeric(gsub(",", "", airline.as.IV$計))
airline.as.IV$利用可能.トンキロメートル <- as.numeric(gsub(",", "", airline.as.IV$利用可能.トンキロメートル))

airline.as.IV <- airline.as.IV %>% 
  dplyr::mutate(一便あたり座席数 = 座席数/運航回数, 
                一便あたり貨物 = 貨物/運航回数, 
                一便あたり超過手荷物 = 超過手荷物/運航回数, 
                路線距離 = 運航キロメートル/運航回数)

airline.as.IV <- airline.as.IV[, c(28:33)]

airline.as.IV <- airline.as.IV %>% 
  dplyr::group_by(No.x, No.y) %>% 
  dplyr::summarise_each(dplyr::funs(mean), 
                        vars = c('一便あたり座席数', '一便あたり貨物', '一便あたり超過手荷物', '路線距離'))
colnames(airline.as.IV) <- c("i", "j", "一便あたり座席数", "一便あたり貨物", "一便あたり超過手荷物", "路線距離")

write.csv(airline.as.IV, "C:/Users/kayab/yuki/大学講義/大学院ゼミ/Data/airline_as_IV確認.csv")


