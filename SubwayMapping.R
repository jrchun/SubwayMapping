rm(list = ls())

library(dplyr)
library(devtools)
library(ggmap)

#Data merge (Subway location + theater)
setwd('C:\\Users\\jrchu\\Desktop\\빅데이터\\data')

data <- read.csv('subway.csv', stringsAsFactors = F)
theater <- read.csv('theater_1.csv', stringsAsFactors = F)
theater[is.na(theater)] <- 0
buzz <- read.csv('buzz.csv', sep = '\t', stringsAsFactors = F)
buzz_2 <- read.csv('buzz_2.csv', stringsAsFactors = F)
colnames(buzz) <- colnames(buzz_2)
buzz_all <- rbind(buzz, buzz_2)

sum(is.na(data[,'X좌표']))
data <- data[!is.na(data[,'X좌표']), ]
data$전철역명 <- as.vector(data$전철역명)
data <- subset(data, select = -c(X좌표))
data <- subset(data, select = -c(Y좌표))
data <- subset(data, select = -c(전철역코드))
data <- subset(data, select = -c(호선))
data <- subset(data, select = -c(외부코드))
data <- subset(data, select = -c(사이버스테이션))

colnames(data) <- c('Station', 'X', 'Y')
colnames(theater) <- c('Line', 'Station', 'C', 'L', 'M', 'Sum')
colnames(buzz_all) <- c('Station', 'Buzz_Sum', '맛집', '카페', '데이트', '데이트코스', '술집')
buzz_all <- buzz_all[,c(1, 3:7, 2)]

theater$Station[which(theater$Station == '총신대입구역/이수역')] <- '총신대입구(이수)역'
buzz_all$Station[which(buzz_all$Station == '이수')] <- '총신대입구(이수)'
theater$Station[which(theater$Station == '우정산역')] <- '우장산역'
data$Station[which(data$Station == '양평(경의중앙선)')] <- '양평'
data$Station[which(data$Station == '서울(경의중앙선)')] <- '서울'
data$Station[which(data$Station == '신촌(경의중앙선)')] <- '신촌'
data$Station[which(data$Station == '효창공원앞')] <- '효창공원'

data$Station <- paste(data$Station, '역', sep = "")
buzz_all$Station <- paste(buzz_all$Station, '역', sep = "")
data <- subset(data, data$Station %in% theater$Station)

data <- data[!duplicated(data$Station), ]

data_buzz <- merge(data, buzz_all, key = 'Station', all = TRUE)
all_data <- merge(theater, data_buzz, key = 'Station', all = TRUE)
all_data <- all_data[!is.na(all_data[,'X']), ]
#NA가 포함된 데이터
all_data[!complete.cases(all_data),]

nrow(all_data)
write.csv(all_data, 'Merging_data_final.csv')
colSums(is.na(all_data))



##구글맵 API
key <- ""
register_google(key = key)

str(all_data)
range(all_data$X)
range(all_data$Y)

Map_Seoul <- get_map(location=c(lat=37.55, lon=126.97), zoom=11, maptype="roadmap") # 서울역의 위도 경도

MM <- ggmap(Map_Seoul)
MM2 <- MM +
  geom_point(aes(x= X , y= Y), data = all_data)

