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

colnames(data) <- c('Station', 'Y', 'X')
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


Map_Seoul <- get_map(location=c(lat=37.55, lon=126.97), zoom=11, maptype="roadmap") # 서울역의 위도 경도

#서울역 기준 서울 지도
MM <- ggmap(Map_Seoul)

##좌표 기준 280개 지하철역 맵핑 (1개의 지하철역 영역 밖)
MM2 <- MM +
  geom_point(aes(x= X , y= Y), data = all_data) 


#######수집데이터 모양 확인하기.
lapply(all_data[, -c(1, 2)], summary)

all_data[which(all_data$Buzz_Sum == max(all_data$Buzz_Sum)), ]
#강남역이 150만으로 다른 지하철역에 비하여 압도적인 언급량을 보이는 것을 알 수 있다. Outlier의 가능성!


##Buzz_Sum의 데이터 모양 확인하기
par(mfrow = c(2,1))
hist(all_data$Buzz_Sum)
#너무 왼쪽에 치우쳐 있다. 

##Log_Transformation!!
hist(log(all_data$Buzz_Sum))
#강남역 빼고는 볼만하다.


colnames(all_data)

all_data$Sum
table(all_data$Sum)
#0이 213개, 0.3이 3개, 0.5가 11개, 1이 38개, 1.5가 5개, 2가 7개, 2.5가 3개
# -> 올바른 지표로 사용가능할까? 가중치를 얼마나 주는게 영향력있을까?

##일단 Buzz 데이터와 theater 데이터를 분할해서 모양을 확인하자.
all_buzz <- all_data[, c('Station', '맛집', '카페', '데이트', '데이트코스', '술집', 'Buzz_Sum')]
all_theater <- all_data[, c('Station' ,'C', 'L', 'M', 'Sum')]

