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

###각 변수별 분포 확인하기
head(all_data)

#plot을 그리는데 필요한 열만 남기고 나머지 열은 모두 삭제 
all_data_p <- subset(all_data, select = c('Sum', '맛집', '카페', '데이트', '데이트코스', '술집', 'Buzz_Sum'))
head(all_data_p)

#변수 설정 
subway<-all_data_p$Station
theater<-all_data_p$Sum
food<-all_data_p$맛집
cafe<-all_data_p$카페
date<-all_data_p$데이트
date2<-all_data_p$데이트코스
bar<-all_data_p$술집
buzzsum<-all_data_p$Buzz_Sum

#결측값 제거 
theater[is.na(theater)] <- 0
food[is.na(food)] <- 0
cafe[is.na(cafe)] <- 0
date[is.na(date)] <- 0
date2[is.na(date2)] <- 0
bar[is.na(bar)] <- 0
buzzsum[is.na(buzzsum)] <- 0


data_p<-cbind(theater,food,cafe,date,date2,bar,buzzsum)
head(data_p)


#boxplot - 원본 데이터
boxplot(data_p,col=rainbow(7),ylim=c(0,20000),main="Boxplot of data")
#theater이 다른 데이터에 비해 심하게 데이터 수가 적음을 알 수 있다.

#histogram - 원본 데이터 
par(mfrow=c(3,3))
hist(theater)
hist(food)
hist(cafe)
hist(date,xlim=c(0,10000))
hist(date2,xlim=c(0,1000))
hist(bar,xlim=c(0,10000))
hist(buzzsum,xlim=c(0,500000))
#히스토그램을 통해서 확인해보니 데이터가 분포 차이가 심하다는 것을 알 수 있다.

#데이터를 로그변환함 
theater2<-log(theater)
food2<-log(food)
cafe2<-log(cafe)
date3<-log(date)
date4<-log(date2)
bar2<-log(bar)
buzzsum2<-log(buzzsum)

data_p2<-cbind(theater2,food2,cafe2,date3,date4,bar2,buzzsum2)
head(data_p2)

#boxplot - 로그변환 데이터 
boxplot(data_p2,col=rainbow(7),ylim=c(-5,20),main="Boxplot of logdata")
#data를 log 변환하니 데이터사이의 분포 차이가 좀 줄었음을 알 수 있다.

#histogram - 로그변환 데이터 
par(mfrow=c(3,3))
hist(theater2)
hist(food2)
hist(cafe2)
hist(date3)
hist(date4)
hist(bar2)
hist(buzzsum2)
#로그변환을하니 어느정도 변수들간의 차이가 줄어듬을 알 수 있다.



#######수집데이터 모양 확인하기.
lapply(all_data[, -c(1, 2)], summary)

#강남역이 150만으로 다른 지하철역에 비하여 압도적인 언급량을 보이는 것을 알 수 있다. Outlier의 가능성!
pairs(log(all_buzz[, -1]))
pairs(all_buzz[, -1])
#술집&맛집&카페&데이트&총계 간의 상관관계를 scatter_plot으로 확인, 이상하게 데이트코스 변수는 상관관계가 약해보인다.

##Buzz_Sum의 데이터 모양 확인하기
par(mfrow = c(2,1))
hist(all_data$Buzz_Sum) #너무 왼쪽에 치우쳐 있다. 
##Log_Transformation!!
hist(log(all_data$Buzz_Sum)) #강남역 빼고는 볼만하다.


####




rm(list = ls())

library(dplyr)
library(devtools)
library(ggmap)

#Data merge (Subway location + theater)
setwd('C:\\Users\\jrchu\\Desktop\\빅데이터\\data')

data <- read.csv('Merging_data_final(수정).csv', stringsAsFactors = F)
head(data)
colnames(data)
str(data)

data <- subset(data, select = -c(Line))
data <- subset(data, select = -c(C))
data <- subset(data, select = -c(L))
data <- subset(data, select = -c(M))

str(data)
hist(data$승하차인원)

#승하차인원은 총 37일의 총합 -> 평균값으로 계산한다.
data$승하차인원 <- (data$승하차인원)/37

#Buzz_sum을 평균승하차인원으로 나누어 인원대비 언급량 변수를 만든다.
data$Buzz_prop <- (data$Buzz_Sum)/(data$승하차인원)

data[which(data$Buzz_prop == max(data$Buzz_prop)), ]
#서울역의 인원대비 언급량 변수값이 너무 적다..!
str(data)


###############Mapping작업###############


##구글맵 API
key <- ""
register_google(key = key)


Map_Seoul <- get_map(location=c(lat=37.55, lon=126.97), zoom=11, maptype="roadmap") # 서울역의 위도 경도

#서울역 기준 서울 지도
MM <- ggmap(Map_Seoul)

##좌표 기준 280개 지하철역 맵핑 (1개의 지하철역 영역 밖)
MM2 <- MM +
  geom_point(aes(x = X , y = Y), data = data)
##점이 몰려있다. 어쩌면 특정 동/구를 핫플로 찾아낼 수 있을까?

#조금 더 확대해서 상위 N개의 역만 나타내보자.
Map_Seoul_B <- get_map(location=c(lat=37.55, lon=126.97), zoom=11, maptype="roadmap")
MM_B <- ggmap(Map_Seoul_B)

#Buzz_Prop에서 상위 20개의 언급량 값을 갖는 idx
idx_20 <- which(data$Buzz_prop >= sort(data$Buzz_prop, decreasing=TRUE)[20])
# all_data$Buzz_Sum[idx_20]

#상위 20개의 지하철역 맵핑 (log transformation 활용하여 원크기 조절)
MM3_20 <- MM_B +
  geom_point(aes(x = X, y = Y, size = Buzz_prop), data = data[idx_20,]) + 
  geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data[idx_20, ]) + 
  labs(x="경도", y="위도")


###############군집분석###############


###K-means Clustering : 첫번째 시도

mydata <- subset(data, select = c(Sum, 맛집, 카페, 데이트, 데이트코스, 술집, Buzz_Sum, 승하차인원, 혼잡도_평일, 혼잡도_주말, 상가개수))
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Elbow point를 4로 설정

K_data <- as.data.frame(mydata)
colnames(K_data)
#K_data <- K_data[-which(K_data$맛집>10), ]

data_kmeans <- kmeans(K_data, centers = 4, iter.max = 10000)

data$cluster <- as.factor(data_kmeans$cluster)
K_data$cluster <- as.factor(data_kmeans$cluster)
#data_kmeans$centers

qplot(Y, X, colour = cluster, data = data[idx_20, ])

MM4 <- MM_B +
  geom_point(aes(x = X, y = Y, color = cluster), data = data[idx_20, ]) + 
  geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data[idx_20, ]) + 
  labs(x="경도", y="위도")
