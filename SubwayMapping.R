rm(list = ls())

library(dplyr)
library(devtools)
library(ggmap)
library(Gmedian)
library(DMwR)
library(wordcloud)
library(RColorBrewer)

#Data merge (Subway location + theater)
setwd('C:\\Users\\jrchu\\Desktop\\빅데이터\\data')

data <- read.csv('Merging_data_final(수정).csv', stringsAsFactors = F)
head(data)
colnames(data)
str(data)

##불필요한 변수 삭제
#호선 변수삭제
data <- subset(data, select = -c(Line))

#영화관의 유무보다는 수를 숫자형 데이터로 입력하자.
data <- subset(data, select = -c(C))
data <- subset(data, select = -c(L))
data <- subset(data, select = -c(M))

str(data)


hist(data$승하차인원)
#비정상적인 분포
data[which(data$승하차인원 == max(data$승하차인원)), ]
#잠실이 가장 큰 값을 지니는 것을 알 수 있다.

#승하차인원은 총 37일의 총합 -> 평균값으로 계산한다.
data$승하차인원 <- (data$승하차인원)/37

#Buzz_sum을 평균승하차인원으로 나누어 인원대비 언급량 변수를 만든다.
data$Buzz_prop <- (data$Buzz_Sum)/(data$승하차인원)

data[which(data$Buzz_prop == max(data$Buzz_prop)), ]
#서울역의 인원대비 언급량 변수값이 너무 크다..!

##상가수의 0 : NA이므로, 평균값으로 대체

data[which(data$상가개수 == 0), '상가개수'] <- NA
data <- centralImputation(data)


##########cnt_data로 Weekly_Buzz 추출##########
cnt_data <- read.table('C:\\Users\\jrchu\\Desktop\\빅데이터\\data\\nohash\\daily_cnt.txt', sep = '\n', encoding = 'UTF-8',skip = 3)
station_cnt <- matrix(nrow = nrow(cnt_data), ncol = 9)
for (i in 1:nrow(cnt_data)) {
  {for (j in 1:9)
    station_cnt[i,j] <- unlist(strsplit(as.character(cnt_data[i,]), ','))[j]
  }
}

station_cnt <- as.data.frame(station_cnt)
station_cnt <- station_cnt[, -2]

for (i in 2:ncol(station_cnt)) {
  station_cnt[, i] <- as.integer(station_cnt[, i])
}

station_cnt$Weekly_buzz <- apply(station_cnt[, 2:8], sum, MARGIN = 1)

data$Weekly_buzz <- station_cnt$Weekly_buzz


##############Mapping작업##############


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

#Buzz_Prop에서 상위 20개의 언급량 값을 갖는 idx
idx_20 <- which(data$Buzz_prop >= sort(data$Buzz_prop, decreasing=TRUE)[20])
# all_data$Buzz_Sum[idx_20]

#상위 20개의 지하철역 맵핑 (log transformation 활용하여 원크기 조절)
MM3_20 <- MM +
  geom_point(aes(x = X, y = Y, size = Buzz_prop), data = data[idx_20,]) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data[idx_20, ]) + 
  labs(x="경도", y="위도")


###############군집분석###############


###K-means Clustering : 첫번째 시도
#혼잡도_평일, 혼잡도_주말, Buzz_Sum, 승하차인원, 좌표 삭제
mydata <- subset(data, select = c(영화관, 맛집, 카페, 데이트, 데이트코스, 술집, Buzz_prop, 상가개수, Weekly_buzz))

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b",
     main = "K-means clustering", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Elbow point를 3으로 설정

K_data <- mydata

data_kmeans <- kmeans(K_data, centers = 3, iter.max = 10000)

data$cluster_mean <- as.factor(data_kmeans$cluster)



MM4_mean <- MM +
  geom_point(aes(x = X, y = Y, color = cluster_mean), data = data, size = 3) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data) + 
  labs(x="경도", y="위도")


###K-median Clustering : 두번째 시도

K_data <- as.data.frame(mydata)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

for (i in 2:15) wss[i] <- sum(kGmedian(mydata, ncenters=i)$withinsrs)

plot(1:15, wss, type="b", 
     main = "K-median clustering", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")


plot(2:15, wss[2:15], type="b", 
     main = "K-median clustering", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Elbow point를 4로 설정

data_kmedian <- kGmedian(mydata, ncenters=4)

data$cluster_median <- as.factor(data_kmedian$cluster)
#data_kmeans$centers


MM4_median <- MM +
  geom_point(aes(x = X, y = Y, color = cluster_median), data = data, size = 3) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data) + 
  labs(x="경도", y="위도")

MM4_median_1 <- MM +
  geom_point(aes(x = X, y = Y, color = cluster_median), data = data[which(data$cluster_median == 1),] , size = 3) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data) + 
  labs(x="경도", y="위도")

MM4_median_2 <- MM +
  geom_point(aes(x = X, y = Y, color = cluster_median), data = data[which(data$cluster_median == 2),] , size = 3) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data) + 
  labs(x="경도", y="위도")

MM4_median_3 <- MM +
  geom_point(aes(x = X, y = Y, color = cluster_median), data = data[which(data$cluster_median == 3),] , size = 3) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data) + 
  labs(x="경도", y="위도")

MM4_median_4 <- MM +
  geom_point(aes(x = X, y = Y, color = cluster_median), data = data[which(data$cluster_median == 4),] , size = 3) + 
  #geom_text(aes(x= X, y= Y, label=Station), colour="red", vjust=1, size=3.5, fontface="bold", data= data) + 
  labs(x="경도", y="위도")





#######################크롤링된 해시태그로 워드클라우드 만들기#######################

hash_data <- read.table('C:\\Users\\jrchu\\Desktop\\빅데이터\\data\\hash_total.txt', sep = '\n', encoding = 'UTF-8', skip = 1)

Test <- 0
A <- list()
for (i in 1:280) {
  Test <- unlist(strsplit(as.character(hash_data[i,]), ','))
  X <- c()
  for (j in 3:ceiling(length(unlist(strsplit(as.character(hash_data[i,]), ',')))/2)) {
    X <- append(X, rep(strsplit(Test, ':')[[j]][1], as.integer(strsplit(Test, ':')[[j+1]][2])))
  }
  A[[i]] <- X
}

cluster_1_hash <- unlist(A[which(data$cluster_median == 1)])
cluster_2_hash <- unlist(A[which(data$cluster_median == 2)])
cluster_3_hash <- unlist(A[which(data$cluster_median == 3)])
cluster_4_hash <- unlist(A[which(data$cluster_median == 4)])

WC_test_1 <- sort(table(cluster_1_hash), decreasing = TRUE)
WC_test_2 <- sort(table(cluster_2_hash), decreasing = TRUE)
WC_test_3 <- sort(table(cluster_3_hash), decreasing = TRUE)
WC_test_4 <- sort(table(cluster_4_hash), decreasing = TRUE)


wordcloud(names(WC_test_1), freq = WC_test_1, scale = c(4, .2),
          rot.per = 0.025, min.freq = 5, random.order = F,
          random.color = T, colors = brewer.pal(9, 'Set1'))
##까페, 커피, coffee등
wordcloud(names(WC_test_2), freq = WC_test_2, scale = c(4, .2),
          rot.per = 0.025, min.freq = 5, random.order = F,
          random.color = T, colors = brewer.pal(9, 'Set1'))
##서울나들이
wordcloud(names(WC_test_3), freq = WC_test_3, scale = c(4, .2),
          rot.per = 0.025, min.freq = 5, random.order = F,
          random.color = T, colors = brewer.pal(9, 'Set1'))
##먹다, 음식점, 존맛...
wordcloud(names(WC_test_4), freq = WC_test_4, scale = c(4, .2),
          rot.per = 0.025, min.freq = 5, random.order = F,
          random.color = T, colors = brewer.pal(9, 'Set1'))

C1 <- data[which(data$cluster_median==1), 'Station']
C1_idx <- order(data[which(data$cluster_median==1), "Buzz_prop"], decreasing = TRUE)[1:3]
C1[C1_idx]

C2 <- data[which(data$cluster_median==2), 'Station']
C2_idx <- order(data[which(data$cluster_median==2), "Buzz_prop"], decreasing = TRUE)[1:3]
C2[C2_idx]

C3 <- data[which(data$cluster_median==3), 'Station']
C3_idx <- order(data[which(data$cluster_median==3), "Buzz_prop"], decreasing = TRUE)[1:3]
C3[C3_idx]

C4 <- data[which(data$cluster_median==4), 'Station']
C4_idx <- order(data[which(data$cluster_median==4), "Buzz_prop"], decreasing = TRUE)[1:3]
C4[C4_idx]
