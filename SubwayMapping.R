rm(list = ls())
setwd('C:\\github\\Project\\SubwayMapping')
data <- read.csv('subway.csv')

theater <- read.csv('theater.csv', sep = '\t')
theater[is.na(theater)] <- 0
seoulsubway <- as.vector(unique(theater$'지하철역'))



sum(is.na(data[,'X좌표']))
data <- data[!is.na(data[,'X좌표']), ]
data$전철역명 <- as.vector(data$전철역명)

seoulsubway[which(seoulsubway == '총신대입구역/이수역')] <- '총신대입구(이수)역'
seoulsubway[which(seoulsubway == '양평역')] <- '양평(경의중앙선)역'
data$전철역명[which(data$전철역명 == '서울(경의중앙선)')] <- '서울'
data$전철역명[which(data$전철역명 == '신촌(경의중앙선)')] <- '신촌'

data$전철역명[which(data$전철역명 == '효창공원앞')] <- '효창공원'


order(seoulsubway)
data$전철역명 <- paste(data$전철역명, '역', sep = "")
data <- subset(data, data$'전철역명' %in% seoulsubway)
unique(data$'전철역명')
