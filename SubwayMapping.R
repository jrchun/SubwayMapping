rm(list = ls())
setwd('C:\\github\\Project\\SubwayMapping')
data <- read.csv('subway.csv')

colnames(data)
sum(is.na(data[,'X좌표']))

data <- data[!is.na(data[,'X좌표']), ]
unique(data[,'전철역명'])
