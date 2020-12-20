# 환경 청소
rm(list=ls())# 환경 청소 : list 설정
cat("\014")  # 콘솔 청소 : Ctrl + L

# getwd() 
# list.files(path="../", pattern = '.') 
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Bike = read.csv('Bike-Use.csv')
BR = read.csv('Bike-Broken.csv',fileEncoding = 'utf-8')

BR$date = as.Date(BR$date, origin = "2015-09-18") # 날짜 기준 맞추기
Bike$date = as.Date(Bike$date, origin = "2015-09-18") # 날짜 기준 맞추기

head(BR)
head(ID)
head(Bike)


library(dplyr)
friq = BR %>% count(date) # 빈도 수 세기
plot(friq, format='%Y.%m')

friq2 = Bike %>% count(date)
plot(friq2, format='%Y.%m')


library(datetime)
datetime(2016,11,12)

names(ID)
ID[,c(1,2,3,7)]
