# 환경 청소
rm(list=ls())# 환경 청소 : list 설정
cat("\014")  # 콘솔 청소 : Ctrl + L

# getwd() 
# list.files(path="../", pattern = '.') 
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Broken = read.csv('../서울시 공공자전거 고장신고 내역_2015_2020.10.csv')

#######################################################################
names(ID)
# c("대여소.번호","보관소.대여소.명","자치구","상세주소","위도","경도","설치.시기","LCD","QR","운영.방식")     
names(Broken)
# c("자전거번호","등록일시","고장구분")
unique(Broken$고장구분)
#c("체인","기타 ","단말기","안장","페달","타이어 ","파손","잠금장치 불량")
Broken
########################################################################
date = as.Date('2015-9-18')
types = c("체인","기타 ","단말기","안장","페달","타이어 ","파손","잠금장치 불량")

BR = data.frame(bike = as.numeric(gsub('SPB-',"",Broken$자전거번호)),
                date = as.Date(substr(Broken$등록일시,1,10),'%Y-%m-%d')-date,
                type = factor(match(Broken$고장구분,types),labels =types))
?factor
summary(BR$type)
head(BR,3, row.names=FALSE)
# write.csv(BR, file='Bike-Broken.csv', row.names = FALSE)
########################################################################
library(dplyr)
friq = BR %>% count(bike) # 빈도 수 세기
friq = friq[order(friq$n,decreasing =TRUE),]
plot(friq)
plot(friq$n)
friq[order(friq$n),]
?order

########################################################################
head(BR)
plot(BR$type)
table(BR)


length(unique(sapply(BR$bike,as.character))) # 접수된 자전거 수 3400
length(BR[,1]) # 전체 신고 건수 : 181592
length(unique(sapply(Bike$bike,as.character))) # 전체 자전거 4971개
length(Bike[,1]) # 전체 이용건수 2043069
########################################################################
p = "../2015~2020_자전거 코드별 이용정보/"
f = list.files(path=p, pattern = '.csv') 
x = read.csv(paste0(p,f[1]))
for (i in 2:length(f)){
  x = rbind(x,read.csv(paste0(p,f[i])))
}
########################################################################
date = as.Date('2015-9-18')
bike = x[,c(1,6,3,7,10,11)]
Bike = data.frame(bike = as.numeric(gsub('SPB-',"",bike$자전거번호)),
                date = as.Date(substr(bike$반납일시,1,10),'%Y-%m-%d')-date,
                start= bike$대여소번호,
                end = bike$반납대여소번호,
                time = bike$이용시간.분.,
                dist = bike$이용거리.M.)
# write.csv(Bike, file='Bike-Use.csv', row.names = FALSE)
?write.csv
head(Bike)
########################################################################
# 참고문헌헌
# 문자열 제어 : https://rfriend.tistory.com/37
# 날짜 데이터 제어 : https://data-make.tistory.com/35
# csv로 저장 : https://thebook.io/006723/ch04/02/01/
# 항목 빈도수 : https://www.r-bloggers.com/2010/02/r-sorting-a-data-frame-by-the-contents-of-a-column/

Bike = read.csv('Bike-Use.csv')
BR = read.csv('Bike-Broken.csv',fileEncoding = 'utf-8')
head(BR)

match(BR$type,unique(BR$type))
types = c("체인","기타 ","단말기","안장","페달","타이어 ","파손","잠금장치 불량")
unique(BR$type)

plot(x = Bike$bike , y = Bike$date)
