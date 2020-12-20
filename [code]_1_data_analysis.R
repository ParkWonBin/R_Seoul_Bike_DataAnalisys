# 환경 청소
rm(list=ls())# 환경 청소 : list 설정
cat("\014")  # 콘솔 청소 : Ctrl + L

# getwd() 
# list.files(path="../", pattern = '.') 
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Bike = read.csv('../Bike-Use-final.csv')
Broken = read.csv('[csv]_Bike-Broken.csv',fileEncoding = 'utf-8')

### 데이터 초기 설정
head(ID)
head(Bike)
head(Broken)

# 날짜 데이터 기준값 맞추기
Broken$date = as.Date(Broken$date, origin = "2015-09-18") 
Bike$date = as.Date(Bike$date, origin = "2015-09-18") 

# 범주 데이터 범주로 설정
Broken$type = as.factor(Broken$type)
Broken$type

###########
# 고장 데이터 보기
# 값으로 색갈 : https://stackoverflow.com/questions/17375668/scatterplot-colour-based-on-y-value
library(dplyr)
Brok_vbike = Broken %>% count(bike)
Brok_vbike = arrange(Brok_vbike,-n)

with(Brok_vbike,
     plot(Brok_vbike, pch=20,cex=0.8,
          col=ifelse(bike<27000,alpha("red",0.2),
                     ifelse(bike<47000,alpha("green",0.2),
                            alpha("blue",0.2)))))


with(Brok_vbike,
     plot(Brok_vbike$n, pch=20,cex=1.5,
          col=ifelse(bike<27000,alpha("red",0.3),
                     ifelse(bike<47000,alpha("green",0.5),
                            alpha("blue",0.1))),
          xaxt = "n",xlab = "",ylab = "")
     )
mtext('고장 횟수',side=2,col='black',line = 2.5,cex=1.5)
mtext('고장 횟수로 정렬',side=1,col='black',line =1 ,cex=1.5)
mtext('자주 수리 받은 자전거가 잘 고장날까',side=3,col='black',line =1 ,cex=1.5)

# 2axis plot : https://stackoverflow.com/questions/41390181/ggplot2-add-separate-legend-each-for-two-y-axes-in-facet-plot
# time seq : https://stat.ethz.ch/R-manual/R-devel/library/base/html/seq.Date.html
Brok_vc = Broken %>% count(date)
Bike_vc = Bike %>% count(date)
xaxis = seq(as.Date("2015/9/1"), as.Date("2020/12/1"), by = "quarter")
xlabel= sapply(xaxis,function(x){gsub("-","\n",gsub("-01","",x))})

### 빈도

library(ggplot2)
xmin = as.Date(1, origin = "2015-09-18")
xmax = as.Date(1870, origin = "2015-09-18")
max1 = max(Broken_valcount$n)
max2 = max(Bike_valcount$n)

plot(Bike_vc,col=alpha('#00dd00',0.2), pch = 20,cex=1.5,
     ylim=c(1,max2),xlim=c(xmin,xmax),
     axes = FALSE,xlab="",ylab="")
axis(side=1, at=xaxis,labels = xlabel,cex.axis=0.8)
axis(side=2,col="#00aa00",col.axis='#00aa00',cex.axis=0.8)
mtext('대여건수',side=2,col='#00cc00',line = -1.2,cex=2)

par(new = TRUE) # 겹치기
# 고장 건수
plot(Brok_vc, col=alpha('#ff0000',0.2), pch = 20,cex=1.3,
     ylim=c(1,max1),xlim=c(xmin,xmax), 
     axes = FALSE,xlab="",ylab="")
axis(side=4, col='#dd6633',col.axis="#dd6633")
mtext('고장건수',side=4,col='#dd6633', line = -1.2,cex=2)
title("자전거 사용량과 고장 건수", cex.main=2)
##############################################################



