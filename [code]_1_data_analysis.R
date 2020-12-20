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
Brok_vc = Broken %>% count(date)
Bike_vc = Bike %>% count(date)

Brok_vbike = Broken %>% count(bike)
Brok_vbike = arrange(Brok_vbike,-n)

xmin = as.Date(1, origin = "2015-09-18")
xmax = as.Date(1870, origin = "2015-09-18")
max1 = max(Brok_vc$n)
max2 = max(Bike_vc$n)
##############################################################

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
xaxis = seq(as.Date("2015/9/1"), as.Date("2020/12/1"), by = "quarter")
xlabel= sapply(xaxis,function(x){gsub("-","\n",gsub("-01","",x))})

### 빈도
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
# 회귀 준비비
##############################################################
# r 색갈 : https://stat.ethz.ch/R-manual/R-patched/library/grDevices/html/palettes.html
library(dplyr)
library(lubridate)
Brok_vc = Broken %>% count(date)
Bike_vc = Bike %>% count(date)

names(Brok_vc) = c('date','broken')
names(Bike_vc) = c('date','rental')
m1 = merge(Brok_vc,Bike_vc,by='date') # 겹치는 것만 들어감감


{par(bg='black')
        with(m1,
             plot(rental,broken,pch = 20,cex=1.5,,xlab="",ylab="",
                  col=rainbow(12,alpha = 0.7)[month(date)]
             )
        )
        axis(side=1,col='#00cc00',col.axis='#00cc00')
        axis(side=2,col='#dd6633',col.axis="#dd6633")
        mtext('대여건수',side=1,col='#00cc00', line = 2.5,cex=2)
        mtext('고장건수',side=2,col='#dd6633', line = 2.5,cex=2)
        legend('bottomright', legend=paste0(1:12,'월'), pch=20, text.col='white',cex=0.9,
               col=rainbow(12), bg="black")
}
##########################################
## 4분할 고장-사용량 그래프
{par(mfcol=c(2,2))
        par(bg='black')
        for (i in 1:4){
                with(m1,
                     plot(rental,broken,pch = 20,cex=1.5,,xlab="",ylab="",
                          col= ifelse(3*i-1)<=month(date) & month(date)<=3*i,rainbow(12)[month(date)],NA),))
axis(side=2,col='#dd6633',col.axis="#dd6633")
axis(side=1,col='#00cc00',col.axis='#00cc00')}
}
legend(650,60000, legend=paste0(1:12,'월'), pch=20, text.col='white',cex=0.9,col=rainbow(12), bg="black")
#################################
# 참고 : https://rstatistics.tistory.com/13
a=summary(with(m1[month(m1$date) %in% c(1,2,3),],lm(broken~rental)))
b=summary(with(m1[month(m1$date) %in% c(4,5,6),],lm(broken~rental)))
c=summary(with(m1[month(m1$date) %in% c(7,8,9),],lm(broken~rental)))
d=summary(with(m1[month(m1$date) %in% c(10,11,12),],lm(broken~rental)))
data.frame(season = c('1~3월','4~6월','7~9월','10~12월'),
           lm.coff = c(a$coefficients[2],b$coefficients[2],c$coefficients[2],d$coefficients[2]),
           p.lmcoff = c(a$coefficients[8],b$coefficients[8],c$coefficients[8],d$coefficients[8]),
           Intercept = c(a$coefficients[1],b$coefficients[1],c$coefficients[1],d$coefficients[1]),
           p.intercept = c(a$coefficients[7],b$coefficients[7],c$coefficients[7],d$coefficients[7])
                   )


a$coefficients

##############################################################################################
##############################################################################################

# 사용량과 고장의 관계 
# aggrigate : https://m.blog.naver.com/PostView.nhn?blogId=coder1252&logNo=221294821930&proxyReferer=https:%2F%2Fwww.google.com%2F
s = Brok_vbike %>% count(n)

# 고장 신고된 자전거 중 50% 이상은 4번 이하로 고장신고를 받았다. (4번 이하 : 17251대, 50.7%)
# 이 50%의 자전거는 전체 신고건수의 22%만 담당한다. 
sum(s[s$n<=4,2]) / sum(s$nn) 
sum(Brok_vbike[Brok_vbike$n<=4,2]) / sum(Brok_vbike$n) 



# 반대로 15회 이상 신고접수가 된 자전거는 총 953개로, 2.8%의 양이지만
# 전체 고장신고의 9.95%를 담당한다.
sum(s[s$n>=15,2])/ sum(s$nn) 
sum(Brok_vbike[Brok_vbike$n>=15,2]) / sum(Brok_vbike$n)


sum(s[4<=s$n & s$n<15,2])/ sum(s$nn) 
sum(Brok_vbike[4<=Brok_vbike$n & Brok_vbike$n<15,2]) / sum(Brok_vbike$n)

# 전체 자전거의 56% 이지만,
# 고장 건수의 75.6%를 담당하는 자전거를 기준으로 회귀함
condition = Brok_vbike[4<=Brok_vbike$n & Brok_vbike$n<15,]
condition

###############################################################
# 그냥 전체로
c(1,2) %in% c(1,3)

k = (unique(Brok_vbike$bike) %in% unique(Bike$bike) )
sum(!k)
sum(k)
