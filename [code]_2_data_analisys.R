# 회귀분석 해석 
# https://dbrang.tistory.com/1102
# 되게 자세한 회귀분석 글*** : https://rstudio-pubs-static.s3.amazonaws.com/190997_40fa09db8e344b19b14a687ea5de914b.html
# 분산분석 : https://nittaku.tistory.com/460
# 분산분석 자세한 설명***  : https://rfriend.tistory.com/131
library(dplyr)
library(lubridate)
library(ggplot2)

# 데이터 읽기
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Bike = read.csv('../Bike-Use-final.csv')
Broken = read.csv('[csv]_Bike-Broken.csv',fileEncoding = 'utf-8')

# 데이터 카운트
Brok_vc = Broken %>% count(date) # 날자당 고장 건수 
Bike_vc = Bike %>% count(date)  # 날자당 대여 건수

# 데이터 병합, 
names(Brok_vc) = c('date','broken')
names(Bike_vc) = c('date','rental')
m1 = merge(Brok_vc,Bike_vc,by='date')
m1$month = as.factor(month(m1$date))

# mfcol 관련*** : https://rfriend.tistory.com/151
# legend : http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way

par(mfcol=c(1,2),
    mar=c(2, 1, 2, 1), # inner margin
    oma=c(2, 1.5, 0.5, 0.5)) # outer margin
with(m1, boxplot(broken ~month,col=rainbow(12)));title('고장 건수')
with(m1, boxplot(rental ~month,col=rainbow(12)));title('대여 건수')
summary(with(m1,aov(broken~month)))

{
par(mfcol=c(1,1),xaxt = "n",mar=c(2, 1, 2, 1),oma=c(0.5, 1.5, 0.5, 0.5))
with(m1, boxplot((broken+52)/rental ~month,col=rainbow(12),ylim=c(0.001,0.028)))
legend('bottom', legend=paste0(1:12,'월'), pch=20, text.col='black',
       cex=.8, horiz=TRUE,col=rainbow(12), bg=alpha('white',0.2), box.lty=0)
title('계절성에 의한 기울기 계수 변화')
mtext('대여건수 1개당 (고장건수+52)의 값',side=3,line=-1.5,cex=1.2)
mtext('anova 분석 결과 : F= 2.266, P=0.00979',side=1,cex=1.5,line=0.5)
par(mfcol=c(1,1),xaxt = "t")
# summary(with(m1,aov((broken+52)/rental~month)))
}


{ # 4분할 이미지
par(mfcol=c(2,2),bg='black',mar=c(2, 1, 2, 1),oma=c(0.5, 1.5, 0.5, 0.5))
for (i in 1:4){
  with(m1,
       plot(rental,broken,pch = 20,cex=1.5,,xlab="",ylab="",
            col= ifelse((3*i-1)<=month(date) & month(date)<=3*i,rainbow(12)[month],NA)
            )
       )
  axis(side=2,col='#dd6633',col.axis="#dd6633")
  axis(side=1,col='#00cc00',col.axis='#00cc00')
  }
}

#####################################################
a=summary(with(m1[m1$month %in% c(1,2,3),],lm(broken~rental)))
b=summary(with(m1[m1$month %in% c(4,5,6),],lm(broken~rental)))
c=summary(with(m1[m1$month %in% c(7,8,9),],lm(broken~rental)))
d=summary(with(m1[m1$month %in% c(10,11,12),],lm(broken~rental)))
e=summary(with(m1,lm(broken~rental)))


data.frame(season = c('1~3월','4~6월','7~9월','10~12월','전체기간'),
           lm.coff = c(a$coefficients[2],b$coefficients[2],c$coefficients[2],d$coefficients[2],e$coefficients[2]),
           Intercept = c(a$coefficients[1],b$coefficients[1],c$coefficients[1],d$coefficients[1],e$coefficients[1]),
           p.lmcoff = c(a$coefficients[8],b$coefficients[8],c$coefficients[8],d$coefficients[8],e$coefficients[8]),
           p.intercept = c(a$coefficients[7],b$coefficients[7],c$coefficients[7],d$coefficients[7],e$coefficients[7]),
           Std.error = c(a$coefficients[3],b$coefficients[3],c$coefficients[3],d$coefficients[3],e$coefficients[3]))


m1
e$fstatistic
e$coefficients
anova(a,b)
