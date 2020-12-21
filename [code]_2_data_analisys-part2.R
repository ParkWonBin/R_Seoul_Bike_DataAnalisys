library(dplyr) # %>% 함수 사용
library(lubridate) # month 함수 사용
library(ggplot2) # alpha 함수 사용

{ # 초기데이터 세팅
# 데이터 열기  
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Bike = read.csv('../Bike-Use-final.csv')
Broken = read.csv('[csv]_Bike-Broken.csv',fileEncoding = 'utf-8')
# 데이터 초기 설정
Broken$date = as.Date(Broken$date, origin = "2015-09-18") 
Bike$date = as.Date(Bike$date, origin = "2015-09-18") 
Broken$type = as.factor(Broken$type)
}
############################################
{ # 자주 사용하는 데이터셋 만들기
# 계절 분석용
Brok_vc = Broken %>% count(date)
Bike_vc = Bike %>% count(date)

# 자전거 분석용
a = Broken %>% count(bike) # 고장 횟수
b = Bike %>% count(bike) # 대여 횟수
c = with(Bike, aggregate(cbind(time,dist)~bike,data=Bike,sum)) 

# 데이터셋 합칠 준비
names(a) = c('bike','broken')  # 자전거 ID별 고장 횟수
names(b) = c('bike','rental')  # 자전거 ID별 사용 횟수
names(c) = c('bike','time','dist')#사용 시간, 이동거리 총합
m2 = merge(merge(a,b),c) ; head(m2)
}

#################
# 분석 시작
summary(m2$broken)
a = m2 %>% count(broken)

# 13회 이상 고장난 자전거는 전체 신고건수에 5%가 된다. 
sum(a[a$broken>13,2])/sum(a$n)

# 13회 이상 고장난 데이터는 같은 분류로 취급한다. 
m2$factor =  as.factor(
  sapply(m2$broken,function(x){ifelse(x>13,13,x)})
  )

log(exp(2))

head(m2)

# 시간과 이동거리가 고장에 끼치는 영향을 보기 위해 그래프를 그렸다.
plot(m2$dist,m2$broken,col=rainbow(13)[m2$factor])
plot(m2$time,m2$broken,col=rainbow(13)[m2$factor])

# 소수의 큰 값떄문에 그림의 전체 상이 잘 보이지 않아 log(x)값으로 그래프를 그렸다.
plot(m2$time,log(m2$broken+exp(2)),col=rainbow(13)[m2$factor])
plot(log(m2$dist),log(m2$broken),col=rainbow(13)[m2$factor])

# dist - broken은 같은 스케일, time과 log(broken)과 같은 스케일로 보면 편할 것 같다.
plot(m2$time,log(m2$dist+exp(2)),col=rainbow(13)[m2$factor])

plot(log(m2$time+exp(9)),log(m2$dist+exp(4)),col=rainbow(13)[m2$factor])
plot(log(m2$time+exp(10)),log(m2$dist+exp(10)),col=rainbow(13)[m2$factor])
plot(log(m2$time+exp(10)),log(m2$dist+exp(10)),col=rainbow(13)[m2$factor])

# 사고난 횟수와 자전거 총 이동거리 
with(m2, boxplot(dist ~factor,col=rainbow(13),ylim=c(5,2*10^7))) 
with(m2, boxplot(time ~factor,col=rainbow(13)))

summary(m2$broken)



lab =  c("3회 이하", "7회 이하", "13회 이하", "14이상")
m2$factor =  as.factor(
  sapply(m2$broken,function(x){
    ifelse(x<=3,lab[1],
         ifelse(x<=7,lab[2],
                ifelse(x<14,lab[3],lab[4])))
  }))


# paire plot : http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
colors = alpha(rainbow(4)[m2$factor],0.5)
pairs(m2[,2:5],col=alpha(colors[m2$factor],0.5))

sum(m2$factor == lab[1]) # 8262
sum(m2$factor == lab[2]) # 10989
sum(m2$factor == lab[3]) # 6458
sum(m2$factor == lab[4]) # 1875

with(m2[m2$factor ==lab[1],],plot(time,dist, col=alpha(rainbow(4)[factor],0.5)))
with(m2[m2$factor ==lab[2],],plot(time,dist, col=alpha(rainbow(4)[factor],0.5)))
with(m2[m2$factor ==lab[3],],plot(time,dist, col=alpha(rainbow(4)[factor],0.5)))
with(m2[m2$factor ==lab[4],],plot(time,dist))

summary(m2$dist)

par(mfcol=c(2,2))
for (i in 1:4){
  with(m2[m2$factor == lab[i],],plot(time,dist))
}
par(mfcol=c(1,1))

head(m2)