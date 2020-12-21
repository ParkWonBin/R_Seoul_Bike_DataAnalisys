library(dplyr) # %>% 함수 사용
library(ggplot2) # alpha 함수 사용
library(lubridate) # month 함수 사용

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
# 자주 사용하는 데이터셋 만들기
# 계절 분석용
Brok_vc = Broken %>% count(date)
Bike_vc = Bike %>% count(date)

################################################################
# 자전거 고장 분석
bike_id=function(id){
  as.factor(
    sapply(id,function(x){
      ifelse(x<=10000,'초기형',
             ifelse(x<=20000,'후기형',
                    ifelse(x<=27000,'개선형',
                           ifelse(x<=38000,'QR뉴따릉이','새싹따릉이'))))}))}

a = Broken %>% count(bike)# 자전거 ID별 고장 건수
b = Bike %>% count(bike)  # 자전거 ID별 대여 건수
c = with(Bike, aggregate(cbind(time,dist)~bike, data=Bike, sum)) #자전거ID별 사용 시간, 이동거리 총합

# 데이터셋 합칠 준비
names(a) = c('bike','broken') 
names(b) = c('bike','rental')  
names(c) = c('bike','time','dist')

# 필요한 데이터 모두 합치기
m2 = merge(merge(a,b),c) 
m2$b_type = bike_id(m2$bike)
m2$b_type = factor(m2$b_type, c('초기형','후기형','개선형','QR뉴따릉이','새싹따릉이'))
m2$factor =  as.factor(sapply(m2$broken,function(x){ifelse(x>13,13,x)}))
levels(m2$b_type)

# 고유번호 기준 사용량
{
par(mfcol=c(2,1),bg='white',mar=c(1.5, 1, 2.5, 1),oma=c(2, 1.5, 0.5,0))
b_type= c('초기형','후기형','개선형','QR뉴따릉이')
with(m2,plot(bike,rental,col=alpha(rainbow(4),0.2)[b_type],pch=20,))
legend('top', legend=b_type, pch=20, text.col='black',cex=0.8,
       col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
title('고유번호에 따란 대여건수 분포')
with(m2,plot(bike,broken,col=alpha(rainbow(4),0.2)[b_type],pch=20,))
legend('top', legend=b_type, pch=20, text.col='black',cex=0.8,
       col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
title('고유번호에 따란 고장건수 분포')
}

################################################################

head(m2)
windows.options(width=12, height=8)
{
  par(mfrow=c(2,2),bg='white',mar=c(1, 1, 2, 1),oma=c(2, 1.5, 1,0))
  
  plot(m2$time,log(m2$broken+exp(2)),col=alpha(rainbow(13),0.3)[m2$factor], pch=20)
  mtext('log(고장건수+e^2) ~ 사용시간 합계',side=3,line=1,cex=1.5)
  legend('top', legend=1:13, pch=20, text.col='black',cex=0.7,
         col=rainbow(13), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
  plot(log(m2$dist),log(m2$broken),col=alpha(rainbow(13),0.3)[m2$factor], pch=20,xlim=c(5,20))
  mtext('log(고장건수) ~ log(이동거리 합계)',side=3,line=1,cex=1.5)
  legend('top', legend=1:13, pch=20, text.col='black',cex=0.7,
         col=rainbow(13), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
  
  plot(m2$time,log(m2$broken+exp(2)),col=alpha(rainbow(4),0.3)[m2$b_type], pch=20)
  legend('top', legend=b_type, pch=20, text.col='black',cex=0.8,
         col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
  
  plot(log(m2$dist),log(m2$broken),col=alpha(rainbow(4),0.3)[m2$b_type], pch=20,xlim=c(5,20))
  legend('top', legend=b_type, pch=20, text.col='black',cex=0.8,
         col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
}

###########################
# 분석 시작
summary(m2$broken)
a = m2 %>% count(broken)
sum(a[a$broken>13,2])/sum(a$n)
# 13회 이상 고장난 자전거는 전체 신고건수에 5%가 된다. 
# 13회 이상 고장난 데이터는 같은 분류로 취급한다. 

m2$factor =  as.factor(sapply(m2$broken,function(x){ifelse(x>13,13,x)})  )
head(m2)
######################################################################

# par(mfcol=c(2,1),bg='white',mar=c(1, 1, 2, 1),oma=c(2, 1.5, 0.5,0))
# 시간과 이동거리가 고장에 끼치는 영향을 보기 위해 그래프를 그렸다.
# plot(m2$time,m2$broken,col=rainbow(13)[m2$factor])
# mtext('자전거별 고장건수 ~ 사용시간 합계',side=3,line=-1.5,cex=1.5)
# plot(m2$dist,m2$broken,col=rainbow(13)[m2$factor])
# mtext('자전거별 고장건수 ~ 이동거리 합계',side=3,line=-1.5,cex=1.5)

# 소수의 큰 값떄문에 그림의 전체 상이 잘 보이지 않아 log(x)값으로 그래프를 그렸다.
# plot(m2$time,log(m2$broken+exp(2)),col=rainbow(13)[m2$factor])
# plot(log(m2$dist),log(m2$broken),col=rainbow(13)[m2$factor])

# dist - broken은 같은 스케일, time과 log(broken)과 같은 스케일로 보면 편할 것 같다.
par(mfcol=c(2,1),bg='white',mar=c(1, 1, 4, 1),oma=c(2, 1.5, 1,0))
with(m2, boxplot(broken ~b_type,col=rainbow(4),ylim=c(0,20),xlab = "",ylab = ""))
title("따릉이 모델에 따른 고장건수",cex.main=2)
with(m2, boxplot(rental ~b_type,col=rainbow(4)))
title("따릉이 모델에 따른 대여건수",cex.main=2)

with(m2,summary(aov(rental ~b_type)))
with(m2,summary(aov(broken ~b_type)))


# 난잡한 3층 그래프
par(mfcol=c(3,1),bg='white',mar=c(1, 1, 2, 1),oma=c(2, 1.5, 1,0))
plot(m2$time,m2$dist,col=rainbow(13)[m2$factor])
plot(m2$time,log(m2$dist+exp(2)),col=rainbow(13)[m2$factor])
plot(m2$time,log(m2$dist+exp(2)),col=rainbow(4)[m2$b_type])



############
# pointer ##
########################################################


par(mfcol=c(2,1),bg='white',mar=c(1, 1, 4, 1),oma=c(2, 1.5, 1,0))
with(m2, boxplot(dist ~factor,col=rainbow(13),ylim=c(5,2*10^7))) 
mtext("n번 고장 신고가 접수된 자전거의 이동거리",side=3,cex=1.5,line=0.3)
with(m2, boxplot(time ~factor,col=rainbow(13)))
mtext("n번 고장 신고가 접수된 자전거의 사용시간",side=3,cex=1.5,line=0.3)

par(mfcol=c(2,1),bg='white',mar=c(1, 1, 4, 1),oma=c(2, 1.5, 1,0))
with(m2, boxplot(dist ~b_type,col=rainbow(13),ylim=c(5,1.5*10^7))) 
mtext("따릉이 모델별 평균 이동거리",side=3,cex=1.5,line=0.3)
with(m2, boxplot(time ~b_type,col=rainbow(13)))
mtext("따릉이 모델별 평균 사용시간",side=3,cex=1.5,line=0.3)


with(m2,summary(aov(time ~factor)))
with(m2,summary(aov(dist ~factor)))
with(m2,summary(aov(time ~b_type)))
with(m2,summary(aov(dist ~b_type)))




# plot(log(m2$time+exp(10)),log(m2$dist+exp(4)),col=rainbow(4)[m2$b_type])

# 사고난 횟수와 자전거 총 이동거리 
with(m2, boxplot(dist ~factor,col=rainbow(13),ylim=c(5,2*10^7))) 
with(m2, boxplot(time ~factor,col=rainbow(13)))
with(m2, boxplot(broken ~b_type,col=rainbow(13),ylim=c(0,20)))

summary(m2$broken)


d = with(Bike, aggregate(cbind(time,dist)~bike, data=Bike, mean)) 
d

m2
# lab =  c("3회 이하", "7회 이하", "13회 이하", "14이상")
# m2$factor =  as.factor(
#   sapply(m2$broken,function(x){
    # ifelse(x<=3,lab[1],
         # ifelse(x<=7,lab[2],
                # ifelse(x<14,lab[3],lab[4])))
  # }))


# paire plot : http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs
# colors = alpha(rainbow(4)[m2$factor],0.5)
# pairs(m2[,2:5],col=alpha(colors[m2$factor],0.5))

# sum(m2$factor == lab[1]) # 8262
# sum(m2$factor == lab[2]) # 10989
# sum(m2$factor == lab[3]) # 6458
# sum(m2$factor == lab[4]) # 1875

# with(m2[m2$factor ==lab[1],],plot(time,dist, col=alpha(rainbow(4)[factor],0.5)))
# with(m2[m2$factor ==lab[2],],plot(time,dist, col=alpha(rainbow(4)[factor],0.5)))
# with(m2[m2$factor ==lab[3],],plot(time,dist, col=alpha(rainbow(4)[factor],0.5)))
# with(m2[m2$factor ==lab[4],],plot(time,dist))

# summary(m2$dist)

# par(mfcol=c(2,2))
# for (i in 1:4){
#   with(m2[m2$factor == lab[i],],plot(time,dist))
# }
# par(mfcol=c(1,1))