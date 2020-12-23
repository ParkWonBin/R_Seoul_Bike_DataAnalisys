# 따릉이 데이터 분석
중앙대학교 물리학과 14박원빈

### 1. 소개 
서울시에서 2015년 9월부터 공공자전거 사업(따릉이)을 시작했다. 지난 5년간 따릉이를 사용하면서 수리가 필요한 자전거를 많이 봤다. 따릉이 사업 소개에 따르면 잔고장이 안 나는 내구성이 강한 설계로 제작된 모델이라고 한다. 과연 얼마나 튼튼한지 확인해보고 싶어 연구를 시작했다. 이 연구는 서울시 공공데이터에 올라온 따릉이 고장 신고 내역과 자전거 대여 내역과 데이터를 사용한다. 이 연구는 1. 계절에 따른 자전거 사용과 고장 신고 수의수 의 관계와 2. 고장접수가 많이 된 자전거(고유번호)의 누적사용량(사용 시간, 이동거리)을 시각화하고 회귀 식을 도출한다.

### 데이터 전처리
서울시 공공데이터 자료실에 게시된 raw-data에 사소한 문제가 있었다. 2020.1월 ~ 2020.5월 파일에 한글 인코딩은 중간중간 깨져 데이터 열 구분이 틀어져있는 경우가 종종 있다. 그리고 대여소 ID를 기록하는 열은 정수형 데이터가 예상되지만 가끔씩 대여 소명 (와트컴, 중량센터, 상암센터 등)이 들어가 있기도 하다. 해당 이슈를 보정하는 작업은 이 문서 마지막에 [부록]에 첨부했다. 

 - [[ 부록 ] 데이터 전처리](#부록-데이터-전처리)
 - [공공데이터 : 대여소 목록](https://data.seoul.go.kr/dataList/OA-13252/F/1/datasetView.do)  
 - [공공데이터 : 따릉이 고장](https://data.seoul.go.kr/dataList/OA-15644/F/1/datasetView.do)  
 - [공공데이터 : 따릉이 사용내역](https://data.seoul.go.kr/dataList/OA-15182/F/1/datasetView.do)  

공공데이터를 모두 다운받으면 5.3GB 정도가 된다. 하지만 데이터 내에 중복되는 데이터와 문자열이 많다. 문자열로 자전거 ID를 정수형으로 바꾸고, 문자열로 기록되 날짜 데이터를 정수형 데이터로 변환(게시일 2015.9.18로부터 +n일)하여 다시 저장했다. 필요한 정보만 남기니 5.3GB(파일 52개)가 1.2GB csv파일 하나로 정리됐다. csv를 zip파일로 압축하니 358.5MB로 압축이 되었다. 이 문서는 전처리가 완료된 이후 코드부터 다룬다.

### 결론
1. 자전거 사용양 고장 건수 사이에 선형 관계를 확인했다. (ch2.4 표)
2. 사용양과 고장 사이의 선형계수(기울기)가 계절성의 영향을 받는 것을 확인했다. (ch2.4 그림)
3. 잔고장이 많은 자전거는 사용시간 보다는 이동거리의 영향을 더 많이 받는다. (ch3.2의 그림)
4. 고장이 많이 나는 자전거일 수록 총 사용시간, 총 이동거리가 많다. (ch3.3 그림1)
5. 자전거의 평균 사용시간, 평균 이동거리는 고장난 횟수의 영향을 거의 받지 않고 비슷비슷했다. (ch3.3 그림2)


# 2. 데이터 둘러보기

```r
# 사용하는 패키지
library(dplyr)     # %>% 함수 
library(ggplot2)   # alpha 함수 
library(lubridate) # month 함수

# 데이터 열기  
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Bike = read.csv('../Bike-Use-final.csv')
Broken = read.csv('[csv]_Bike-Broken.csv',fileEncoding = 'utf-8')

# 데이터 초기 설정
Broken$date = as.Date(Broken$date, origin = "2015-09-18") 
Bike$date = as.Date(Bike$date, origin = "2015-09-18") 
Broken$type = as.factor(Broken$type)

# 날짜별 대여건수와 고장 건수 계산
Bike_vc = Bike %>% count(date)
Brok_vc = Broken %>% count(date)
names(Brok_vc) = c('date','broken')
names(Bike_vc) = c('date','rental')

# 계절성 분석 및 회귀에서 계속 쓰일 변수
m1 = merge(Brok_vc,Bike_vc,by='date')
m1$month = as.factor(month(m1$date))
```

### 2.1. 잘 고장나는 부품

가장 고장이 많은 부품은 **1.단말기, 2.체인 3.기타 4.타이어** 순이다. 2020년 3월 1일 이후에 도입한 뉴 따릉이(QR코드형)은 자전거에서 단말기를 제거되었다. 앞으로는 단말기로 인한 잔고장은 ~~물리적으로~~ 사라질 예정이다.  

```r
data.frame(t(sort(summary(Broken$type),decreasing = TRUE )))
```
![](\README_img\_2.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/_2.png)

### 2.2. 고장나기 좋은 계절
자전거 사용은 계절성을 나타낸다. 자전거 사용의 계절성과 자전거 고장의 계절성에 대해서 시각화 하려고 한다. 직관적으로 사람들이 자전거를 많이 사용하는 때 고장 신고도 많이 발생한다는 것을 생각할 수 있다. 이 직관이  통계적으로 얼마나 믿을만한지 분석해보려고 한다. 

```r
plot(Bike_vc,col=alpha('#00dd00',0.2), pch = 20,cex=1.5,
     ylim=c(1,max2),xlim=c(xmin,xmax),
     axes = FALSE,xlab="",ylab="")
axis(side=1, at=xaxis,labels = xlabel,cex.axis=0.8)
axis(side=2,col="#00aa00",col.axis='#00aa00',cex.axis=0.8)
mtext('대여건수',side=2,col='#00cc00',line = -1.2,cex=1.2)

par(new = TRUE) # 겹치기
plot(Brok_vc, col=alpha('#ff0000',0.2), pch = 20,cex=1.3,
     ylim=c(1,max1),xlim=c(xmin,xmax), 
     axes = FALSE,xlab="",ylab="")
axis(side=4, col='#dd6633',col.axis="#dd6633")
mtext('고장건수',side=4,col='#dd6633', line = -1.2,cex=1.2)
title("자전거 사용량과 고장건수", cex.main=2)
```
![](\README_img\3.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/3.png)

```r
par(mfcol=c(2,1),xaxt = "t", mar=c(2, 1, 2, 1),oma=c(0.5, 1.5, 0.5, 0.5))
boxplot(Bike_vc$n ~ Bike_vc$month , col=rainbow(12))
title('월별 총 대여수',cex.main=2)
boxplot(Brok_vc$n ~ Brok_vc$month , col=rainbow(12),ylim=c(0,500))
title('월별 총 고장수',cex.main=2)
```
![](\README_img\3_1.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/3_1.png)


따릉이 자전거 사용량(일일 대여건수)과 고장 신고 건수는 명확히 일치하는 계절성을 보인다.
그리고 고장 건수와 대여 건수의 축을 교차해보니 '고장 건수'와 '대여 건수' 사이에 선형적인 관계가 있을 것 같은 느낌이 든다.

1. 고장 건수와 대여 건수 사이의 선형회귀하면 설명력은 어느 정도가 될까?
2. 위에서 구한 회귀식이 계절성의 영향을 받을까?


### 2.3. 선형성 시각화
변수 간의 선형-상관성을 시각적으로 확인하기 위해 x, y축을 고장 건수와 대여 건수로 나타냈다.
데이터가 계절성을 갖고 있기 때문에, 계절성에 대한 직관을 잃지 않고자 월별로 data의 색깔을 다르게 칠해봤다.
아래 그림을 살펴보면, 고장 건수와 대여 건수 사이의 기울기가 계절마다 다른 느낌이 든다.

```r
par(bg='black')
with(m1,plot(rental,broken,pch = 20,cex=1.5,,xlab="",ylab="",ylim=c(0,950),
     col=rainbow(12,alpha = 0.7)[month(date)]))
axis(side=1,col='#00cc00',col.axis='#00cc00')
axis(side=2,col='#dd6633',col.axis="#dd6633")
mtext('대여건수',side=1,col='#00cc00', line = 2.5,cex=2)
mtext('고장건수',side=2,col='#dd6633', line = 2.5,cex=2)
legend('top', legend=paste0(1:12,'월'), pch=20, text.col='white',cex=1,
       col=rainbow(12), bg=alpha("white",0.2), horiz=TRUE,)
```
![](\README_img\4.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/4.png)

3월(노란색) 데이터는 기울기가 가파르고 6월(초록색) 데이터는 기울기가 원만하다.  
그림 하나에 데이터 전부 넣으니 분류(월)별 데이터의 경향성을 파악하기 쉽지 않다. 
깔끔하게 분기별로 3달씩 그래프를 다시 그렸다. 

```r
par(mfcol=c(2,2),bg='black',mar=c(2, 1, 2, 1),oma=c(0.5, 1.5, 0.5,0))
for (i in 1:4){
  with(m1,plot(rental, broken, pch = 20,cex=1.5,,xlab="",ylab="",
        col= ifelse((3*i-1)<=month(date) & month(date)<=3*i,rainbow(12)[month],NA)))
  axis(side=2,col='#dd6633',col.axis="#dd6633")
  axis(side=1,col='#00cc00',col.axis='#00cc00')}
```
![](\README_img\5.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/5.png)

뭔가 있어 보이는 그림이 그려져서 다행이다. 안심하고 과제를 계속 진행해도 될 것 같다.
2015~2020, 5년간의 데이터가 모인 자료인데 년 도가 달라도 월별로 비슷한 경향성을 보이는 것이 신기하다. 그림이 너무 잘 뽑혀서 2015.9.18부터 날짜별로 점이 하나씩 추가하는 그래프 애니메이션을 만들면 볼만하겠다는 생각이 든다. 하지만 과제 마감이 멀지 않아 사치 부릴 생각은 접어둔다. 회귀식이나 통계와 관련된 숫자만 구하고 다음으로 넘어갈 예정이다.



### 2.4 계절성에 의한 회귀식 구하기
분기별로 회귀식을 돌려봤다. 데이터가 많아서 그런지 p-value가 매우 작다. 역시 GB 단위의 CSV는 믿고 쓸 수 있는 것 같다. 아래 회귀식을 통해 '자전거 대여가 많은 달에 자전거 고장 신고도 많이 들어온다.'라는 당연한 사실을 확인할 수 있다. 생각보다 일이 잘 풀리는 기분이라 작업하는 보람이 있다.

#### \[고장건수 = lm.coff*대여건수 + Intercept + error\]

```r
# library(lubridate)
a=summary(with(m1[m1$month %in% c(1,2,3),],lm(broken~rental)))$coefficients
b=summary(with(m1[m1$month %in% c(4,5,6),],lm(broken~rental)))$coefficients
c=summary(with(m1[m1$month %in% c(7,8,9),],lm(broken~rental)))$coefficients
d=summary(with(m1[m1$month %in% c(10,11,12),],lm(broken~rental)))$coefficients
e=summary(with(m1,lm(broken~rental)))$coefficients

data.frame(season = c('1~3월','4~6월','7~9월','10~12월','전체기간'),
           lm.coff    = c(a[2],b[2],c[2],d[2],e[2]),
           Intercept  = c(a[1],b[1],c[1],d[1],e[1]),
           p.lmcoff   = c(a[8],b[8],c[8],d[8],e[8]),
           p.intercept= c(a[7],b[7],c[7],d[7],e[7]),
           Std.error  = c(a[3],b[3],c[3],d[3],e[3]))
```
![](\README_img\6.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/6.png)


계절별로 유도된 선형 계수(lm.coff)의 크기가 다르다. 이 계수가 계절성의 영향을 받아 변화하는 것이 맞는지 분산분석을 통해 확인해본다. 확인 방법은 위에서 얻은 회귀식을 변형하여 데이터마다 (lm.coff)값에 대응하는 수치를 계산하여 월별로 분포를 확인한다.

#### \[ lm.coff = (고장건수 -Intercept)/대여건수  \]
```r
par(mfcol=c(1,1),xaxt = "n",mar=c(2, 1, 2, 1),oma=c(0.5, 1.5, 0.5, 0.5))
with(m1, boxplot((broken+52)/rental ~month,col=rainbow(12),ylim=c(0.001,0.028)))
legend('bottom', legend=paste0(1:12,'월'), pch=20, text.col='black',
       cex=1.4, horiz=TRUE,col=rainbow(12), bg=alpha('white',0.2), box.lty=0)
title('계절성에 의한 기울기 계수 변화',cex.main=2)
mtext('대여건수 1개당 (고장건수+52)의 값',side=3,line=-1.5,cex=1.5)
mtext('anova 분석 결과 : F= 2.266, P=0.00979',side=1,cex=1.5,line=0.5)
# par(mfcol=c(1,1),xaxt = "t")
# summary(with(m1,aov((broken+52)/rental~month)))
```
![](\README_img\7.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/7.png)

```r
summary(with(m1,aov((broken+52)/rental~month))) # F = 2.266 , P = 0.00979 **
```

p-value가 작게 나와서 마음이 편하다. 회귀 식은 7월~9월 회귀 식을 기준으로, y 절편(약-52)만큼 평행이동 후 데이터마다 기울기를 계산한 것이다. 분산분석 결과 99% 유의수준에서 '기울기 데이터는 월별로 차이가 없다.'라는 귀무가설을 기각할 수 있다.

여담으로 회귀식의 y 절편을 크게 잡을수록 (원점에서 멀리 이동할수록) p 값이 낮게 나온다. 가령 원점을 y축으로 10^7만큼 이동하여 기울기를 계산하면 p 값으로 0.0001을 얻을 수 있다. 이는 데이터의 x축(대여 건수)이 계절성의 영향을 많이 받기 때문이다.

결과적으로 선형계수는 계절성의 영향을 받고, 그 주기는 6개월 정도 되어 보인다. 푸리에 변환으로 위의 그래프를 잘 설명하는 근사식 a(t) = [a*exp(iwt)+b]를 상정하면 '계절성의 영향을 보정한' 회귀 식을 구할 수 있을 것이다.

####  y = a(t)x + Intersect
####   y = [a*exp(iwt)+b] x + Intersect

### 3. 고장 난 자전거(고유번호) 분석
따릉이는 자전거의 모델(종류)마다 고유번호가 다르다. 자전거 고유번호를 기준으로 자전거 모델별로 고장률에 미치는 영향을 확인하고, 자전거의 고유번호(ID)를 key 값으로 잔고장이 많은 자전거의 데이터를 분석해보려고 한다. 고장 건수, 대여 건수, 대여 시간, 이동 거리 등을 자전거 고유 번호를 기준으로 합계하여 데이터를 준비한다.


```r
# 고장 분석에 사용할 데이터 만들기
bike_id=function(id){
  as.factor(
    sapply(id,function(x){
      ifelse(x<=10000,'초기형',
             ifelse(x<=20000,'후기형',
                    ifelse(x<=27000,'개선형',
                           ifelse(x<=38000,'QR뉴따릉이'))))}))}

a = Broken %>% count(bike)# 자전거 ID별 고장 건수
b = Bike %>% count(bike)  # 자전거 ID별 대여 건수
c = with(Bike,#자전거ID별 사용 시간, 이동거리 총합
         aggregate(cbind(time,dist)~bike, data=Bike, sum)) 


# 데이터합치기
names(a) = c('bike','broken') 
names(b) = c('bike','rental')  
names(c) = c('bike','time','dist')
m2 = merge(merge(a,b),c) 

# 자전거 고유번호 구분
m2$b_type = bike_id(m2$bike)
m2$b_type = factor(m2$b_type, c('초기형','후기형','개선형','QR뉴따릉이'))
m2$factor =  as.factor(sapply(m2$broken,function(x){ifelse(x>13,13,x)}))

```
### 3.1. 자전거의 사용량과 고장 신고 건수
자전거의 고유번호를 x축으로 총 대여 건수와 고장 건수의 그래프를 그렸다.
모델마다 시행 일자가 달라 사용량에서는 평균의 차이는 있어보인다. box-plot과 ANOVA 분석을 한다.

```r
par(mfcol=c(2,2),bg='white',mar=c(1.5, 1, 2.5, 1),oma=c(2, 1.5, 0.5,0))
b_type= c('초기형','후기형','개선형','QR뉴따릉이')
with(m2,plot(bike,rental,col=alpha(rainbow(4),0.2)[b_type],pch=20,))
legend('top', legend=b_type, pch=20, text.col='black',cex=1.5,
       col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
title('고유번호에 따란 대여건수 분포',cex.main=1.5)
with(m2,plot(bike,broken,col=alpha(rainbow(4),0.2)[b_type],pch=20,))
legend('top', legend=b_type, pch=20, text.col='black',cex=1.5,
       col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
title('고유번호에 따란 고장건수 분포',cex.main=1.5)

with(m2, boxplot(rental ~b_type,col=rainbow(4)))
title("따릉이 모델에 따른 대여건수",cex.main=2)
with(m2, boxplot(broken ~b_type,col=rainbow(4),ylim=c(0,20),xlab = "",ylab = ""))
title("따릉이 모델에 따른 고장건수",cex.main=2)
```
![](\README_img\8.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/8.png)

따릉이 모델에 따른 사용량과 사용량과 고장 수는 관계가 있어보인다. 분산분석 결과 매우 높은 유의수준 (1 - 2e-16)으로 집단간 평균의 차이가 있다는 것을 알 수 있다. 데이터가 많아 전반적으로 모든 분석에 유의수준이 높게 나오는 것 같다. 

```r
# 따릉이 모델구룹간의 
with(m2,summary(aov(broken ~b_type))) # 고장건수 : F = 677.4 , P <= 2e-16 ***
with(m2,summary(aov(rental ~b_type))) # 대여건수 : F = 7024  , P <= 2e-16 ***
```

### 3.2. 고장 횟수와 사용량(사용시간,이동거리)
자전거 총 이용시간과 이동거리가 고장신고 수에 미치는 영향을 알아보기 위해 그래프를 그렸다. 데이터마다 사용시간과 이동거리 값의 분산이 매우 큰데, 이는 자전거마다 운행 기간이 다르고, 주로 배치된 지역의 자전거 사용률도 다르기 때문이다. 고장 건수 데이터는 평균에서 멀리 떨어진 값이 많아 log-scale로 시각화를 했다.

```r
par(mfrow=c(2,2),bg='white',mar=c(1, 1, 2, 1),oma=c(2, 1.5, 1,0))

plot(m2$time,log(m2$broken+exp(2)),
    col=alpha(rainbow(13),0.3)[m2$factor], pch=20)
mtext('log(고장건수+e^2) ~ 사용시간 합계',side=3,line=1,cex=1.5)
legend('top', legend=1:13, pch=20, text.col='black',cex=1,
        col=rainbow(13), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)

plot(log(m2$dist),log(m2$broken),
    col=alpha(rainbow(13),0.3)[m2$factor], pch=20,xlim=c(5,20))
mtext('log(고장건수) ~ log(이동거리 합계)',side=3,line=1,cex=1.5)
legend('top', legend=1:13, pch=20, text.col='black',cex=1,
        col=rainbow(13), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)

plot(m2$time,log(m2$broken+exp(2)),
    col=alpha(rainbow(4),0.3)[m2$b_type], pch=20)
legend('top', legend=b_type, pch=20, text.col='black',cex=1.2,
        col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)

plot(log(m2$dist),log(m2$broken),
    col=alpha(rainbow(4),0.3)[m2$b_type], pch=20,xlim=c(5,20))
legend('top', legend=b_type, pch=20, text.col='black',cex=1.2,
        col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
```
![](\README_img\9.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/9.png)

1행의 데이터의 색깔은 고장 건수를 의미한다. 다음에 시각화 한 자료에서 위의 색깔을 사용할 것이기 때문에 대략적인 분포와 색깔을 표시했다.

2행은 따릉이 모델을 기준으로 색깔을 나는 것이다. 모델 간 운영 기간의 차이 때문에 사용 시간(x축)에 있어서 모델별 차이가 크다. 1행 2열을 보면 총 이동 거리가 3,269km(약 e^14) 전후인 자전거에서 잔고장이 많았다. 선형으로 비례하는 것은 아니지만, 이동 거리가 많은 자전거는 잔고장이 많아질 수 있다는 것을 알 수 있다.

1행의 그래프에서 데이터의 분포가 제대로 보이지 않아, 이를 log-scale 하여 2, 3행에 나타냈다. 2행은 고장 횟수로 색칠한 자료고, 3행은 자전거 모델(종류)에 따라 색칠했다.


```r
par(mfcol=c(3,1),bg='white',mar=c(1, 1, 2, 1),oma=c(2, 1.5, 2,0))
plot(m2$time,m2$dist,col=rainbow(13)[m2$factor])
mtext("자전거별 이동거리~사용시간, 기울기:속도",side=3,cex=1.8,line=0.5)
legend('top', legend=paste0(1:13,'회 고장'), pch=20, text.col='black',cex=1.3,
         col=rainbow(13), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
plot(m2$time,log(m2$dist+exp(2)),col=rainbow(13)[m2$factor])
plot(m2$time,log(m2$dist+exp(2)),col=rainbow(4)[m2$b_type])
legend('bottomright', legend=b_type, pch=20, text.col='black',cex=1.5,
         col=rainbow(4), bg=alpha("white",0.2), horiz=TRUE,box.lty=0)
```
![](\README_img\10.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/10.png)

- x 축 : 자전거별 총 이동시간
- y 축 : log(총 이동 거리 + e^2)

위 그림 2행의 데이터를 보면 원점과 가까울수록 붉은 계열, 원점과 멀수록 푸른 계열을 띄는 것을 알 수 있다. 이는 자전거의 사용 시간과 이동 거리가 클수록 고장이 잘 난다는 것을 의미한다. box plot과 ANOVA 검사를 사용하여 분석해본다.

### 3.3 분산분석 
분산분석을 돌리기 전에 '자전거 별 운영기간'에 의한 차이를 고려할지에 대해 생각을 해봤다. 잠깐 고민을 해보니 두 상황에 대해서 각각 분석을 해보려고 한다. 

우선 사용기간의 영향을 무시하지 않고 데이터를 살펴본다. 아래 그림은 자전거의 고유번호(ID)를 key값을 이동거리와 사용시간을 모두 '합계'한 자료다.
```r
par(mfcol=c(2,2),bg='white',mar=c(1, 1, 4, 1),oma=c(2, 1.5, 1,0))
with(m2, boxplot(dist ~factor,col=rainbow(13),ylim=c(5,2*10^7))) 
mtext("n번 고장 신고가 접수된 자전거의 이동거리",side=3,cex=1.5,line=0.3)
with(m2, boxplot(time ~factor,col=rainbow(13)))
mtext("n번 고장 신고가 접수된 자전거의 사용시간",side=3,cex=1.5,line=0.3)

with(m2, boxplot(dist ~b_type,col=rainbow(4),ylim=c(5,1.5*10^7))) 
mtext("따릉이 모델별 평균 이동거리",side=3,cex=1.5,line=0.3)
with(m2, boxplot(time ~b_type,col=rainbow(4)))
mtext("따릉이 모델별 평균 사용시간",side=3,cex=1.5,line=0.3)
```
![](\README_img\11.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/11.png)
```r
# 고장 횟수에 따른 
with(m2,summary(aov(time ~factor))) # 사용시간 : F = 918.9 , P <= 2e-16 ***
with(m2,summary(aov(dist ~factor))) # 이동거리 : F = 355.0 , P <= 2e-16 ***

# 자전거 모델에 따른
with(m2,summary(aov(time ~b_type))) # 사용시간 : F = 5244 , P <= 2e-16 ***
with(m2,summary(aov(dist ~b_type))) # 이동거리 : F = 3546 , P <= 2e-16 ***
```
많이 고장 나는 자전거는 평균적으로 사용 시간과 이동 거리가 큰 '오래된 자전거'라는 것을 알 수 있다.

이번에는 운영 기간에 의한 영향을 배제하고 살펴보려고 한다. 이동 거리와 사용 시간을 대여 건수에 대해 평균을 내면, 운영 기간에 의한 영향이 보정되리라 생각한다.

```r
par(mfcol=c(2,2),bg='white',mar=c(1, 1, 4, 1),oma=c(2, 1.5, 1,0))
with(m2, boxplot(dist/rental ~factor,col=rainbow(13),ylim=c(0,8000))) 
mtext("n번 고장 신고가 접수된 자전거 대여건당 평균 이동거리",side=3,cex=1.5,line=0.3)
with(m2, boxplot(time/rental ~factor,col=rainbow(13),ylim=c(15,45)))
mtext("n번 고장 신고가 접수된 자전거 대여건당 평균 사용시간",side=3,cex=1.5,line=0.3)

with(m2, boxplot(dist/rental ~b_type,col=rainbow(4),ylim=c(5,6000))) 
mtext("따릉이 모델별 대여 건당 평균 이동거리",side=3,cex=1.5,line=0.3)
with(m2, boxplot(time/rental ~b_type,col=rainbow(4),ylim=c(20,40)))
mtext("따릉이 모델별 대여 건당 평균 사용시간",side=3,cex=1.5,line=0.3)
```
![](\README_img\12.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/12.png)

```r
# 고장 횟수에 따른 (대여 건당 평균)
with(m2,summary(aov(time/rental ~factor))) # 사용시간 : F = 146   , P <= 2e-16 ***
with(m2,summary(aov(dist/rental ~factor))) # 이동거리 : F = 57.53 , P <= 2e-16 ***

# 자전거 모델에 따른(대여 건당 평균)
with(m2,summary(aov(time/rental ~b_type))) # 사용시간 : F = 9578  , P <= 2e-16 ***
with(m2,summary(aov(dist/rental ~b_type))) # 이동거리 : F = 2643  , P <= 2e-16 ***
```

QR 뉴 따릉이 데이터의 이동 거리 데이터가 유실되어 있음을 그것을 알게 됐다. 1행 2열의 표를 보면 다수의 뉴 따릉이의 이동 거리가 0으로 잡혀있다. 뉴 따릉이의 모델은 GPS 센서가 없나보다. 출발 대여소와 도착 대여소의 위도와 경도, 그리고 다른 자전거의 데이터를 사용하여 결측값을 채우는 모델을 만들어 데이터를 보정할 수 있을 것 같은데, 시간관계상 생략한다.

뉴 따릉이의 데이터 이상치의 영향으로 1행 1열 "1, 2, 3 번 고장' 테이터(뉴 따릉이의 지분이 큼)의 이동 거리 평균과 분산이 낮게 측정된다. 뉴 따릉이의 데이터의 시간은 웹서버를 통해 관리되고 있어 2행 2열에서 이상치는 발생하지 않는다.

따릉이 모델 중 '개선형'과 '뉴 따릉이'의 이용 시간 평균이 사용 시간이 높게 나오는데, 2017년 1월부터 시행된 따릉이 2시간 이용권의 영향으로 생각된다. 그전에는 정기권 1시간 이내 사용만 가능했다. 2행 2열 뉴 따릉이 이의 사용 시간 평균이 높은 것이 2행 1열 '1, 2, 3번 고장' 데이터의 분산으로 나타난다.

# 4. 결론
1. 자전거 사용양 고장 건수 사이에 선형 관계를 확인했다. (ch2.4 표)
2. 사용양과 고장 사이의 선형계수(기울기)가 계절성의 영향을 받는 것을 확인했다. (ch2.4 그림)
3. 잔고장이 많은 자전거는 사용시간 보다는 이동거리의 영향을 더 많이 받는다. (ch3.2의 그림)
4. 고장이 많이 나는 자전거일 수록 총 사용시간, 총 이동거리가 많다. (ch3.3 그림1)
5. 자전거의 평균 사용시간, 평균 이동거리는 고장난 횟수의 영향을 거의 받지 않고 비슷비슷했다. (ch3.3 그림2)


# 추후 연구
1. 생존 분석 (캐플런 마이어) 모델 공부하여 따릉이 데이터의 예상 수명을 구한다. 현재 고장과 수리가 반복되는 모형에 대해서 처리하는 방법을 공부하는 중이다.
2. 뉴 따릉이의 이동 거리를 예측하는 모형을 만들어 결측값을 보정 해본다. 공공 데이터 따릉이 대여소 목록을 보면 '위도, 경도, 지역구'에 관한 자료가 나와 있다. 
3. 코로나 영향

![](\README_img\코로나_영향.png)
![](https://github.com/ParkWonBin/seoul_bike_data/blob/main/README_img/코로나_영향.png)

# [부록] 데이터 전처리
1. [대여소 목록](https://data.seoul.go.kr/dataList/OA-13252/F/1/datasetView.do)  
엑셀에서 병합된 셀을 모두 해제하고 csv(UTF-8 쉼표로 분리)로 저장한다.  
해당 csv는 R에서 바로 읽히지 않는데, 이는 메모장에서 다른 이름으로 저장(ANSI로 인코딩)해야 R에서 읽힌다.

2. [따릉이 고장](https://data.seoul.go.kr/dataList/OA-15644/F/1/datasetView.do)   
2020.11.17에 수정된 최신 파일을 내려받는다.  
이 파일도 위와 같이 csv로 만든 후에 R에서 연다. 

```r
ID = read.csv("../공공자전거 대여소 정보(20.07.13 기준).csv")
Broken = read.csv('../서울시 공공자전거 고장신고 내역_2015_2020.10.csv')

names(ID)
# c("대여소.번호","보관소.대여소.명","자치구","상세주소","위도","경도","설치.시기","LCD","QR","운영.방식")     
names(Broken)
# c("자전거번호","등록일시","고장구분")
unique(Broken$고장구분)
#c("체인","기타 ","단말기","안장","페달","타이어 ","파손","잠금장치 불량")
```

### 3.1. 고장 데이터
자전거 번호 : SPB- 번호  
등록일시 : 2015-9-18(=0) 기준  
고장코드 :(1)체인,(2)기타, (3)단말기,(4)안장,(5)페달,(6)타이어,(7)파손,(8)잠금장치 불량

```r
date = as.Date('2015-9-18')
types = c("체인","기타 ","단말기","안장","페달","타이어 ","파손","잠금장치 불량")

BR = data.frame(bike = as.numeric(gsub('SPB-',"",Broken$자전거번호)),
                date = as.Date(substr(Broken$등록일시,1,10),'%Y-%m-%d')-date,
                type = factor(match(Broken$고장구분,types),labels =types))
write.csv(BR, file='Bike-Broken.csv.csv', row.names = FALSE)
```

### 3.2 자전거 데이터 

##### 3.2.1. R 로 전저리
자전거 번호 : SPB- 번호  
등록일시 : 2015-9-18(=0) 기준  

이 R 코드 대로 하면 2017년 4월 이전 자료까지만 읽을 수 있다. 그 이후 자료는 인코딩, 열 이름, 결측값 등 변칙적인 문제가 있어 python pandas를 사용하여 작업을 진행했다.

```r
# 폴더 내 파일 이름 검색
p = "../2015~2020_자전거 코드별 이용정보/"
f = list.files(path=p, pattern = '.csv') 

# 모든 파일 불러와서 병합
x = read.csv(paste0(p,f[1])) 
for (i in 2:length(f)){
  y = read.csv(paste0(p,f[i]))
  names(y) = names(x)
  x = rbind(x,y)
  print(f[i]) # 작업 완료된 파일 이름 출력
}

# 필요한 열만 저장
date = as.Date('2015-9-18')
bike = x[,c(1,6,3,7,10,11)]
Bike = data.frame(bike = as.numeric(gsub('SPB-',"",bike$자전거번호)),
                date = as.Date(substr(bike$반납일시,1,10),'%Y-%m-%d')-date,
                start = bike$대여소번호, 
                end = bike$반납대여소번호,
                time = bike$이용시간.분.,
                dist = bike$이용거리.M.)
```

##### 3.2.2. python 으로 전처리
정리해야 하는 파일이 총 52개 인데 파일마다 인코딩 서식이 다른 경우도 있고, 예상을 벗어나는 에러의 종류도 다양해서 파일을 하나씩 변환하여 저장하는 방법을 썼다.

아래 전처리 끝내고 시계열 작업 중 날짜 데이터에 오류가 있음을 알게 됐다.
원인은 원본 데이터의 글자 깨짐이었다. UTF-8으로 인코딩되어있지만 중간중간 인코딩이 깨져있었다. 인코딩이 깨져 중간중간 열이 밀려 쓰기 되거나 병합된 채로 읽히고 저장되는 문제다. 2015.9~2019.12까지는 나타나지 않은 문제인데, 2020.1월 이후에만 나타나 해당 부분만 작업했다.


```python 
# python 
import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from dateutil.parser import parse
from datetime import datetime, timedelta

def set_num(txt):
    try: return int(str(txt).strip("'").lstrip("SPB-"))
    except : return 0
ws = "2015~2020_자전거 코드별 이용정보/"
directory = os.listdir(ws)

# 디렉토리 내 파일 확인 (총 52개)
for i in range(len(directory)):
    print(i,':',directory[i]) 

# 전처리 작업 시작
for i in range(46):
    x = pd.read_csv(ws+directory[i],encoding='CP949')
    x = x.iloc[:,[0,5,2,6,9,10]]
    x.columns = x.columns = ['bike','date','start','end','time','dist']
    x['bike']=x['bike'].map(set_num)
    x['date']=x['date'].astype('datetime64[D]').map(lambda x : (x-datetime(2015, 9, 18)).days)

    x.to_csv(f"use/_Bike-Use{i}.csv", index=False)
```
#### 3.2.3. 날짜 데이터 오류 수정
 1. 필터링 1 :
대여/반납 대여소 명에서 인코딩이 깨지면 마지막 행에 결측값이 생기는 패턴이 있다.
마지막 열의 결측값을 기준으로 열이 병합되는 문제를 1차로 필터링한다.

 2. 필터링 2 :
데이터가 밀려 쓰기/병합되면 date에 대여소 코드/임의의 문자열이 들어와 1900년대 날짜가 반환된다.
위에서 걸리지 않은 결측값은 2015.9~2020.12 사이 날짜인지 확인하는 것으로 거를 수 있다.

 3. 데이터 보정 :
에러값의 개수는 파일당 20~30열 내외이고 문제의 파일은 총 5개이므로 보정 작업은 수작업으로 했다.
필터링 1을 한 덕분에 밀려 쓰기/병합된 행이 구분되어 비교적 간단한 수작업으로 데이터를 보정할 수 있었다.

```python
# python
def set_df2(n) :     
    x = pd.read_csv(ws+directory[n],encoding='CP949')
    nan = x.iloc[:,10].isnull()
    
    error = x[nan]
    y = x[~nan]
        
    y = y.iloc[:,[0,5,2,6,9,10]]
    y.columns = ['bike','date','start','end','time','dist']
    y['bike']=y['bike'].map(set_num)
    y['date']=y['date'].map(lambda x : (parse(x[:10])-datetime(2015, 9, 18)).days)
    
    is_date = [0 < date and date < 1920 for date in y['date']]
    if sum([not x for x in is_date]) != 0: 
        print(n)
        return y[is_date],  pd.concat([error, y[~is_date]])
    
    return y, error
    
# 정상 데이터와 에러값을 따로 저장한다.
for i in range(46,len(directory)):
    x,error = set_df2(i)
    
    x.to_csv(f"use/_Bike-Use{i}.csv", index=False)
    error.to_csv(f"use/_Bike-Use-error{i}.csv", index=False)
```


#### 3.2.4. 정상적인 데이터 병합
데이터를 저장할 때 열하나에 (문자열, 정수형, 결측(Na)값) 데이터가 등이 섞여 있는 경우가 있었다. 내 목표는 데이터를 모두 정숫값을 가진 CSV로 저장하여 Dataset 의 용량을 줄이는 것이다. pandas에서 int 열은 Nan 값을 가질 수 없기 떄문이다. 따라서 Nan이 등장하는 데이터 열은 자동으로 float 자료형으로 변환된다. 그래서 결측값을 제거하고 다시 해당 열을 int 형 자료로 변환하는 작업을 거쳤다. 

```python
# python 
# 대여소 아이디에 정수형이 아닌 데이터 처리
def set_Int(name):
    tp = type(name)
    if tp == int : return name
    elif tp == str :
        name = name.strip("'")
        try :
            name = int(name)
            return name
        except : 
            k = 0
            if name == '중랑센터' : k = 3
            elif name == '중랑정비팀test 1005' : k = 3
            elif name == '상암센터 정비실' :k = 5
            elif name == '위트콤공장' : k = 1
            elif name == '위트콤' :k = 1
            else : pass
            print(k, end="")
            return k
    elif np.isnan(name) : return name
    elif tp == float :  return int(name)
    else: print(type(name)); return name
    
ws = "use/"
directory = os.listdir(ws)
#########################################################
# 저장한 파일 모두  불러오기
cols = ['bike', 'date', 'start', 'end', 'time', 'dist']
x = pd.DataFrame(columns = cols)
for i in range(len(directory)): 
    y = pd.read_csv(ws+directory[i],encoding='utf-8')
    # 2015.9.18 ~ 2020.12.20 날짜인지 검사
    is_date = 0 < min(y['date']) and max(y['date']) < 1920 
    if not is_date :
        print('#[date error]# :',directory[i])
    else : print(i,end=" ")
    x = pd.concat([x,y]) 
display(x)

#########################################################
# 데이터가 모두 정수형으로 들어왔는지 확인
result = x
for col in cols :
    print(col, sum([type(x) != int for x in result[col]]))
    
################
# 로그 값을 보고 추적한 데이터. 
# int 열 데이터에 nan 값이 있으면 folat열로 변환된다. 
# bike : 0  
# date : 3145725  (float)  
# start : 13377238 (와트콤 384, 중량 836, 상암 374)  
# end  : 13377238 (와트콤 384, 중량 864, 상암 522)  
# time : 10403754 (str)  
# dist : 31131628 (str, float)  
# 로그 추적은 jupyer 파일 참고.
##########################################################
```

#### 3.2.5. 에러 복원한 데이터와 병합
```python 
# python 
fixed = pd.read_csv('./_Bike-Use-errors-fixed.csv',encoding='CP949') 

# 데이터 손으로 복원한 데이터 초기설정
y = fixed
y.columns = ['bike','date','start','end','time','dist']
y['bike']=y['bike'].map(set_num)
y['date']=y['date'].map(lambda x : (parse(x[:10])-datetime(2015, 9, 18)).days)
fixed = y

# 정상 데이터와 병합
result = pd.concat([result,y])

result = result.dropna() # nan 붙어있는 데이터 삭제
result['date'] = result['date'].map(set_Int)
result['start'] = result['start'].map(set_Int)
result['end'] = result['end'].map(set_Int)
result['time'] = result['time'].map(set_Int)
result['dist'] = result['dist'].map(set_Int)

result = result.sort_values(by='date')
result.to_csv(f"Bike-Use-final.csv", index=False)

################
# 사용량 그래프 복기
a = result.date.value_counts()
b = pd.DataFrame({'date' : a.index, 'count' : a.values})
display(b.plot.scatter(x='date', y='count'))
```


### 3.3 전처리 결과
#### 자전거 고장 : 파일 1개 (6MB) -> 1개(3.9MB)
#### 자전거 사용 : 파일 52개(5.2GB) -> 52개(1.23GB) -> ZIP(358.5MB)
GitHub 단일 파일 용량 제한 으로 50MB씩 분할하여 416MB

#### 파일 설명 : 
- 자전거 고장 : bike(자전거ID),date(접수일 2015.9.18일 +day), type(사유)  
(1)체인, (2)기타, (3)단말기, (4)안장, (5)페달, (6)타이어, (7)파손, (8)잠금장치 불량

- 자전거 사용 : bike(자전거ID),date(반납일 2015.9.18일 +day),   
                start(출발-대여소ID), end(반납-대여소ID), time(이동시간-분),  dist(이동거리-미터)

```r
length(unique(sapply(BR$bike,as.character)))  ; length(BR[,1]) 
length(unique(sapply(Bike$bike,as.character))); length(Bike[,1])
# 고장 신고 : 181592 건
# 전체 이용 : 2043069건
# 고장 자전거 : 3400 대
# 전체 자전거 : 4971 대
```
