# 전처리 기준

## Bike-Brock.csv
자전거 번호 : SPB- 번호
등록일시 : 2015-9-18(=0) 기준
고장코드 :(1)체인,(2)단말기,(3)안장,(4)페달,(5)타이어,(6)파손,(7)잠금장치 불량,(8)기타

```r
BR = data.frame(bike = as.numeric(gsub('SPB-',"",Broken$자전거번호)),
                date = as.Date(substr(Broken$등록일시,1,10), '%Y-%m-%d') - as.Date('2015-9-18'),
                type = match(Broken$고장구분,c("체인","단말기","안장","페달","타이어 ","파손","잠금장치 불량","기타 ")))
```