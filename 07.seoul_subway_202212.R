#작업에 필요한 패키지 설치
install.packages("dplyr")
install.packages("ggplot2")

#라이브러리 등록
library(dplyr)
library(ggplot2)

#subway 구조 확인
str(subway)

#변수의 이상치와 결측치 확인하고 처리
summary(subway)

#분석목적에 따른 파생변수 만들기
#파생변수1.정수형day변수
subway$day <- substr(subway$Date,7,8)
class(subway$day)
subway$day <- as.integer(subway$day)

#파생변수2.line변수
table(subway$Line)
subway$Line <- ifelse(subway$Line=="9호선2~3단계","9호선",subway$Line)

#파생변수3.station변수
table(subway$Station)

#파생변수4.총승하차승객수 total_passenger
subway$total_passenger <- subway$on_board + subway$getting_off

#분석데이터 최종확인
str(subway)

#데이터분석
#1. 지하철역의 하루 평균 승차/하차승객수
subway%>%
  summarise(on_m=mean(on_board),off_m=mean(getting_off))

#2.승차승객수가 가장 많았던 역의 노선을 찾아보기
#2-1.최댓값 찾기
max(subway$on_board)
#2-2.최댓값인 역과 노선 찾기
subway%>%
  filter(on_board==94732)%>%
  select(Date,Line,Station,on_board)

#3.역별 하루 평균 전체승객수 분석
#3-1. 하루 평균 전체 승객수가 많은 10곳 찾기
passenger10 <- subway%>%
  group_by(Station)%>%
  summarise(m=mean(total_passenger))%>%
  arrange(desc(m))%>%
  head(10)

passenger11 <- subway%>%
  group_by(Station,Line)%>%
  summarise(m=mean(total_passenger))%>%
  arrange(desc(m))%>%
  head(10)
#3-2. 상위 3곳 찾기
head(passenger10,3)

#4.역별 일평균 전체승객수 상위 10개 역을 막대그래프로 작성
ggplot(data = passenger10, aes(x=reorder(Station,m),y=m)) +
  geom_col()+
  coord_flip()

ggplot(data = passenger11, aes(x=reorder(Station,m),y=m)) +
  geom_col()+
  coord_flip()
#5.일별 전체승객 분석
subway%>%
  group_by(Date)%>%
  summarise(total=sum(total_passenger))%>%
  arrange(desc(total))%>%
  head(10)

#6.특정 line 분석(1호선)
subway%>%
  filter(Line=="1호선")%>%
  filter(total_passenger==max(total_passenger))%>%
  select(Date,Station,on_board,getting_off,total_passenger)

#7.노선별 전체승객 비율 비교
line_pct <- subway%>%
  group_by(Line)%>%
  summarise(total=sum(total_passenger))%>%
  mutate(all=sum(total),pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct))%>%
  head(5)

#8.지하철 전체 승객 비율 막대그래프 그리기
line_pct10 <- line_pct%>%
  filter(Line%in%c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선"))

ggplot(data = line_pct10,aes(x=reorder(Line,pct),y=pct)) +
  geom_col()+
  coord_flip()+
  ggtitle("수도권 지하철 노선별 이용비율")+
  xlab("노선")+
  ylab("이용비율")

#9.일별 전체승객 선 그래프 그리기
line_graph <- subway%>%
  group_by(day)%>%
  summarise(s=sum(total_passenger))

ggplot(data = line_graph,aes(x=day,y=s,group=1))+
  geom_line()+
  ggtitle("수도권 지하철 일별 이용승객수")+
  xlab("일")+
  ylab("이용승객")