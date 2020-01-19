library(tidyverse)
library(gridExtra)


#1. Data load

accident <- read.csv("caraccident2013.csv", stringsAsFactors = FALSE)

str(accident)
head(accident)

#2. 전처리

#.Comment. 피벗 테이블 형태의 집계 테이블로 구성된 데이터셋을 분석에 용이한 형태로 변경 필요

#.사고건수, 사망자수, 부상자수로 각 3 rows 반복되는 형태

accident2 <- accident %>%
  mutate(month = rep(1:12, each = 3)) # 1부터 12까지 각 3 rows 씩 반복하여 만듬

accident3 <- accident2 %>%
  mutate(월 = paste0(month,"월")) %>% # '월' 컬럼을 'month' 컬럼과 '월'을 'paste0'를 이용 대체
  select(-month) # 'month' 제거

accident3$월 <- as.factor(accident3$월) # factor로 변경
levels(accident3$월) # levle 확인
accident3$월 <- factor(accident3$월,
                      levels = c("1월", "2월", "3월", "4월", "5월", "6월",
                                 "7월", "8월", "9월", "10월", "11월", "12월")) # level 지정
accident3


#. Comment. 주/야로 각 나누어진 수치(건)을 type 컬럼으로 테이블을 정규화

## tidyr 패키지 이용. 주/야 gathering 'library(tidyr)' or 위와 같이 'library(tidyverse)'

accident4 <- accident3 %>%
  gather(colnames(accident3)[-c(1,2)],
         key = "주야구분",
         value = "건수")

accident5 <- accident4 %>%
  rename("타입" = 주야) # 컬럼명 변경 '주야' -> '타입' 

str(accident5)

#3. EDA

###. 교통사고 건수가 가장 많은 월은 언제인가?

gg_1 <- accident5 %>%
  filter(타입 == "사고건수") %>%
  group_by(월) %>%
  summarise(건수 = sum(건수)) %>%
  ggplot(aes(x = 월, y = 건수, group = 1)) +
  geom_line() +
  geom_point()
gg_1

###. 주/야로 구분하였을 때 월별 현황은 어떠한가?

gg_2 <- accident5 %>%
  filter(타입 == "사고건수") %>%
  group_by(월, 주야구분) %>%
  summarise(건수 = sum(건수)) %>%
  ggplot(aes(x = 월, y = 건수, group = 주야구분, color = 주야구분)) +
  geom_line() +
  geom_point()
gg_2


###. 사망자수가 가장 많았던 월은?

gg_3 <- accident5 %>%
  filter(타입 == "사망자수") %>%
  group_by(월) %>%
  summarise(건수 = sum(건수)) %>%
  ggplot(aes(x = 월, y = 건수, group = 1)) +
  geom_line() +
  geom_point()
gg_3


###. 사고건수와 사망자수 비교해보기
grid.arrange(gg_1, gg_3, ncol = 2)


###. 주간/야간에 따른 사망자수의 현황

gg_4 <- accident5 %>%
  filter(타입 == "사망자수") %>%
  group_by(월, 주야구분) %>%
  summarise(건수 = sum(건수)) %>%
  ggplot(aes(x = 월, y = 건수, group = 주야구분, color = 주야구분)) +
  geom_line() +
  geom_point()
gg_4


###. 주간/야간에 따른 각 사고건수, 사망자수, 부상자수의 상관관계 파악하기

#4. 반정규화 된 테이블로 주간/야간 각 사고건수, 사망자수, 부상자수 나타내기


## 1단계 주/야 데이터셋 나누기

day_tb <- accident5 %>%
  filter(주야구분 == "주") %>%
  spread(key = "주야구분", value = "건수") %>%
  spread(key = "타입", value = "주")               # 주간 테이블

colnames(day_tb) <- c("월", "주간부상자수", "주간사고건수", "주간사망자수") # 컬럼변경

night_tb <- accident5 %>%
  filter(주야구분 == "야") %>%
  spread(key = "주야구분", value = "건수") %>%
  spread(key = "타입", value = "야")               # 야간 테이블

colnames(night_tb) <- c("월", "야간부상자수", "야간사고건수", "야간사망자수") # 컬럼변경

## 2단계 주/야 Data Merge

sago_tb <- inner_join(day_tb, night_tb, by = "월")


####. 주간/야간 각 상관관계

cor(sago_tb[-1]) # 상관계수만 보기

library(psych)
pairs.panels(sago_tb[-1])  # 좀 더 유익한 산포도 행렬 생성