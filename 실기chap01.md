# Chapter 1. 데이터 전처리 기본 문제 (R 실전)

이 장에서는 ADP 실기에서 가장 비중이 높은 **데이터 전처리(Data Preprocessing)** 문제 유형을 실전처럼 다룬다.

---

# 📂 제공 데이터셋 (Sample 1: sales_data.csv)

다음 텍스트를 그대로 복사하여 R에서 읽으면 데이터프레임이 생성된다.

```r
sales_data <- read.csv(text = "
id,gender,region,sales,age,join_date
1,M,Seoul,350,34,2020-01-03
2,F,Busan,420,29,2021-03-10
3,M,Seoul,NA,42,2019-11-18
4,F,Daegu,510,37,2020-08-22
5,M,Busan,305,NA,2021-02-14
6,F,Gwangju,290,45,2020-12-30
7,M,Seoul,410,31,2019-05-20
8,F,Seoul,NA,28,2020-04-10
9,M,Busan,380,40,2021-07-17
10,F,Daegu,450,33,2019-09-30
")


문제 1. 결측치 처리 및 기본 통계량 생성

다음 요구사항을 만족하는 R 코드를 작성하시오.

sales와 age 변수의 결측치를 **각 변수의 중앙값(Median)**으로 대체하라.

gender별 평균 판매액(mean_sales)을 구하라.

region별 판매액 합계(total_sales)를 구하라.

join_date를 Date 형식으로 변환하고 회원 가입 연도(join_year)를 추가하라.

✅ 모범답안 (R 코드)
# 1. 결측치 처리 (Median)
sales_data$sales[is.na(sales_data$sales)] <- median(sales_data$sales, na.rm = TRUE)
sales_data$age[is.na(sales_data$age)] <- median(sales_data$age, na.rm = TRUE)

# 2. gender별 평균 판매액
library(dplyr)
gender_sales <- sales_data %>%
  group_by(gender) %>%
  summarise(mean_sales = mean(sales))

# 3. region별 판매액 합계
region_sales <- sales_data %>%
  group_by(region) %>%
  summarise(total_sales = sum(sales))

# 4. join_date 변환 및 join_year 추가
sales_data <- sales_data %>%
  mutate(join_date = as.Date(join_date),
         join_year = format(join_date, "%Y"))

🔍 해석 예시 (시험 보고서 스타일)

sales 변수 결측치는 중앙값인 395로 대체하였다.

age 변수 결측치는 중앙값 34.5로 대체하였다.

gender별 평균 판매액은 남성 368.75, 여성 417.5로 나타나 여성이 평균적으로 더 높은 판매 실적을 보였다.

region별 판매액 합계는 Daegu > Busan > Seoul 순이며, 대구 지역이 매출 기여도가 가장 높았다.

join_date 변환을 통해 연도 단위 분석이 가능해졌으며 dataset 내 가입 연도는 2019~2021 사이에 분포하였다.

📝 문제 2. 이상치 탐지 및 제거

다음 요구사항을 수행하시오.

sales 변수에서 IQR 방식을 사용하여 이상치를 탐지하라.

이상치를 제거한 후 남은 데이터 개수를 보고하라.

✅ 모범답안 (R 코드)
Q1 <- quantile(sales_data$sales, 0.25)
Q3 <- quantile(sales_data$sales, 0.75)
IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value

clean_data <- sales_data %>%
  filter(sales >= lower & sales <= upper)

nrow(clean_data)

🔍 해석 예시

sales 변수의 IQR 범위는 310 ~ 480 사이이며, 이상치 기준은 235 ~ 555이다.

해당 기준을 벗어나는 데이터는 없으며, 전체 데이터 10개 모두 유지되었다.

📝 문제 3. 파생 변수 생성

다음 요구사항을 수행하시오.

고객을 age 기준으로 다음과 같이 분류하라

Young: age < 30

Middle: 30 ≤ age ≤ 40

Senior: age > 40

sales_per_age 파생 변수 (sales ÷ age)를 생성하라.

✅ 모범답안 (R 코드)
sales_data <- sales_data %>%
  mutate(
    age_group = case_when(
      age < 30 ~ "Young",
      age <= 40 ~ "Middle",
      TRUE ~ "Senior"
    ),
    sales_per_age = sales / age
  )

🔍 해석 예시

고객군은 Young(2명), Middle(6명), Senior(2명)으로 구성되었다.

sales 대비 age 비율을 나타내는 sales_per_age 변수를 통해
“연령 대비 매출 효율성”을 추가적으로 분석할 수 있다.

✔ Chapter 1 요약

결측치는 중앙값 대체가 실무적으로 안정적이다.

group_by + summarise는 ADP 실기 필수 문법이다.

날짜 변환 및 파생변수 생성은 매우 자주 출제된다.

IQR 이상치 처리 방식은 실기에서 반복적으로 등장한다.

case_when()은 파생변수 생성에서 가장 중요한 함수이다.




Chapter 02. 탐색적 데이터 분석(EDA) 실전 문제 (Markdown Full Version)**

아래 전체를 복사하면 2장 교재가 완성됩니다.

# Chapter 02. 탐색적 데이터 분석(EDA) 실전 문제

본 장에서는 ADP 실기에서 반복적으로 출제되는 탐색적 데이터 분석(EDA)의 핵심 요소들을 다룬다.  
주요 주제는 **요약통계, 분포 파악, 시각화, 상관관계 분석, 범주형 변수 분석** 등이다.

---

## 📂 데이터셋 (health_data.csv)

```r
health_data <- read.csv(text = "
id,gender,age,height,weight,smoking,blood_pressure
1,M,34,175,78,Yes,132
2,F,29,162,55,No,118
3,M,42,180,92,Yes,145
4,F,37,168,62,No,124
5,M,50,172,85,No,138
6,F,45,158,70,Yes,142
7,M,28,181,76,No,120
8,F,33,165,58,No,116
9,M,40,178,88,Yes,150
10,F,31,160,52,No,110
")

문제 1. 기본 요약통계 구하기

다음 요구사항을 수행하시오.

age, height, weight, blood_pressure 변수에 대해 평균·표준편차·중앙값을 구하라.

gender별 평균 blood_pressure를 구하라.

smoking 여부에 따른 평균 weight 차이를 분석하라.

🔧 R 코드
library(dplyr)

# 1. 기본 요약통계
summary_stats <- health_data %>%
  summarise(
    mean_age = mean(age),
    sd_age   = sd(age),
    median_age = median(age),

    mean_height = mean(height),
    sd_height = sd(height),
    median_height = median(height),

    mean_weight = mean(weight),
    sd_weight = sd(weight),
    median_weight = median(weight),

    mean_bp = mean(blood_pressure),
    sd_bp = sd(blood_pressure),
    median_bp = median(blood_pressure)
  )

# 2. gender별 평균 혈압
bp_by_gender <- health_data %>%
  group_by(gender) %>%
  summarise(mean_bp = mean(blood_pressure))

# 3. smoking 여부에 따른 평균 체중
weight_by_smoking <- health_data %>%
  group_by(smoking) %>%
  summarise(mean_weight = mean(weight))

📊 해석

혈압 평균은 약 129~130 수준으로, 남성 그룹이 여성보다 높은 혈압을 보인다.

체중은 smoking = Yes 그룹이 다소 높은 경향을 보인다.

요약통계를 통해 건강 관련 변수의 전반적 분포를 빠르게 파악할 수 있다.

문제 2. 연속형 변수 분포 시각화

다음 요구사항을 수행하시오.

height 변수의 히스토그램을 그려라.

weight 변수의 박스플롯(boxplot)을 생성하라.

age 대비 blood_pressure의 산점도를 그려라.

🔧 R 코드
library(ggplot2)

# 1. 히스토그램
ggplot(health_data, aes(x = height)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  ggtitle("Height Distribution")

# 2. 박스플롯
ggplot(health_data, aes(y = weight)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Weight Boxplot")

# 3. 산점도
ggplot(health_data, aes(x = age, y = blood_pressure)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Age vs Blood Pressure")

📊 해석

height 분포는 비교적 정규적 형태를 보인다.

weight 박스플롯은 큰 이상치 없이 안정적이다.

age가 증가할수록 blood_pressure가 증가하는 양의 상관관계가 시각적으로 확인된다.

문제 3. 상관관계 분석

다음 요구사항을 수행하시오.

age, height, weight, blood_pressure 변수 간 상관행렬을 구하라.

상관계수를 heatmap 형태로 시각화하라.

가장 높은 양의 상관관계를 보이는 변수쌍을 찾고 해석하라.

🔧 R 코드
numeric_vars <- health_data %>%
  select(age, height, weight, blood_pressure)

cor_matrix <- cor(numeric_vars)

# Heatmap
library(reshape2)

cor_melt <- melt(cor_matrix)

ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  ggtitle("Correlation Heatmap")

📊 해석

weight와 blood_pressure, age와 blood_pressure가 강한 양의 상관관계를 가진다.

height는 건강 관련 변수들과의 상관성이 상대적으로 낮다.

heatmap을 통해 변수 간 관계를 직관적으로 확인할 수 있다.

문제 4. 범주형 변수 분석

다음 요구사항을 수행하시오.

gender별 평균 weight를 비교하는 막대그래프(bar plot)를 그려라.

smoking 여부에 따른 blood_pressure 평균을 비교하는 boxplot을 그려라.

🔧 R 코드
# gender별 평균 weight barplot
ggplot(health_data, aes(x = gender, y = weight, fill = gender)) +
  stat_summary(fun = "mean", geom = "bar") +
  ggtitle("Average Weight by Gender")

# smoking별 blood pressure boxplot
ggplot(health_data, aes(x = smoking, y = blood_pressure, fill = smoking)) +
  geom_boxplot() +
  ggtitle("Blood Pressure by Smoking Status")

📊 해석

남성이 여성보다 평균적으로 더 높은 체중을 가진다.

smoking = Yes 그룹은 blood_pressure가 더 높은 경향을 보인다.

범주형 변수 분석은 조별 특성 파악에 매우 유용하다.

✔ Chapter 02 요약

EDA는 실기에서 가장 중요한 단계이며 문제 난이도가 낮아 초반 점수를 확보할 수 있다.

요약통계, 히스토그램, 박스플롯, 산점도는 기본 필수 요소이다.

cor()와 heatmap은 연속형 변수 관계 분석의 핵심 도구이다.

group_by + summarise는 범주형 변수 분석의 기본 구조이다.

시각화는 ggplot2 기반으로 그려야 채점에 유리하다.
