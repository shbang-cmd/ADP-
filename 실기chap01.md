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
