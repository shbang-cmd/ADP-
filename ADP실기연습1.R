# ADP 실기연습 1

library(dplyr)
# sales_data.csv를 읽거나 아래와 같이 데이터를 가져온다.
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

# 문제 1. 결측치 처리 및 기본 통계량 생성
# 다음 요구사항을 만족하는 R 코드를 작성하시오.# 
# sales와 age 변수의 결측치를 **각 변수의 중앙값(Median)**으로 대체하라.# 
# gender별 평균 판매액(mean_sales)을 구하라.# 
# region별 판매액 합계(total_sales)를 구하라.# 
# join_date를 Date 형식으로 변환하고 회원 가입 연도(join_year)를 추가하라.

# 1. 결측치 처리 (Median)
# 중앙값 확인 후 결측치 처리
median(sales_data$sales, na.rm = TRUE)
sales_data$sales[is.na(sales_data$sales)] <- median(sales_data$sales, na.rm = TRUE)
median(sales_data$age, na.rm = TRUE)
sales_data$age[is.na(sales_data$age)] <- median(sales_data$age, na.rm = TRUE)

# 2. gender별 평균 판매액
gender_sales <- sales_data %>% 
        group_by(gender) %>% 
        summarise(mean_sales = mean(sales))
gender_sales

# 3. region별 판매액 합계
region_sales <- sales_data %>% 
        group_by(region) %>% 
        summarise(total_sales = sum(sales)) %>% 
        arrange(desc(total_sales))
region_sales

# 4. join_date 변환 및 join_year 추가
sales_data <- sales_data %>%
        mutate(join_date = as.Date(join_date),
               join_year = format(join_date, "%Y"))
sales_data

# 해석 예시 (시험 보고서 스타일)
# 
# sales 변수 결측치는 중앙값인 395로 대체하였다.
# age 변수 결측치는 중앙값 34.5로 대체하였다.
# gender별 평균 판매액은 남성 368, 여성 413으로 나타나 여성이 평균적으로 더 높은 판매 실적을 보였다.
# region별 판매액 합계는 Daegu < Busan < Seoul 순이며, Seoul지역이 매출 기여도가 가장 높았다.
# join_date 변환을 통해 연도 단위 분석이 가능해졌으며 dataset 내 가입 연도는 2019~2021 사이에 분포하였다.


# 문제 2. 이상치 탐지 및 제거
# 
# 다음 요구사항을 수행하시오.
# sales 변수에서 IQR 방식을 사용하여 이상치를 탐지하라.
# 이상치를 제거한 후 남은 데이터 개수를 보고하라.

Q1 <- quantile(sales_data$sales, 0.25)
Q3 <- quantile(sales_data$sales, 0.75)
IQR_value <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_value
upper <- Q3 + 1.5 * IQR_value
nrow(sales_data)
clean_data <- sales_data %>% 
        filter(sales >= lower & sales <= upper)
nrow(clean_data)

# 해석 예시
# sales 변수의 IQR 범위는 310 ~ 480 사이이며, 이상치 기준은 235 ~ 555이다.
# 해당 기준을 벗어나는 데이터는 없으며, 전체 데이터 10개 모두 유지되었다.


# 문제 3. 파생 변수 생성
# 
# 다음 요구사항을 수행하시오.
# 고객을 age 기준으로 다음과 같이 분류하라
# Young: age < 30
# Middle: 30 ≤ age ≤ 40
# Senior: age > 40
# sales_per_age 파생 변수 (sales ÷ age)를 생성하라.

sales_data <- sales_data %>%
        mutate(
                age_group = case_when(
                        age < 30 ~ "Young",
                        age <= 40 ~ "Middle",
                        TRUE ~ "Senior"
                ),
                sales_per_age = sales / age
        )
# 해석 예시
# 
# 고객군은 Young(2명), Middle(6명), Senior(2명)으로 구성되었다.
# sales 대비 age 비율을 나타내는 sales_per_age 변수를 통해
# “연령 대비 매출 효율성”을 추가적으로 분석할 수 있다.



# ADP 실기연습 2

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

# 문제 1. 기본 요약통계 구하기
# 
# 다음 요구사항을 수행하시오.
# age, height, weight, blood_pressure 변수에 대해 평균·표준편차·중앙값을 구하라.
# gender별 평균 blood_pressure를 구하라.
# smoking 여부에 따른 평균 weight 차이를 분석하라.

library(dplyr)
# 1-1. 기본 요약통계
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
summary_stats

# 1-2. gender별 평균 혈압
bp_by_gender <- health_data %>%
        group_by(gender) %>%
        summarise(mean_bp = mean(blood_pressure))
bp_by_gender

# 1-3. smoking 여부에 따른 평균 체중
weight_by_smoking <- health_data %>%
        group_by(smoking) %>%
        summarise(mean_weight = mean(weight))
weight_by_smoking

# 해석
# 혈압 평균은 약 129~130 수준으로, 남성 그룹이 여성보다 높은 혈압을 보인다.
# 체중은 smoking = Yes 그룹이 다소 높은 경향을 보인다.
# 요약통계를 통해 건강 관련 변수의 전반적 분포를 빠르게 파악할 수 있다.


# 문제 2. 연속형 변수 분포 시각화
 
# 다음 요구사항을 수행하시오.
# height 변수의 히스토그램을 그려라.
# weight 변수의 박스플롯(boxplot)을 생성하라.
# age 대비 blood_pressure의 산점도를 그려라.

library(ggplot2)
# 2-1. 히스토그램
ggplot(health_data, aes(x = height)) +
        geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
        ggtitle("Height Distribution")

# 2-2. 박스플롯
ggplot(health_data, aes(y = weight)) +
        geom_boxplot(fill = "orange") +
        ggtitle("Weight Boxplot")

# 2-3. 산점도
ggplot(health_data, aes(x = age, y = blood_pressure)) +
        geom_point(color = "red") +
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle("Age vs Blood Pressure")

# 해석
# 
# height 분포는 비교적 정규적 형태를 보인다.
# weight 박스플롯은 큰 이상치 없이 안정적이다.
# age가 증가할수록 blood_pressure가 증가하는 양의 상관관계가 시각적으로 확인된다.


# 문제 3. 상관관계 분석
# 
# 다음 요구사항을 수행하시오.
# age, height, weight, blood_pressure 변수 간 상관행렬을 구하라.
# 상관계수를 heatmap 형태로 시각화하라.
# 가장 높은 양의 상관관계를 보이는 변수쌍을 찾고 해석하라.

# 3-1. 상관행렬
numeric_vars <- health_data %>%
        select(age, height, weight, blood_pressure)
numeric_vars
cor_matrix <- cor(numeric_vars)
cor_matrix

# 3-2. Heatmap
# install.packages("reshape2")
library(reshape2)

cor_melt <- melt(cor_matrix)
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
        ggtitle("Correlation Heatmap")

# 3-3. 해석
# weight와 blood_pressure, age와 blood_pressure가 강한 양의 상관관계를 가진다
# height는 건강 관련 변수들과의 상관성이 상대적으로 낮다.
# heatmap을 통해 변수 간 관계를 직관적으로 확인할 수 있다.


# 문제 4. 범주형 변수 분석
# 
# 다음 요구사항을 수행하시오.
# gender별 평균 weight를 비교하는 막대그래프(bar plot)를 그려라.
# smoking 여부에 따른 blood_pressure 평균을 비교하는 boxplot을 그려라.

# 4-1. gender별 평균 weight barplot
ggplot(health_data, aes(x = gender, y = weight, fill = gender)) +
        stat_summary(fun = "mean", geom = "bar") +
        ggtitle("Average Weight by Gender")

# 4-2. smoking별 blood pressure boxplot
ggplot(health_data, aes(x = smoking, y = blood_pressure, fill = smoking)) +
        geom_boxplot() +
        ggtitle("Blood Pressure by Smoking Status")

# 해석
# 남성이 여성보다 평균적으로 더 높은 체중을 가진다.
# smoking = Yes 그룹은 blood_pressure가 더 높은 경향을 보인다.
# 범주형 변수 분석은 조별 특성 파악에 매우 유용하다.


# ADP 실기연습 3

customer_info <- read.csv(text = "
cust_id,name,gender,join_date
1,Kim,M,2020-01-03
2,Lee,F,2021-03-10
3,Park,M,2019-11-18
4,Choi,F,2020-08-22
5,Jung,M,2021-02-14
")
purchase_log <- read.csv(text = "
cust_id,product,amount,purchase_date
1,A,300,2020-02-10
1,B,150,2020-02-15
2,B,200,2021-04-01
3,A,500,2020-01-10
3,C,250,2020-01-15
4,B,120,2020-09-01
4,C,330,2020-09-15
5,A,420,2021-03-01
")

# 문제 1. Inner Join / Left Join 수행
# 
# 요구사항
# customer_info와 purchase_log를 cust_id 기준 inner join하라.
# 모든 고객 정보를 유지하는 left join을 수행하라.
# left join 결과에서 purchase 내역이 없는 고객을 확인하라.

library(dplyr)

# 1-1. Inner Join
inner_join_data <- customer_info %>%
        inner_join(purchase_log, by = "cust_id")
inner_join_data

# 1-2. Left Join
left_join_data <- customer_info %>%
        left_join(purchase_log, by = "cust_id")
left_join_data

# 1-3. 구매 내역이 없는 고객 찾기
no_purchase <- left_join_data %>%
        filter(is.na(amount))
no_purchase

# 해석
# inner join에서는 양쪽 테이블에 존재하는 고객만 남는다.
# left join은 customer_info 전체를 유지하며, 구매 기록이 없으면 amount가 NA로 표시된다.
# no_purchase를 통해 구매 기록이 없는 고객은 0명임을 확인할 수 있다.

# 문제 2. Pivot(피벗) 및 요약 테이블 생성
# 
# 요구사항
# 고객별 제품(product) 구매 금액을 wide format으로 피벗하라.
# 각 고객별 총 구매액(total_amount)을 구하라.
# 총 구매액이 400 이상인 고객을 추출하라.

library(tidyr)

# 2-1. Pivot wider
pivot_data <- purchase_log %>%
        pivot_wider(names_from = product, values_from = amount, values_fill = 0)
pivot_data

# 2-2. 고객별 총 구매액
total_purchase <- purchase_log %>%
        group_by(cust_id) %>%
        summarise(total_amount = sum(amount))
total_purchase

# 2-3. 총 구매액 400 이상 고객
high_value <- total_purchase %>%
        filter(total_amount >= 400)
high_value

# 해석
# 피벗 결과 고객별로 A, B, C 제품 구매액이 열로 나열되어 분석이 쉬워진다.
# 총 구매액 400 이상 고객은 cust_id 1, 3, 5로 나타났다.
# high-value 고객군은 마케팅 타깃팅에 중요하다.


# 문제 3. 문자열 처리(String Handling)
# 
# 요구사항
# name 변수에서 이름의 첫 글자를 추출해 initial 변수를 생성하라.
# product 이름을 모두 소문자(lowercase)로 변환하라.
# 문자열 길이(name_length)를 계산하라.

library(stringr)
# 3-1. 3-3.
string_data <- customer_info %>%
        mutate(
                initial = str_sub(name, 1, 1),  # 문자열에서 1번째 문자부터 1번째 문자까지, 즉 첫 글자만 추출
                name_length = str_length(name)
        )
string_data

# 3-2.
purchase_log <- purchase_log %>%
        mutate(product = str_to_lower(product))
purchase_log

# 해석
# initial 변수를 통해 고객 약칭을 생성할 수 있다.
# product 값을 소문자로 통일하여 분석 시 오류를 방지한다.
# 문자열 길이 분석은 고객 이름 패턴 분석 등에 활용 가능하다.


# 문제 4. 날짜 처리(Date Handling)
# 
# 요구사항
# join_date와 purchase_date를 Date 형식으로 변환하라.
# 구매일 기준 연도(purchase_year)와 월(purchase_month) 파생 변수를 생성하라.
# 구매 시점과 가입일 간의 날짜 차이(day_diff)를 구하라.

# 4-1.
customer_info <- customer_info %>%
        mutate(join_date = as.Date(join_date))
customer_info

# 4-2.
purchase_log <- purchase_log %>%
        mutate(
                purchase_date = as.Date(purchase_date),
                  # 문자열 → Date 형식으로 변환
                purchase_year = format(purchase_date, "%Y"),
                purchase_month = format(purchase_date, "%m"),
                day_diff = as.numeric(purchase_date - customer_info$join_date[
                        match(cust_id, customer_info$cust_id)
                        # purchase_log의 cust_id가 customer_info의 어느 행에 위치하는지 번호를 반환
                        ])  #두 날짜의 차이
        )
purchase_log

# 해석
# 
# 구매 활동이 가입 후 얼마나 빠르게 발생했는지(day_diff) 파악할 수 있다.
# 연도·월 단위 분석을 통해 계절성(seasonality) 분석이 가능하다.



# ADP 실기 4

patient_data <- read.csv(text = "
id,gender,age,bmi,glucose,blood_pressure,smoking
1,M,34,23.1,98,132,Yes
2,F,29,20.4,NA,118,No
3,M,42,27.3,110,NA,Yes
4,F,37,22.1,95,124,No
5,M,50,28.9,130,138,No
6,F,45,24.0,115,142,Yes
7,M,28,26.5,105,120,No
8,F,33,21.0,99,116,No
9,M,40,27.9,NA,150,Yes
10,F,31,19.8,92,110,No
")

# 문제 1. 그룹별 결측치 처리 (Group-wise Imputation)
# 
# 요구사항
# glucose 변수의 결측치를 gender별 평균 값으로 대체하라.
# blood_pressure 결측치는 age 그룹(30미만 / 30~40 / 40초과) 기준 평균으로 대체하라.
# 결측치 대체 후 데이터의 결측치 수를 확인하라.

library(dplyr)

# 1-1. glucose: 결측치를 gender 그룹별 평균 대체
patient_data <- patient_data %>%
        group_by(gender) %>%
        mutate(glucose = ifelse(is.na(glucose),
                                mean(glucose, na.rm = TRUE),
                                glucose)) %>%
        ungroup()  # 이후의 연산이 그룹 상태 영향을 받지 않도록 그룹 해제
patient_data

# 1-2. age 그룹 생성
patient_data <- patient_data %>%
        mutate(age_group = case_when(
                age < 30 ~ "Young",
                age <= 40 ~ "Middle",
                TRUE ~ "Senior"
        ))
patient_data

# 1-3. blood_pressure: age_group별 평균 대체
patient_data <- patient_data %>%
        group_by(age_group) %>%
        mutate(blood_pressure = ifelse(is.na(blood_pressure),
                                       mean(blood_pressure, na.rm = TRUE),
                                       blood_pressure)) %>%
        ungroup()
patient_data

# 1-4. 결측치 확인
colSums(is.na(patient_data))

# 해석
# 
# gender별 평균으로 glucose를 대체함으로써 성별 차이를 보존한 합리적 처리 방법이다.
# 혈압의 경우 age_group 기반 평균 대체는 실제 의료 데이터 처리 방식과 유사하다.
# 모든 결측치가 제거되어 분석 가능한 완성 데이터셋이 생성되었다.


# 문제 2. KNN 기반 결측치 대체 (KNN Imputation)
# 
# 요구사항
# 결측치가 포함된 원본 데이터를 다시 불러와라.
# KNN 기반 결측치 대체(k = 3)를 수행하라.
# 대체 과정에서 사용되는 변수들만 선택하라(age, bmi, glucose, blood_pressure).

patient_data_raw <- read.csv(text = "
id,gender,age,bmi,glucose,blood_pressure,smoking
1,M,34,23.1,98,132,Yes
2,F,29,20.4,NA,118,No
3,M,42,27.3,110,NA,Yes
4,F,37,22.1,95,124,No
5,M,50,28.9,130,138,No
6,F,45,24.0,115,142,Yes
7,M,28,26.5,105,120,No
8,F,33,21.0,99,116,No
9,M,40,27.9,NA,150,Yes
10,F,31,19.8,92,110,No
")
patient_data_raw
# install.packages("VIM")
library(VIM)

knn_data <- kNN(patient_data_raw[, c("age", "bmi", "glucose", "blood_pressure")],
                k = 3)  # 수치형 변수는 가까운 이웃 3명의 평균값으로 대체
knn_data

# 해석
# KNN 대체는 주변 관측값의 유사도를 기반으로 결측치를 채우기 때문에 단순 평균/중앙값보다 정확도가 높다.
# 특히 glucose와 blood_pressure 같은 건강 지표에 적합한 대체 방식이다.


# 문제 3. 다변량 이상치 탐지 (Mahalanobis Distance)

# 요구사항
# age, bmi, glucose, blood_pressure 4개 변수로 Mahalanobis Distance를 계산하라.
# 카이제곱 분포 기준(p = 0.01)으로 이상치를 판정하라.
# 이상치 데이터만 추출하라.

# Mahalanobis Distance : 변수의 단위, 분산, 상관관계까지 고려
# 다변량 분석에 사용할 수치형 변수(age, bmi, glucose, blood_pressure)를 선택

patient_data_raw <- read.csv(text = "
id,gender,age,bmi,glucose,blood_pressure,smoking
1,M,34,23.1,98,132,Yes
2,F,29,20.4,NA,118,No
3,M,42,27.3,110,NA,Yes
4,F,37,22.1,95,124,No
5,M,50,28.9,130,138,No
6,F,45,24.0,115,142,Yes
7,M,28,26.5,105,120,No
8,F,33,21.0,99,116,No
9,M,40,27.9,NA,150,Yes
10,F,31,19.8,92,110,No
")

# 1. 다변량 분석용 데이터
multivar <- patient_data_raw[, c("age", "bmi", "glucose", "blood_pressure")]
multivar <- multivar[complete.cases(multivar), ]

# 2. Mahalanobis distance
center     <- colMeans(multivar)
cov_matrix <- cov(multivar)
md         <- mahalanobis(multivar, center, cov_matrix)

# 3. cutoff (상위 1%)
cutoff   <- qchisq(0.99, df = 4)
outliers <- which(md > cutoff)

# 4. 이상치 제거 (이상치 없으면 전체 사용)
if (length(outliers) == 0) {
        clean_multivar <- multivar
} else {
        clean_multivar <- multivar[-outliers, ]
}

nrow(clean_multivar)   # 여기서 7 나와야 합니다.

# 5. 상관관계 + 회귀
cor(clean_multivar$bmi, clean_multivar$glucose)

model <- lm(glucose ~ bmi, data = clean_multivar)
summary(model)

# lm(formula = glucose ~ bmi, data = clean_multivar)
# 
# Residuals:
#         1       4       5       6       7       8      10 
# -4.930  -4.285   5.925   8.789 -10.325   3.726   1.100 
# 
# Coefficients:
#         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  18.7164    22.8005   0.821   0.4491  
# bmi           3.6456     0.9576   3.807   0.0125 *
#         ---
#         Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.439 on 5 degrees of freedom
# Multiple R-squared:  0.7435,	Adjusted R-squared:  0.6922 
# F-statistic: 14.49 on 1 and 5 DF,  p-value: 0.01254

# BMI와 혈당(glucose) 간의 단순 선형회귀 분석 결과, BMI는 혈당을 유의하게 예측하는 변수로 나타났다(p = 0.0125). 회귀계수는 3.65로 BMI가 1 증가할 때 혈당은 약 3.65 증가하는 경향을 보였다. 모델의 설명력은 R² = 0.743(조정 R² = 0.692)로 비교적 높아 BMI는 혈당 변화에 중요한 영향을 주는 변수임을 확인할 수 있다.


# 해석
# 
# 이상치를 제거한 후 상관관계는 더 안정적이며 신뢰도 있는 분석이 가능하다.
# bmi가 증가할 때 glucose가 증가하는 양의 관계가 나타난다.
# 간단한 모델링으로도 건강 데이터의 경향성을 파악할 수 있다.