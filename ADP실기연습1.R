# ADP 실기연습 1

library(dplyr)
# sales_data.csv를 읽거나 아래와 같이 데이터를 가져온다.
sales_data <- read.csv(text = "
id,    gender,   region,  sales,  age,  join_date
 1,         M,    Seoul,    350,   34, 2020-01-03
 2,         F,    Busan,    420,   29, 2021-03-10
 3,         M,    Seoul,     NA,   42, 2019-11-18
 4,         F,    Daegu,    510,   37, 2020-08-22
 5,         M,    Busan,    305,   NA, 2021-02-14
 6,         F,  Gwangju,    290,   45, 2020-12-30
 7,         M,    Seoul,    410,   31, 2019-05-20
 8,         F,    Seoul,     NA,   28, 2020-04-10
 9,         M,    Busan,    380,   40, 2021-07-17
10,         F,    Daegu,    450,   33, 2019-09-30
", 
strip.white = TRUE)  # 이렇게 하면 공백 자동 제거
sales_data
# 문제 1. 결측치 처리 및 기본 통계량 생성
# 다음 요구사항을 만족하는 R 코드를 작성하시오.# 
# sales와 age 변수의 결측치를 **각 변수의 중앙값(Median)**으로 대체하라.# 
# gender별 평균 판매액(mean_sales)을 구하라.# 
# region별 판매액 합계(total_sales)를 구하라.# 
# join_date를 Date 형식으로 변환하고 회원 가입 연도(join_year)를 추가하라.

# 1. 결측치 처리 (Median)
# 중앙값 확인 후 결측치 처리
median(sales_data$sales, na.rm = TRUE)
# 395
sales_data$sales[is.na(sales_data$sales)] <- median(sales_data$sales, na.rm = TRUE)
median(sales_data$age, na.rm = TRUE)
# 34
sales_data$age[is.na(sales_data$age)] <- median(sales_data$age, na.rm = TRUE)
sales_data
# id gender  region sales age  join_date
# 1   1      M   Seoul   350  34 2020-01-03
# 2   2      F   Busan   420  29 2021-03-10
# 3   3      M   Seoul   395  42 2019-11-18
# 4   4      F   Daegu   510  37 2020-08-22
# 5   5      M   Busan   305  34 2021-02-14
# 6   6      F Gwangju   290  45 2020-12-30
# 7   7      M   Seoul   410  31 2019-05-20
# 8   8      F   Seoul   395  28 2020-04-10
# 9   9      M   Busan   380  40 2021-07-17
# 10 10      F   Daegu   450  33 2019-09-30
# 해석 : 결측치가 잘 대체 되었다.

# 2. gender별 평균 판매액
gender_sales <- sales_data %>% 
        group_by(gender) %>% 
        summarise(mean_sales = mean(sales))
gender_sales
# gender mean_sales
# <chr>       <dbl>
# 1 F             413
# 2 M             368

# 3. region별 판매액 합계
region_sales <- sales_data %>% 
        group_by(region) %>% 
        summarise(total_sales = sum(sales)) %>% 
        arrange(desc(total_sales))
region_sales
# region  total_sales
# <chr>         <dbl>
# 1 Seoul          1550
# 2 Busan          1105
# 3 Daegu           960
# 4 Gwangju         290

# 4. join_date 변환 및 join_year 추가
sales_data <- sales_data %>%
        mutate(join_date = as.Date(join_date),
               join_year = format(join_date, "%Y"))
sales_data
# id gender  region sales age  join_date    join_year
# 1   1      M   Seoul   350  34 2020-01-03      2020
# 2   2      F   Busan   420  29 2021-03-10      2021
# 3   3      M   Seoul   395  42 2019-11-18      2019
# 4   4      F   Daegu   510  37 2020-08-22      2020
# 5   5      M   Busan   305  34 2021-02-14      2021
# 6   6      F Gwangju   290  45 2020-12-30      2020
# 7   7      M   Seoul   410  31 2019-05-20      2019
# 8   8      F   Seoul   395  28 2020-04-10      2020
# 9   9      M   Busan   380  40 2021-07-17      2021
# 10 10      F   Daegu   450  33 2019-09-30      2019

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

library(dplyr)
Q1 <- quantile(sales_data$sales, 0.25)
Q1
# 25% 
# 357.5 
Q3 <- quantile(sales_data$sales, 0.75)
Q3
# 75% 
# 417.5
IQR_value <- Q3 - Q1
IQR_value
# 75% 
# 60 

lower <- Q1 - 1.5 * IQR_value
lower
# 25% 
# 267.5
upper <- Q3 + 1.5 * IQR_value
upper
# 75% 
# 507.5
sales_data
# id gender  region sales age  join_date
# 1   1      M   Seoul   350  34 2020-01-03
# 2   2      F   Busan   420  29 2021-03-10
# 3   3      M   Seoul   395  42 2019-11-18
# 4   4      F   Daegu   510  37 2020-08-22
# 5   5      M   Busan   305  34 2021-02-14
# 6   6      F Gwangju   290  45 2020-12-30
# 7   7      M   Seoul   410  31 2019-05-20
# 8   8      F   Seoul   395  28 2020-04-10
# 9   9      M   Busan   380  40 2021-07-17
# 10 10      F   Daegu   450  33 2019-09-30
nrow(sales_data)
# 10
clean_data <- sales_data %>% 
        filter(sales >= lower & sales <= upper)
clean_data
# id gender  region sales age  join_date
# 1  1      M   Seoul   350  34 2020-01-03
# 2  2      F   Busan   420  29 2021-03-10
# 3  3      M   Seoul   395  42 2019-11-18
# 4  5      M   Busan   305  34 2021-02-14
# 5  6      F Gwangju   290  45 2020-12-30
# 6  7      M   Seoul   410  31 2019-05-20
# 7  8      F   Seoul   395  28 2020-04-10
# 8  9      M   Busan   380  40 2021-07-17
# 9 10      F   Daegu   450  33 2019-09-30
nrow(clean_data)
# 9

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
sales_data
# id gender  region sales age  join_date age_group sales_per_age
# 1   1      M   Seoul   350  34 2020-01-03    Middle     10.294118
# 2   2      F   Busan   420  29 2021-03-10     Young     14.482759
# 3   3      M   Seoul   395  42 2019-11-18    Senior      9.404762
# 4   4      F   Daegu   510  37 2020-08-22    Middle     13.783784
# 5   5      M   Busan   305  34 2021-02-14    Middle      8.970588
# 6   6      F Gwangju   290  45 2020-12-30    Senior      6.444444
# 7   7      M   Seoul   410  31 2019-05-20    Middle     13.225806
# 8   8      F   Seoul   395  28 2020-04-10     Young     14.107143
# 9   9      M   Busan   380  40 2021-07-17    Middle      9.500000
# 10 10      F   Daegu   450  33 2019-09-30    Middle     13.636364

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
", strip.white = TRUE)
health_data

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
# mean_age   sd_age median_age mean_height sd_height median_height mean_weight sd_weight median_weight
# 1     36.9 7.248755       35.5       169.9  8.504247           170        71.6   14.3929            73
# mean_bp    sd_bp median_bp
# 1   129.5 13.78606       128

# 1-2. gender별 평균 혈압
bp_by_gender <- health_data %>%
        group_by(gender) %>%
        summarise(mean_bp = mean(blood_pressure))
bp_by_gender
# gender mean_bp
# <chr>    <dbl>
# 1 F          122
# 2 M          137

# 1-3. smoking 여부에 따른 평균 체중
weight_by_smoking <- health_data %>%
        group_by(smoking) %>%
        summarise(mean_weight = mean(weight))
weight_by_smoking
smoking mean_weight  smoking mean_weight
# <chr>         <dbl>
# 1 No             64.7
# 2 Yes            82 

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
# age height weight blood_pressure
# 1   34    175     78            132
# 2   29    162     55            118
# 3   42    180     92            145
# 4   37    168     62            124
# 5   50    172     85            138
# 6   45    158     70            142
# 7   28    181     76            120
# 8   33    165     58            116
# 9   40    178     88            150
# 10  31    160     52            110
cor_matrix <- cor(numeric_vars)
cor_matrix
# age     height    weight blood_pressure
# age            1.00000000 0.06110235 0.5949038      0.7677459
# height         0.06110235 1.00000000 0.7966543      0.4354786
# weight         0.59490378 0.79665435 1.0000000      0.8567623
# blood_pressure 0.76774587 0.43547862 0.8567623      1.0000000

# 3-2. Heatmap
# install.packages("reshape2")
library(reshape2)

cor_melt <- melt(cor_matrix)
cor_melt
# Var1           Var2      value
# 1             age            age 1.00000000
# 2          height            age 0.06110235
# 3          weight            age 0.59490378
# 4  blood_pressure            age 0.76774587
# 5             age         height 0.06110235
# 6          height         height 1.00000000
# 7          weight         height 0.79665435
# 8  blood_pressure         height 0.43547862
# 9             age         weight 0.59490378
# 10         height         weight 0.79665435
# 11         weight         weight 1.00000000
# 12 blood_pressure         weight 0.85676229
# 13            age blood_pressure 0.76774587
# 14         height blood_pressure 0.43547862
# 15         weight blood_pressure 0.85676229
# 16 blood_pressure blood_pressure 1.00000000
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
", strip.white = TRUE)
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
", strip.white = TRUE)

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
# cust_id name gender  join_date product amount purchase_date
# 1       1  Kim      M 2020-01-03       A    300    2020-02-10
# 2       1  Kim      M 2020-01-03       B    150    2020-02-15
# 3       2  Lee      F 2021-03-10       B    200    2021-04-01
# 4       3 Park      M 2019-11-18       A    500    2020-01-10
# 5       3 Park      M 2019-11-18       C    250    2020-01-15
# 6       4 Choi      F 2020-08-22       B    120    2020-09-01
# 7       4 Choi      F 2020-08-22       C    330    2020-09-15
# 8       5 Jung      M 2021-02-14       A    420    2021-03-01

# 1-2. Left Join
left_join_data <- customer_info %>%
        left_join(purchase_log, by = "cust_id")
left_join_data
# cust_id name gender  join_date product amount purchase_date
# 1       1  Kim      M 2020-01-03       A    300    2020-02-10
# 2       1  Kim      M 2020-01-03       B    150    2020-02-15
# 3       2  Lee      F 2021-03-10       B    200    2021-04-01
# 4       3 Park      M 2019-11-18       A    500    2020-01-10
# 5       3 Park      M 2019-11-18       C    250    2020-01-15
# 6       4 Choi      F 2020-08-22       B    120    2020-09-01
# 7       4 Choi      F 2020-08-22       C    330    2020-09-15
# 8       5 Jung      M 2021-02-14       A    420    2021-03-01

# 1-3. 구매 내역이 없는 고객 찾기
no_purchase <- left_join_data %>%
        filter(is.na(amount))
no_purchase
# [1] cust_id       name          gender        join_date     product       amount        purchase_date
# <0 행> <또는 row.names의 길이가 0입니다>

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
# cust_id purchase_date     A     B     C
# <int> <chr>         <int> <int> <int>
#   1       1 2020-02-10      300     0     0
# 2       1 2020-02-15        0   150     0
# 3       2 2021-04-01        0   200     0
# 4       3 2020-01-10      500     0     0
# 5       3 2020-01-15        0     0   250
# 6       4 2020-09-01        0   120     0
# 7       4 2020-09-15        0     0   330
# 8       5 2021-03-01      420     0     0

# 2-2. 고객별 총 구매액
total_purchase <- purchase_log %>%
        group_by(cust_id) %>%
        summarise(total_amount = sum(amount))
total_purchase
# cust_id total_amount
# <int>        <int>
# 1       1          450
# 2       2          200
# 3       3          750
# 4       4          450
# 5       5          420

# 2-3. 총 구매액 400 이상 고객
high_value <- total_purchase %>%
        filter(total_amount >= 400)
high_value
# cust_id total_amount
# <int>        <int>
# 1       1          450
# 2       3          750
# 3       4          450
# 4       5          420

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
# cust_id name gender  join_date initial name_length
# 1       1  Kim      M 2020-01-03       K           3
# 2       2  Lee      F 2021-03-10       L           3
# 3       3 Park      M 2019-11-18       P           4
# 4       4 Choi      F 2020-08-22       C           4
# 5       5 Jung      M 2021-02-14       J           4

# 3-2.
purchase_log <- purchase_log %>%
        mutate(product = str_to_lower(product))
purchase_log
# cust_id product amount purchase_date
# 1       1       a    300    2020-02-10
# 2       1       b    150    2020-02-15
# 3       2       b    200    2021-04-01
# 4       3       a    500    2020-01-10
# 5       3       c    250    2020-01-15
# 6       4       b    120    2020-09-01
# 7       4       c    330    2020-09-15
# 8       5       a    420    2021-03-01

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
# cust_id name gender  join_date
# 1       1  Kim      M 2020-01-03
# 2       2  Lee      F 2021-03-10
# 3       3 Park      M 2019-11-18
# 4       4 Choi      F 2020-08-22
# 5       5 Jung      M 2021-02-14

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
# cust_id product amount purchase_date purchase_year purchase_month day_diff
# 1       1       a    300    2020-02-10          2020             02       38
# 2       1       b    150    2020-02-15          2020             02       43
# 3       2       b    200    2021-04-01          2021             04       22
# 4       3       a    500    2020-01-10          2020             01       53
# 5       3       c    250    2020-01-15          2020             01       58
# 6       4       b    120    2020-09-01          2020             09       10
# 7       4       c    330    2020-09-15          2020             09       24
# 8       5       a    420    2021-03-01          2021             03       15

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
", strip.white = TRUE)

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
# id gender   age   bmi glucose blood_pressure smoking
# <int> <chr>  <int> <dbl>   <dbl>          <int> <chr>  
# 1     1 M         34  23.1     98             132 Yes    
# 2     2 F         29  20.4    100.            118 No     
# 3     3 M         42  27.3    110              NA Yes    
# 4     4 F         37  22.1     95             124 No     
# 5     5 M         50  28.9    130             138 No     
# 6     6 F         45  24      115             142 Yes    
# 7     7 M         28  26.5    105             120 No     
# 8     8 F         33  21       99             116 No     
# 9     9 M         40  27.9    111.            150 Yes    
# 10    10 F         31  19.8     92             110 No


# 1-2. age 그룹 생성
patient_data <- patient_data %>%
        mutate(age_group = case_when(
                age < 30 ~ "Young",
                age <= 40 ~ "Middle",
                TRUE ~ "Senior"
        ))
patient_data
# id gender   age   bmi glucose blood_pressure smoking age_group
# <int> <chr>  <int> <dbl>   <dbl>          <int> <chr>   <chr>    
# 1     1 M         34  23.1     98             132 Yes     Middle   
# 2     2 F         29  20.4    100.            118 No      Young    
# 3     3 M         42  27.3    110              NA Yes     Senior   
# 4     4 F         37  22.1     95             124 No      Middle   
# 5     5 M         50  28.9    130             138 No      Senior   
# 6     6 F         45  24      115             142 Yes     Senior   
# 7     7 M         28  26.5    105             120 No      Young    
# 8     8 F         33  21       99             116 No      Middle   
# 9     9 M         40  27.9    111.            150 Yes     Middle   
# 10    10 F         31  19.8     92             110 No      Middle 

# 1-3. blood_pressure: age_group별 평균 대체
patient_data <- patient_data %>%
        group_by(age_group) %>%
        mutate(blood_pressure = ifelse(is.na(blood_pressure),
                                       mean(blood_pressure, na.rm = TRUE),
                                       blood_pressure)) %>%
        ungroup()
patient_data
# id gender   age   bmi glucose blood_pressure smoking age_group
# <int> <chr>  <int> <dbl>   <dbl>          <dbl> <chr>   <chr>    
# 1     1 M         34  23.1     98             132 Yes     Middle   
# 2     2 F         29  20.4    100.            118 No      Young    
# 3     3 M         42  27.3    110             140 Yes     Senior   
# 4     4 F         37  22.1     95             124 No      Middle   
# 5     5 M         50  28.9    130             138 No      Senior   
# 6     6 F         45  24      115             142 Yes     Senior   
# 7     7 M         28  26.5    105             120 No      Young    
# 8     8 F         33  21       99             116 No      Middle   
# 9     9 M         40  27.9    111.            150 Yes     Middle   
# 10    10 F         31  19.8     92             110 No      Middle

# 1-4. 결측치 확인
colSums(is.na(patient_data))
# id         gender            age            bmi        glucose blood_pressure        smoking 
# 0              0              0              0              0              0              0 
# age_group 
# 0

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
",strip.white = TRUE)
patient_data_raw
# install.packages("VIM")
library(VIM)

knn_data <- kNN(patient_data_raw[, c("age", "bmi", "glucose", "blood_pressure")],
                k = 3)  # 수치형 변수는 가까운 이웃 3명의 평균값으로 대체
# age            bmi blood_pressure            age            bmi blood_pressure 
# 28.0           19.8          110.0           50.0           28.9          150.0 
# age     bmi glucose     age     bmi glucose 
# 28.0    19.8    92.0    50.0    28.9   130.0
knn_data
# age  bmi glucose blood_pressure age_imp bmi_imp glucose_imp blood_pressure_imp
# 1   34 23.1      98            132   FALSE   FALSE       FALSE              FALSE
# 2   29 20.4      95            118   FALSE   FALSE        TRUE              FALSE
# 3   42 27.3     110            142   FALSE   FALSE       FALSE               TRUE
# 4   37 22.1      95            124   FALSE   FALSE       FALSE              FALSE
# 5   50 28.9     130            138   FALSE   FALSE       FALSE              FALSE
# 6   45 24.0     115            142   FALSE   FALSE       FALSE              FALSE
# 7   28 26.5     105            120   FALSE   FALSE       FALSE              FALSE
# 8   33 21.0      99            116   FALSE   FALSE       FALSE              FALSE
# 9   40 27.9     115            150   FALSE   FALSE        TRUE              FALSE
# 10  31 19.8      92            110   FALSE   FALSE       FALSE              FALSE

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
",strip.white = TRUE)

# 1. 다변량 분석용 데이터
multivar <- patient_data_raw[, c("age", "bmi", "glucose", "blood_pressure")]
multivar
# age  bmi glucose blood_pressure
# 1   34 23.1      98            132
# 4   37 22.1      95            124
# 5   50 28.9     130            138
# 6   45 24.0     115            142
# 7   28 26.5     105            120
# 8   33 21.0      99            116
# 10  31 19.8      92            110
multivar <- multivar[complete.cases(multivar), ]
multivar
# age  bmi glucose blood_pressure
# 1   34 23.1      98            132
# 4   37 22.1      95            124
# 5   50 28.9     130            138
# 6   45 24.0     115            142
# 7   28 26.5     105            120
# 8   33 21.0      99            116
# 10  31 19.8      92            110
# 2. Mahalanobis distance
center     <- colMeans(multivar)
center
# age            bmi        glucose blood_pressure 
# 36.85714       23.62857      104.85714      126.00000 
cov_matrix <- cov(multivar)
cov_matrix
# age      bmi   glucose blood_pressure
# age            62.47619 13.45476  87.97619       76.00000
# bmi            13.45476 10.05905  36.67143       23.23333
# glucose        87.97619 36.67143 179.80952      117.66667
# blood_pressure 76.00000 23.23333 117.66667      138.66667
md         <- mahalanobis(multivar, center, cov_matrix)
md
# 1        4        5        6        7        8       10 
# 2.573672 3.955764 4.623188 3.695205 4.506185 2.092065 2.553921

# 3. cutoff (상위 1%)
cutoff   <- qchisq(0.99, df = 4)
cutoff
# 13.2767
outliers <- which(md > cutoff)
outliers
# named integer(0)

# 4. 이상치 제거 (이상치 없으면 전체 사용)
if (length(outliers) == 0) {
        clean_multivar <- multivar
} else {
        clean_multivar <- multivar[-outliers, ]
}

clean_multivar
# age  bmi glucose blood_pressure
# 1   34 23.1      98            132
# 4   37 22.1      95            124
# 5   50 28.9     130            138
# 6   45 24.0     115            142
# 7   28 26.5     105            120
# 8   33 21.0      99            116
# 10  31 19.8      92            110
nrow(clean_multivar)   # 여기서 7 나와야 합니다.
# 7

# 5. 상관관계 + 회귀
cor(clean_multivar$bmi, clean_multivar$glucose)
# 0.8622696
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

# BMI와 혈당(glucose) 간의 단순 선형회귀 분석 결과, 
# BMI는 혈당을 유의하게 예측하는 변수로 나타났다(p = 0.0125). 
# 회귀계수는 3.65로 BMI가 1 증가할 때 혈당은 약 3.65 증가하는 경향을 보였다. 
# 모델의 설명력은 R² = 0.743(조정 R² = 0.692)로 비교적 높아 BMI는 
# 혈당 변화에 중요한 영향을 주는 변수임을 확인할 수 있다.

# 해석
# 
# 이상치를 제거한 후 상관관계는 더 안정적이며 신뢰도 있는 분석이 가능하다.
# bmi가 증가할 때 glucose가 증가하는 양의 관계가 나타난다.
# 간단한 모델링으로도 건강 데이터의 경향성을 파악할 수 있다.



# ADP 실기연습 5

survey_data <- read.csv(text = "
id,gender,age_group,smoking,exercise,hypertension
1,M,Young,Yes,Low,No
2,F,Middle,No,High,No
3,M,Senior,Yes,Low,Yes
4,F,Middle,No,Medium,No
5,M,Young,No,High,No
6,F,Senior,Yes,Medium,Yes
7,M,Middle,No,Low,No
8,F,Young,No,High,No
9,M,Senior,Yes,Low,Yes
10,F,Middle,No,Medium,No
", strip.white = TRUE)
  
# 문제 1. 교차표(Cross Table) 생성 및 비율 확인
# 
# 요구사항
# gender × smoking 교차표를 생성하라.
# 행 비율(row proportion)과 열 비율(column proportion)을 계산하라.
# smoking 비율이 가장 높은 그룹을 해석하라.                      

# 1-1. 교차표
tab1 <- table(survey_data$gender, survey_data$smoking)
tab1
# No Yes
# F  4   1
# M  2   3

# 1-2. 비율 계산
row_prop <- prop.table(tab1, 1)  # 행 비율
row_prop
# No Yes
# F 0.8 0.2
# M 0.4 0.6
col_prop <- prop.table(tab1, 2)  # 열 비율
col_prop
# No       Yes
# F 0.6666667 0.2500000
# M 0.3333333 0.7500000

# 해석
# 
# 남성(M)의 흡연 비율이 여성(F)보다 높게 나타난다
# 행 기준 비율(row proportion)은 각 성별 그룹 내부의 smoking 비율을 보여준다.
# 열 기준 비율(column proportion)은 smoking 상태에서 남녀 비율을 나타낸다.


# 문제 2. 카이제곱 검정(Chi-square Test)
# 
# 요구사항
# smoking과 hypertension(고혈압)의 독립성을 검정하라.
# 유의수준 0.05 기준으로 관계가 유의한지 판단하라.
# 결과를 보고서 형식으로 서술하라.

# 카이제곱검정 : **“기대한 것과 실제 결과가 얼마나 다른지 비교하는 검사”**입니다.
# 특히 **범주형 데이터(예: 남/여, 예/아니오, 선호/비선호)**가 기대한 비율과 일치하는지 판단할 때 사용합니다.

tab2 <- table(survey_data$smoking, survey_data$hypertension)
chi_result <- chisq.test(tab2)
chi_result
# Pearson's Chi-squared test with Yates' continuity correction
# data:  tab2
# X-squared = 3.3532, df = 1, p-value = 0.06708

# 해석
# p-value가 0.05보다 작다면 smoking과 hypertension 간에는 통계적으로 유의한 연관성이 존재한다.
# p-value가 크다면 두 변수는 서로 독립적(independent)이라고 판단한다.
# Chi-square 검정은 ADP 실기에서 가장 중요한 범주형 검정이다.


# 문제 3. 그룹별 비율 비교
# 
# 요구사항
# exercise 수준(Medium, High, Low)에 따른 hypertension 비율을 계산하라.
# hypertension 비율이 가장 높은 운동 수준을 파악하라.
# 기준 비율 대비 상대 위험도(RR)를 계산하라.

# 3-1. 그룹별 hypertension 비율
rate_table <- survey_data %>%
  group_by(exercise) %>%
  summarise(hyper_rate = mean(hypertension == "Yes"))
rate_table
# exercise hyper_rate
# <chr>         <dbl>
#   1 High          0    
# 2 Low           0.5  
# 3 Medium        0.333

# 3-2. 상대 위험도 계산 (Low를 기준)
baseline <- rate_table$hyper_rate[rate_table$exercise == "Low"]
rate_table$RR <- rate_table$hyper_rate / baseline
rate_table
# exercise hyper_rate    RR
# <chr>         <dbl> <dbl>
#   1 High          0     0    
# 2 Low           0.5   1    
# 3 Medium        0.333 0.667

# 해석
# 
# Low 운동 그룹에서 고혈압 비율이 가장 높게 나타날 가능성이 있다.
# RR(상대 위험도)을 통해 운동 수준별 건강 위험도를 정량적으로 평가할 수 있다.
# 이러한 비율 비교 문제는 건강·의학 분야 데이터에서 매우 자주 등장한다.



# 문제 4. 로지스틱 회귀(Logistic Regression)를 이용한 범주형 분석
# 
# 요구사항
# 
# hypertension을 종속변수로, age_group, smoking, exercise를 독립변수로 하는 로지스틱 회귀모델을 적합하라.
# 각 변수의 회귀계수를 해석하라.
# smoking(Yes)이 hypertension Yes의 가능성을 얼마나 증가시키는지 오즈비(OR)를 계산하라.

# factor 변환 : 모델이 범주형 값을 숫자가 아닌 “범주”로 인식하도록 만들기 위해서
survey_data
# id gender age_group smoking exercise hypertension
# 1   1      M     Young     Yes      Low           No
# 2   2      F    Middle      No     High           No
# 3   3      M    Senior     Yes      Low          Yes
# 4   4      F    Middle      No   Medium           No
# 5   5      M     Young      No     High           No
# 6   6      F    Senior     Yes   Medium          Yes
# 7   7      M    Middle      No      Low           No
# 8   8      F     Young      No     High           No
# 9   9      M    Senior     Yes      Low          Yes
# 10 10      F    Middle      No   Medium           No
survey_data$hypertension <- as.factor(survey_data$hypertension)
survey_data$smoking <- as.factor(survey_data$smoking)
survey_data$age_group <- as.factor(survey_data$age_group)
survey_data$exercise <- as.factor(survey_data$exercise)
survey_data
# id gender age_group smoking exercise hypertension
# 1   1      M     Young     Yes      Low           No
# 2   2      F    Middle      No     High           No
# 3   3      M    Senior     Yes      Low          Yes
# 4   4      F    Middle      No   Medium           No
# 5   5      M     Young      No     High           No
# 6   6      F    Senior     Yes   Medium          Yes
# 7   7      M    Middle      No      Low           No
# 8   8      F     Young      No     High           No
# 9   9      M    Senior     Yes      Low          Yes
# 10 10      F    Middle      No   Medium           No
# 4-1. 로지스틱 회귀모델
model <- glm(hypertension ~ age_group + smoking + exercise,
             data = survey_data, family = binomial)
summary(model)
# Call:
#   glm(formula = hypertension ~ age_group + smoking + exercise, 
#       family = binomial, data = survey_data)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)     -2.457e+01  1.310e+05       0        1
# age_groupSenior  4.913e+01  2.237e+05       0        1
# age_groupYoung  -2.087e-14  1.605e+05       0        1
# smokingYes       4.620e-14  2.331e+05       0        1
# exerciseLow     -2.087e-14  1.691e+05       0        1
# exerciseMedium  -2.087e-14  1.559e+05       0        1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1.2217e+01  on 9  degrees of freedom
# Residual deviance: 4.2867e-10  on 4  degrees of freedom
# AIC: 12
# 
# Number of Fisher Scoring iterations: 23

# 4-2. 오즈비 계산
exp(coef(model))
# (Intercept) age_groupSenior  age_groupYoung      smokingYes     exerciseLow  exerciseMedium 
# 2.143345e-11    2.176788e+21    1.000000e+00    1.000000e+00    1.000000e+00    1.000000e+00 


# 해석
# 
# exp(coef()) 값은 각 변수의 오즈비(odds ratio)를 의미한다.
# smoking = Yes의 OR > 1이면 흡연자가 비흡연자보다 고혈압 위험이 더 높다는 뜻이다.
# age_group 중 Senior가 기준 그룹(Young)보다 OR이 크다면 나이가 많을수록 고혈압 위험이 증가함을 의미한다.