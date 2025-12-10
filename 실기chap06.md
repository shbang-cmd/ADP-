Chapter 06 (Full Markdown Version)**
# Chapter 06. 연속형 변수 분석 (ANOVA · t-test · Correlation)

본 장에서는 ADP 실기에서 반드시 등장하는 **연속형 통계 분석** 문제들을 실전처럼 다룬다.  
주요 분석 항목은 다음과 같다.

- 정규성 검정 (Shapiro-Wilk)  
- 두 그룹 평균 비교 (t-test)  
- 세 그룹 평균 비교 (ANOVA)  
- 상관분석 (Correlation)  
- 시각화를 통한 관계 해석  

---

## 📂 데이터셋 (blood_test.csv)

```r
blood <- read.csv(text = "
id,gender,group,glucose,cholesterol
1,M,A,95,180
2,F,A,88,170
3,M,B,110,210
4,F,B,105,190
5,M,C,130,240
6,F,C,120,230
7,M,A,100,185
8,F,B,115,205
9,M,C,140,260
10,F,A,92,175
")

문제 1. 정규성 검정 (Shapiro-Wilk)

요구사항

glucose 변수의 정규성을 검정하라.

p-value 기준으로 정규성을 해석하라.

🔧 R 코드
shapiro.test(blood$glucose)

📊 해석

p-value < 0.05 → 정규성을 만족하지 않음

p-value ≥ 0.05 → 정규성을 만족함

정규성은 평균 비교(t-test, ANOVA)의 전제 조건이므로 반드시 검정해야 한다.

문제 2. 그룹 간 평균 비교 (t-test)

요구사항

gender(M/F)에 따른 glucose 평균 차이가 있는지 t-test로 검정하라.

등분산 여부는 var.test() 결과를 사용하여 결정하라.

🔧 R 코드
# 등분산 검정
var.test(glucose ~ gender, data = blood)

# 성별 간 t-test
t.test(glucose ~ gender, data = blood, var.equal = TRUE)

📊 해석

p-value < 0.05 → 성별에 따라 glucose 평균 값이 유의하게 다름

p-value ≥ 0.05 → 유의한 차이가 없음

var.test() 결과에 따라 var.equal 인자를 TRUE/FALSE로 설정해야 한다.

문제 3. 세 그룹 평균 비교 (ANOVA)

요구사항

group(A/B/C) 간 cholesterol 평균 차이를 ANOVA로 검정하라.

유의하면 사후검정(TukeyHSD)을 수행하라.

🔧 R 코드
anova_model <- aov(cholesterol ~ group, data = blood)
summary(anova_model)

# 사후검정
TukeyHSD(anova_model)

📊 해석

ANOVA에서 p-value < 0.05 → 세 그룹 중 적어도 하나는 평균이 다름

TukeyHSD 결과에서 어떤 그룹 간 차이가 유의한지 확인할 수 있다.

이 문제는 ADP 실기에서 매우 자주 출제된다.

문제 4. 상관분석 (Correlation)

요구사항

glucose와 cholesterol 간 상관계수를 계산하라.

산점도 + 회귀선으로 시각화하라.

🔧 R 코드
cor(blood$glucose, blood$cholesterol)

library(ggplot2)
ggplot(blood, aes(glucose, cholesterol)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

📊 해석

r > 0.7 → 강한 양의 상관

0.3 < r < 0.7 → 중간 정도 상관

r < 0.3 → 약한 상관

r < 0 → 음의 상관

상관분석은 단독으로도 출제되며 회귀 분석 문제와도 연결된다.

문제 5. 상관행렬(Correlation Matrix) 생성

요구사항

glucose, cholesterol만이 아니라 age, weight 등의 추가 변수가 있을 경우
상관행렬로 전체 관계를 한 번에 분석하라.

heatmap으로 시각화하라.

(현재 데이터셋에는 없는 변수이므로 예시 코드 제공)

🔧 R 코드 (예시)
num_vars <- blood[, c("glucose", "cholesterol")]
cor_matrix <- cor(num_vars)

library(reshape2)
library(ggplot2)

melted <- melt(cor_matrix)

ggplot(melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="white") +
  ggtitle("Correlation Heatmap")

✔ Chapter 06 요약

Shapiro-Wilk 정규성 검정은 연속형 분석의 기본 전제이다.

t-test는 두 그룹 평균 비교, ANOVA는 세 그룹 이상 비교에 사용된다.

사후검정(Tukey)은 ANOVA 유의 시 필수이다.

상관분석은 r의 크기와 부호를 해석할 수 있어야 한다.

모두 ADP 실기에서 반복적으로 출제되는 핵심 유형이다.



Chapter 07. 시계열 분석 기초 (Time Series Basics)**
# Chapter 07. 시계열 분석 기초 (Time Series Basics)

본 장에서는 ADP 실기에서 반복적으로 등장하는 **시계열(Time Series)** 문제를 다룬다.  
핵심 내용은 다음과 같다.

- 시계열 객체 생성  
- 추세(Trend)와 계절성(Seasonality) 분해  
- ACF/PACF  
- ARIMA 기본 모델  
- STL 분해  
- 시계열 예측  

시계열은 실기에서 단독 문제로 출제되거나 ARIMA 모델링과 함께 출제되는 중요한 영역이다.

---

## 📂 데이터셋 (monthly_sales.csv)

```r
ts_data <- read.csv(text = "
month,sales
2020-01,120
2020-02,130
2020-03,128
2020-04,150
2020-05,160
2020-06,155
2020-07,170
2020-08,175
2020-09,168
2020-10,180
2020-11,185
2020-12,200
")

📌 시계열 객체 변환
library(zoo)
ts_data$month <- as.yearmon(ts_data$month)
ts <- ts(ts_data$sales, frequency = 12)

문제 1. 시계열 분해(Decomposition)

요구사항

ts 객체를 추세·계절성·불규칙 요소로 분해하라.

분해 결과를 시각화하라.

🔧 R 코드
decomp <- decompose(ts)
plot(decomp)

📊 해석

Trend: 장기적 증가/감소 패턴

Seasonal: 월별 반복되는 패턴

Random: 예측 불가능한 오차

분해 결과를 보면 sales가 꾸준히 증가 추세를 가진 것을 확인할 수 있다.

문제 2. ACF / PACF 분석

요구사항

시계열의 ACF(자기상관함수)를 그려라.

PACF(편자기상관함수)를 그려라.

AR/MA 모델 차수를 추정하라.

🔧 R 코드
acf(ts)
pacf(ts)

📊 해석

ACF가 천천히 감소 → AR 모델

PACF가 천천히 감소 → MA 모델

특정 lag에서 급격히 끊기는 지점 → 차수 결정의 근거

이 해석은 ARIMA 모델링에 필수적이다.

문제 3. ARIMA 모델 자동 적합

요구사항

auto.arima를 이용해 최적 ARIMA 모델을 적합하라.

향후 6개월을 예측하라.

예측 그래프를 그려라.

🔧 R 코드
library(forecast)

model <- auto.arima(ts)
model

future <- forecast(model, 6)
plot(future)

📊 해석

auto.arima는 AIC 기준으로 최적 모델을 자동 선택한다.

예측 결과는 “Point Forecast + 80/95% Interval” 형태로 출력된다.

실기에서는 모델 적합 결과 해석 문제도 자주 등장한다.

문제 4. STL 분해 (Seasonal-Trend Decomposition)

요구사항

STL 분해를 수행하라(s.window = "periodic").

Seasonality가 강한지 판단하라.

🔧 R 코드
stl_model <- stl(ts, s.window = "periodic")
plot(stl_model)

📊 해석

STL 분해 결과 Seasonal 패턴이 일정하거나 강하게 나타나면 계절성이 뚜렷하다고 본다.

STL은 decompose보다 현대적인 분해 방식이며 실기에서 자주 사용된다.

문제 5. 구조 변화 탐지 (Change Point Detection)

요구사항

구조적 변화 시점을 감지하라.

매출 패턴이 변한 시점을 보고하라.

🔧 R 코드
library(strucchange)
bp <- breakpoints(ts ~ 1)
bp
plot(bp)

📊 해석

breakpoints()는 시계열이 급격히 변하는 시점을 탐지한다.

실기에서 “어느 달 이후로 패턴이 변했는가?”와 같은 문제에 활용된다.

문제 6. 로그변환 + 차분(Differencing)

요구사항

sales에 로그 변환을 적용하라.

1차 차분을 수행하라.

차분 후 ACF/PACF를 다시 그려라.

🔧 R 코드
log_ts <- log(ts)
diff_ts <- diff(log_ts)

acf(diff_ts)
pacf(diff_ts)

📊 해석

로그 변환은 분산 안정화에 효과적이다.

차분은 비정상성(non-stationarity)을 제거하기 위한 핵심 기법이다.

차분 후 ACF/PACF는 더 명확한 모형 판정을 가능하게 한다.

✔ Chapter 07 요약

시계열 분석의 핵심 요소는 추세, 계절성, 불규칙성, 정상성이다.

ACF/PACF는 ARIMA 모델 차수 선정의 필수 도구이다.

auto.arima는 실기에서 매우 유용하며 자주 사용된다.

STL 분해는 계절성 확인 및 이상 패턴 탐지에 활용된다.

구조 변화(breakpoint) 탐지는 고급 실기 문제로 빈출된다.

로그변환 + 차분은 정상성 확보에 가장 기본적인 절차이다.




Chapter 08. 군집 분석 (Cluster Analysis, K-means · Hierarchical Clustering)**
# Chapter 08. 군집 분석 (Cluster Analysis)

군집분석(Clustering)은 **비지도학습(Unsupervised Learning)**의 대표적인 기법이다.  
ADP 실기에서 자주 출제되는 핵심 분석이며, K-means · 계층적 군집화 · 실루엣 계수를 중점적으로 다룬다.

---

## 📂 데이터셋 (customer_seg.csv)

```r
cluster_data <- read.csv(text = "
id,age,spending_score,income
1,25,60,40
2,32,55,38
3,40,80,70
4,22,30,22
5,35,75,65
6,45,90,80
7,29,50,35
8,50,85,78
9,31,62,48
10,28,45,33
")

문제 1. 표준화(Standardization)

K-means는 변수 스케일에 민감하므로 표준화가 필수이다.

요구사항

age, spending_score, income 변수를 표준화하라.

표준화된 데이터를 new_data로 저장하라.

🔧 R 코드
new_data <- scale(cluster_data[, c("age", "spending_score", "income")])
head(new_data)

📊 해석

scale() 함수는 (x - 평균) / 표준편차 형태로 표준화를 수행한다.

변수 단위 차이를 제거하여 K-means의 성능을 향상시킨다.

문제 2. K-means 군집화 수행

요구사항

k = 3으로 K-means 군집화를 수행하라.

각 데이터의 군집 번호를 cluster 변수로 추가하라.

군집별 중심점(cluster center)을 출력하라.

🔧 R 코드
set.seed(123)
km <- kmeans(new_data, centers = 3)

cluster_data$cluster <- factor(km$cluster)
km$centers

📊 해석

cluster_data$cluster에 각 고객의 군집이 부여된다.

km$centers는 각 군집의 평균적 특성을 나타낸다.

예: 군집 1은 “젊고 소비 점수가 낮은 그룹”, 군집 3은 “소득·소비 모두 높은 그룹” 등.

문제 3. 군집 시각화

요구사항

age vs spending_score 산점도에 군집 색상을 표시하라.

점 크기는 income 값에 비례하도록 설정하라.

🔧 R 코드
library(ggplot2)

ggplot(cluster_data,
       aes(x = age, y = spending_score, color = cluster, size = income)) +
  geom_point(alpha = 0.7) +
  ggtitle("Customer Segmentation by K-means")

📊 해석

시각화를 통해 군집이 잘 분리되는지 직관적으로 확인할 수 있다.

income을 size로 표현하여 고객의 소득 패턴까지 함께 파악할 수 있다.

문제 4. 실루엣 계수(Silhouette Score)

군집 품질을 판단하는 가장 중요한 지표이다.

요구사항

silhouette() 함수를 이용해 평균 실루엣 계수를 계산하라.

실루엣 계수가 높을수록 군집 품질이 좋은 이유를 설명하라.

🔧 R 코드
library(cluster)
sil <- silhouette(km$cluster, dist(new_data))
mean_sil <- mean(sil[, 3])
mean_sil

📊 해석

실루엣 계수는 -1 ~ 1 범위를 가진다.

0.5 이상이면 군집이 잘 분리됨,

0 ~ 0.5는 보통

음수는 군집이 잘못 분류된 경우

문제 5. 계층적 군집 분석 (Hierarchical Clustering)

요구사항

거리행렬(distance matrix)을 구하라.

ward.D2 방법으로 계층적 군집을 수행하라.

덴드로그램(dendrogram)을 시각화하라.

3개의 군집으로 잘라 cutree 결과를 출력하라.

🔧 R 코드
d <- dist(new_data)

hc <- hclust(d, method = "ward.D2")

plot(hc, main = "Hierarchical Clustering (ward.D2)", xlab = "", sub = "")

cutree(hc, k = 3)

📊 해석

ward.D2 방식은 군집 내 분산을 최소화하여 안정적인 군집 생성이 가능하다.

덴드로그램의 절단 높이에 따라 적절한 군집 수를 직관적으로 확인할 수 있다.

문제 6. K-means vs Hierarchical Clustering 비교

요구사항

두 군집 방법의 장단점을 정리하라.

📊 해석
기법	장점	단점
K-means	속도가 빠름, 대규모 데이터 적합	초기 중심점에 민감, 구형(cluster shape) 가정
Hierarchical	덴드로그램으로 해석 용이, 군집 수 사전 지정 필요 없음	대규모 데이터에 비효율적

군집 분석 문제에서는 맥락에 맞는 기법 선택이 중요하다.

✔ Chapter 08 요약

K-means는 표준화(scale)가 필수이다.

실루엣 계수는 군집 품질을 평가하는 가장 중요한 지표다.

Hierarchical clustering은 해석이 용이하며 시각적으로 군집 판단이 가능하다.

군집 번호는 factor로 변환해 분석·시각화에 활용한다.

K-means와 Hierarchical을 비교하는 문제는 실기에서 자주 등장한다.




Chapter 09. 주성분 분석(PCA) · 차원축소(Dimension Reduction)**
# Chapter 09. 주성분 분석(PCA) · 차원축소

주성분 분석(PCA: Principal Component Analysis)은  
고차원 데이터를 **저차원으로 요약하여 핵심 구조를 파악하는 차원축소 방법**이다.

ADP 실기에서는 다음 항목이 자주 출제된다.

- 주성분 점수(PC score)  
- 주성분 적재값(loadings)  
- 고유값(eigenvalue)과 설명분산비율  
- Scree plot  
- Biplot 해석  
- 차원 축소 후 시각화  

---

## 📂 데이터셋 (quality_metrics.csv)

```r
quality <- read.csv(text = "
id,metric1,metric2,metric3,metric4
1,5,7,6,8
2,6,8,7,9
3,4,5,5,6
4,7,9,8,9
5,5,6,6,7
6,8,9,9,10
7,3,4,4,5
8,9,10,10,11
")


분석 대상 변수는 metric1 ~ metric4 이다.

문제 1. PCA 수행

요구사항

metric1~4 변수를 표준화한 후 PCA를 수행하라.

각 주성분의 설명 분산비율을 확인하라.

🔧 R 코드
pca <- prcomp(quality[, 2:5], scale = TRUE)
summary(pca)

📊 해석

summary()에서 다음을 확인한다:

Standard deviation → 각 주성분의 √고유값

Proportion of Variance → 각 주성분의 설명력

Cumulative Proportion → 누적 설명력

예:

PC1이 80% 이상 설명하면, PC1 하나만으로 데이터의 큰 구조를 설명 가능하다는 의미다.

문제 2. Scree Plot 그리기

고유값을 시각화하여 몇 개의 주성분을 유지할지 판단한다.

🔧 R 코드
plot(pca, type = "l", main = "Scree Plot")

📊 해석

꺾이는 지점(elbow point) 이후의 주성분은 설명력이 낮다.

Scree plot은 차원 축소 시 판단 근거로 매우 중요하다.

문제 3. Biplot으로 주성분 해석하기

Biplot은 **주성분 점수(개체)**와 **적재값(변수)**을 동시에 보여준다.

🔧 R 코드
biplot(pca, scale = 0)

📊 해석

화살표 방향이 비슷하면 해당 변수들은 양의 상관관계를 가진다.

화살표 길이가 길수록 해당 변수의 설명력이 크다.

PC1 방향으로 강하게 기여하는 변수가 무엇인지 확인 가능하다.

문제 4. 주성분 적재값(loadings) 출력 및 해석

요구사항

pca$rotation 값을 출력하라.

어떤 변수가 PC1에 가장 크게 기여하는지 해석하라.

🔧 R 코드
pca$rotation

📊 해석

PC1에서 값이 큰 변수 = 가장 중요한 설명 변수

부호는 방향을 의미함

변수 간 상관구조를 요약한 결과라고 볼 수 있다.

예: PC1 = 0.50metric1 + 0.52metric2 + 0.49metric3 + 0.49metric4
→ 모든 metric이 비슷하게 기여 → “종합 품질 점수”의 개념

문제 5. 주성분 점수(PC Score) 산출

요구사항

각 개체(ID)의 주성분 점수를 계산하라.

PC1 점수가 높은 개체의 특성을 해석하라.

🔧 R 코드
pca$x

📊 해석

pca$x는 각 데이터 포인트가 주성분 공간에서 어디에 위치하는지 나타낸다.

PC1 점수가 높다 → 변수들이 전체적으로 높은 값을 가지는 개체

PC2는 PC1로 설명되지 않는 변동성을 반영

문제 6. 차원 축소 후 시각화

요구사항

PC1과 PC2만 사용해 2차원 평면에 데이터를 시각화하라.

🔧 R 코드
library(ggplot2)

pc_df <- data.frame(ID = quality$id,
                    PC1 = pca$x[,1],
                    PC2 = pca$x[,2])

ggplot(pc_df, aes(PC1, PC2, label = ID)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text(vjust = -0.5) +
  ggtitle("PCA 2D Visualization")

📊 해석

PCA는 관측치를 저차원 공간에 자연스럽게 배치하여 패턴을 파악할 수 있게 한다.

가까이 위치한 점 → 유사한 특성을 가진 데이터

멀리 떨어진 점 → 서로 다른 특성

문제 7. 주성분 개수 선택 기준

실기에서 자주 묻는 개념:

기준	설명
누적 설명분산비율 80~90% 기준	가장 흔한 방식
Scree plot elbow point	고유값이 완만해지는 시점
Kaiser rule	고유값 > 1인 주성분만 선택
업무 도메인 기반 판단	실무에서는 가장 현실적인 기준
✔ Chapter 09 요약

PCA는 고차원 데이터의 구조를 요약하는 차원축소 기법이다.

prcomp(scale=TRUE)는 PCA 분석의 표준 절차이다.

Scree Plot, Biplot, Loadings 해석이 실기에서 매우 자주 등장한다.

PCA는 군집분석, 회귀분석 전처리에도 자주 활용된다.

주성분 점수는 새로운 축에서의 개체 위치를 의미하며 시각화에 유용하다.



Chapter 10. 회귀 분석(Regression Analysis)**
# Chapter 10. 회귀 분석 (Linear & Multiple Regression)

본 장에서는 ADP 실기에서 매우 자주 등장하는 **회귀분석(Regression)** 문제를 다룬다.

핵심 내용:

- 단순선형회귀(Simple Regression)
- 다중회귀(Multiple Regression)
- 회귀계수 해석
- 잔차 진단(Residual Analysis)
- 다중공선성(VIF)
- 상호작용항(Interaction Term)
- 모델 선택(AIC 기반 비교)

---

## 📂 데이터셋 (housing.csv)

```r
house <- read.csv(text = "
price,size,rooms,age,dist_center
200,55,2,20,8
250,70,3,15,6
300,80,3,10,5
150,40,1,30,12
400,100,4,5,3
350,90,3,7,4
180,50,2,25,10
220,60,2,18,7
270,75,3,12,6
330,85,3,9,4
")


변수 설명:

price: 집값 (단위: 백만원)

size: 평수

rooms: 방 개수

age: 건물 연식

dist_center: 도심까지 거리(km)

문제 1. 단순회귀 모델 (price ~ size)

요구사항

size를 독립변수로 하는 단순회귀모델을 적합하라.

회귀계수(기울기, 절편)의 의미를 해석하라.

🔧 R 코드
model1 <- lm(price ~ size, data = house)
summary(model1)

📊 해석

기울기(slope): size가 1 증가할 때 price가 얼마나 증가하는지 의미

절편(intercept): size = 0일 때의 이론적 price(실무적 의미는 약함)

R-squared: 모델 설명력

ADP 실기에서는 ‘회귀계수 해석’이 자주 출제됨.

문제 2. 다중회귀 (price ~ size + rooms + age + dist_center)

요구사항

4개 독립변수를 포함한 다중회귀모델을 적합하라.

유의한 변수만 골라 해석하라.

🔧 R 코드
model2 <- lm(price ~ size + rooms + age + dist_center, data = house)
summary(model2)

📊 해석

size의 계수 양수 → 평수가 클수록 가격 상승

age의 계수 음수 → 낡을수록 가격 하락

dist_center 음수 → 도심과 가까울수록 비쌈

p-value < 0.05인 변수는 통계적으로 유의함

문제 3. 잔차 진단 (Residual Diagnostics)

회귀모델 가정이 만족되지 않으면 모형 신뢰성이 떨어진다.

🔧 R 코드
par(mfrow=c(2,2))
plot(model2)

📊 주요 진단 항목

Residuals vs Fitted → 선형성 검증

Normal Q-Q → 잔차 정규성

Scale-Location → 등분산성

Residuals vs Leverage → 이상치(Outlier) 검출

문제 4. 다중공선성(VIF) 검사

VIF(Variance Inflation Factor)가 10을 넘으면 다중공선성이 강하다고 본다.

🔧 R 코드
library(car)
vif(model2)

📊 해석

VIF > 10 → 문제

VIF 5~10 → 주의

VIF < 5 → 무난

문제 5. 상호작용 항(Interaction Effect)

size와 rooms의 상호작용이 price에 미치는 영향을 분석하라.

🔧 R 코드
model_int <- lm(price ~ size * rooms, data = house)
summary(model_int)

📊 해석

size:rooms 항의 계수가 유의하면
**“size의 효과가 rooms에 따라 달라진다”**는 의미

상호작용은 실기에서 가끔 등장하는 고급 문제

문제 6. 모델 비교(AIC 기준)

AIC가 낮을수록 좋은 모델이다.

🔧 R 코드
AIC(model1, model2, model_int)

📊 해석

model2 또는 model_int가 가장 낮은 AIC를 가진다면 성능이 우수한 모델

AIC는 과적합을 방지하는 데 유용

문제 7. 예측하기

새로운 데이터에 대해 price를 예측하라.

🔧 R 코드
new_house <- data.frame(size=77, rooms=3, age=10, dist_center=6)
predict(model2, new_house)

📊 해석

예측된 price는 새로운 주택의 적정 가격을 의미한다.

predict()는 실기 답안에서 반드시 필요한 코드 중 하나.

문제 8. 회귀계수 신뢰구간

요구사항
회귀계수 95% 신뢰구간을 구하라.

🔧 R 코드
confint(model2)

📊 해석

신뢰구간에 0이 포함되지 않으면 “유의한 변수”로 볼 수 있다.

회귀계수 해석에 추가적인 통계적 근거 제공.

✔ Chapter 10 요약

회귀분석은 ADP 실기에서 가장 중요한 모델 중 하나이다.

단순·다중회귀, 회귀계수 해석, 잔차 진단, VIF는 반드시 숙지해야 한다.

상호작용 모델은 고급 문제지만 출제 가능성이 있다.

AIC 비교는 모델 선택의 핵심 기준이다.

predict()는 실기 정답에서 빠지지 않는 함수다.
