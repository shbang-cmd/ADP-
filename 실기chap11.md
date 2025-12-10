Chapter 11. 로지스틱 회귀(Logistic Regression)**
# Chapter 11. 로지스틱 회귀(Logistic Regression)

로지스틱 회귀는 **범주형(특히 이진) 종속변수**를 설명하는 가장 중요한 통계적 모델이다.  
ADP 실기에서 단독 문제로 자주 등장하며, 회귀계수 해석·오즈비·예측 등이 필수적으로 묻힌다.

본 장에서 다루는 내용:

- 이진 로지스틱 회귀 개념  
- glm(family = binomial) 사용법  
- 회귀계수 해석  
- 오즈비(OR: Odds Ratio)  
- 모델 적합도 평가  
- 예측과 분류 성능 평가  

---

## 📂 데이터셋 (disease_risk.csv)

```r
risk <- read.csv(text = "
id,age,gender,smoking,bp_high,risk
1,45,M,Yes,140,1
2,30,F,No,120,0
3,60,M,Yes,150,1
4,50,F,No,135,0
5,35,M,No,128,0
6,70,M,Yes,160,1
7,55,F,Yes,145,1
8,40,F,No,125,0
9,65,M,Yes,155,1
10,32,M,No,118,0
")


변수 설명

risk: 질병 위험 여부(1=위험, 0=정상)

age: 나이

gender: 성별

smoking: 흡연 여부

bp_high: 수축기 혈압

문제 1. Factor 변환 및 데이터 확인

로지스틱 회귀에서 종속변수는 factor(0/1)이어야 한다.

🔧 R 코드
risk$risk <- as.factor(risk$risk)
risk$gender <- as.factor(risk$gender)
risk$smoking <- as.factor(risk$smoking)

str(risk)

📊 해석

종속변수 risk가 factor가 아니면 glm()이 잘못 동작한다.

성별, 흡연처럼 범주형 변수는 반드시 factor 처리해야 한다.

문제 2. 로지스틱 회귀모델 적합

요구사항

risk ~ age + gender + smoking + bp_high

로지스틱 회귀모델을 적합하고 summary()로 결과를 확인하라.

🔧 R 코드
model <- glm(risk ~ age + gender + smoking + bp_high,
             data = risk, family = binomial)

summary(model)

📊 해석 포인트
1) 회귀계수(coef)는 log-odds 단위

양수 → 위험 증가

음수 → 위험 감소

2) p-value < 0.05

→ 해당 변수는 통계적으로 유의함

3) Null deviance vs Residual deviance

→ 모델이 얼마나 개선되었는지 확인 가능

문제 3. 오즈비(Odds Ratio) 계산

로지스틱 회귀의 핵심은 오즈비(OR) 해석이다.

🔧 R 코드
exp(coef(model))

📊 해석

예시 해석:

OR(age) = 1.08
→ 나이가 1세 증가할 때 질병 위험 오즈가 8% 증가

OR(smokingYes) = 3.5
→ 흡연자는 비흡연자보다 위험이 3.5배 높음

OR(bp_high) = 1.02
→ 혈압 1 증가 시 위험도 2% 증가

오즈비 해석은 실기 문항에서 거의 100% 출제된다.

문제 4. 예측값 생성 (예측확률 & 0/1 분류)
🔧 R 코드
risk$pred_prob <- predict(model, type = "response")
risk$pred_class <- ifelse(risk$pred_prob > 0.5, 1, 0)

risk

📊 해석

type="response" → 0~1 사이 확률 출력

임계값 0.5 기준으로 0/1 분류

예측 확률은 ROC Curve와 연결되는 핵심 개념

문제 5. 혼동행렬(Confusion Matrix) 생성
🔧 R 코드
table(Predicted = risk$pred_class, Actual = risk$risk)

📊 해석

Confusion Matrix로부터 아래 값을 계산할 수 있다.

Accuracy = 정확도

Sensitivity = 민감도(재현율, Recall)

Specificity = 특이도

시험에서는 Recall이 중요한 의료 분야 문제가 자주 출제된다.

문제 6. ROC Curve 및 AUC
🔧 R 코드
library(pROC)

roc_obj <- roc(risk$risk, risk$pred_prob)
auc(roc_obj)
plot(roc_obj, col="blue", main="ROC Curve")

📊 해석

AUC 0.9 이상 → 매우 우수

AUC 0.7~0.9 → 보통~좋음

AUC < 0.7 → 낮음

AUC는 모델의 분류 성능을 종합적으로 평가하는 매우 중요한 지표다.

문제 7. 다중공선성(VIF) 체크
🔧 R 코드
library(car)
vif(model)

📊 해석

VIF > 10 → 다중공선성 문제

필요시 변수제거 또는 Lasso 회귀 등 적용

문제 8. 모델 선택(AIC 기반)

AIC가 낮을수록 좋은 모델이다.

🔧 R 코드
step(model, direction = "both")

📊 해석

backward/forward/both 방식으로 자동 변수 선택

실무에서도 매우 자주 사용되는 기법

✔ Chapter 11 요약

로지스틱 회귀는 이진 분류 모델 중 가장 기본이자 중요한 분석 방법이다.

회귀계수는 log-odds 단위이므로 exp(coef)로 OR을 반드시 계산해야 한다.

Confusion Matrix, ROC, AUC는 분류 모델 평가의 핵심이다.

VIF로 다중공선성을 점검해야 한다.

step()으로 AIC 기반 변수 선택을 수행할 수 있다.





Chapter 12. 의사결정나무 & 랜덤포레스트 (Decision Tree · Random Forest)**
# Chapter 12. 의사결정나무 & 랜덤포레스트
의사결정나무(Decision Tree)는 규칙 기반의 매우 직관적인 모델이며,  
랜덤포레스트(Random Forest)는 다수의 의사결정나무를 결합한 강력한 앙상블 모델이다.

ADP 실기에서는 다음 개념이 중요하다:

- 의사결정나무 생성 및 시각화  
- 가지치기(pruning)  
- 변수 중요도(feature importance)  
- 랜덤포레스트 기본 사용법  
- 예측 및 성능 비교  

---

## 📂 데이터셋 (customer_churn.csv)

```r
churn <- read.csv(text = "
id,age,tenure,usage,contract,churn
1,25,5,300,Month,1
2,40,12,200,Year,0
3,35,24,250,Year,0
4,50,6,150,Month,1
5,45,36,350,Year,0
6,30,3,100,Month,1
7,28,8,220,Month,0
8,55,18,180,Year,0
9,33,4,130,Month,1
10,48,30,320,Year,0
")


변수 설명

age: 나이

tenure: 가입기간

usage: 월 사용량

contract: 계약형태(Month/Year)

churn: 이탈여부(1=이탈, 0=유지)

문제 1. Factor 변환

의사결정나무는 범주형 변수가 factor여야 한다.

🔧 R 코드
churn$contract <- as.factor(churn$contract)
churn$churn <- as.factor(churn$churn)

str(churn)

문제 2. 의사결정나무 모델 생성
🔧 R 코드
library(rpart)
tree_model <- rpart(churn ~ age + tenure + usage + contract,
                    data = churn, method = "class")

tree_model

📊 해석

rpart()는 기본적인 CART(Classification and Regression Tree) 모델을 생성

의사결정 규칙은 변수 분할 기준을 기반으로 자동 생성

분할조건: Gini index 또는 Entropy

문제 3. 의사결정나무 시각화
🔧 R 코드
library(rpart.plot)
rpart.plot(tree_model)

📊 해석

루트 노드부터 리프 노드까지 내려가며 분류 규칙을 확인

“usage < 180 → Churn=1” 같은 규칙이 자동 생성됨

문제 4. 예측 & 혼동행렬
🔧 R 코드
pred <- predict(tree_model, churn, type = "class")
table(Predicted = pred, Actual = churn$churn)

📊 해석

Accuracy를 직접 계산할 수 있음

실기에서는 "예측 클래스 vs 실제 클래스" 비교가 필수

문제 5. 가지치기(Pruning)

의사결정나무는 과적합되기 쉽기 때문에 가지치기가 필요하다.

🔧 R 코드
printcp(tree_model)       # cp값 확인
pruned <- prune(tree_model, cp = tree_model$cptable[which.min(tree_model$cptable[,"xerror"]), "CP"])

rpart.plot(pruned)

📊 해석

cp(complexity parameter)가 작을수록 트리가 복잡

xerror가 최소가 되는 cp를 선택하는 것이 일반적

가지치기 후 트리가 간결해지고 예측력도 좋아질 가능성이 있음

문제 6. 랜덤포레스트 모델 생성

랜덤포레스트는 여러 개의 결정나무를 랜덤하게 생성하여 평균을 취하는 앙상블 방식이다.

🔧 R 코드
library(randomForest)

rf <- randomForest(churn ~ age + tenure + usage + contract,
                   data = churn, ntree = 200, mtry = 2)

rf

📊 해석

ntree: 생성할 트리 개수(많을수록 안정)

mtry: 분할 시 고려할 변수 수

Out-of-bag(OOB) error는 랜덤포레스트의 기본 성능지표

문제 7. 변수 중요도 (Feature Importance)
🔧 R 코드
importance(rf)
varImpPlot(rf)

📊 해석

MeanDecreaseGini: 변수의 분류 기여도

usage, tenure가 churn 예측에서 중요한 변수일 가능성이 큼

feature importance는 실기에서 자주 묻는 포인트

문제 8. 랜덤포레스트 예측 & 혼동행렬
🔧 R 코드
pred_rf <- predict(rf, churn)
table(Predicted = pred_rf, Actual = churn$churn)

📊 해석

일반적으로 랜덤포레스트가 의사결정나무보다 더 높은 정확도를 제공

결정나무는 해석 용이성, 랜덤포레스트는 예측력이라는 장점이 있다.

문제 9. 모델 성능 비교
📊 비교 요약
모델	장점	단점
Decision Tree	해석 용이, 규칙 기반	과적합 위험, 예측력 낮음
Random Forest	예측력 우수, 안정적	해석 어려움, 계산 부담

실기에서는
“두 모델의 차이를 설명하라”,
“랜덤포레스트가 더 좋은 이유는?”
같은 서술형 문제가 자주 출제된다.

✔ Chapter 12 요약

의사결정나무(CART)는 간단하고 해석이 명확해 실무에 자주 사용됨

randomForest는 예측력이 매우 높아 데이터 분석에서 기본 모델로 활용됨

cp 기반 가지치기(pruning)는 트리 모델의 필수 과정

변수 중요도는 모델 해석에서 매우 중요

실기에서는 분류 정확도, 혼동행렬, 규칙 해석 관련 문항이 자주 등장한다.




Chapter 13. 클러스터 분석(Cluster Analysis: K-means & Hierarchical Clustering)**
# Chapter 13. 클러스터 분석 (Cluster Analysis)

클러스터 분석은 **비지도학습(Unsupervised Learning)**의 핵심 기법으로  
데이터를 비슷한 특성을 가진 그룹으로 묶는 방법이다.

ADP 실기에서는 다음 개념이 자주 출제된다:

- K-means 클러스터링  
- 계층적 군집(Hierarchical clustering)  
- 실루엣 계수(Silhouette Score)  
- 거리 계산 방법  
- 덴드로그램 해석  

---

## 📂 데이터셋 (mall_customers.csv)

```r
mall <- read.csv(text = "
id,age,annual_income,spending_score
1,19,15,39
2,21,15,81
3,20,16,6
4,23,16,77
5,31,17,40
6,22,17,76
7,35,18,6
8,23,18,94
9,64,19,3
10,30,19,72
")


변수 설명

age: 나이

annual_income: 연간 소득

spending_score: 소비 성향 점수

문제 1. 변수 표준화(Standardization)

K-means는 변수 크기(scale)에 민감하기 때문에 반드시 표준화해야 한다.

🔧 R 코드
mall_scaled <- scale(mall[, c("age", "annual_income", "spending_score")])
head(mall_scaled)

📊 해석

scale()은 (x - mean) / sd로 표준화 수행

변수 스케일 차이를 제거하여 거리 기반 알고리즘(K-means, hierachical)의 정확도를 높인다.

문제 2. K-means 군집화 수행
🔧 R 코드
set.seed(123)
km <- kmeans(mall_scaled, centers = 3)
km$cluster

📊 해석

각 고객이 1~3번 군집 중 하나로 배정

km$centers를 확인하여 군집 특성 파악

예시 해석:

Cluster 1: 젊고 spending_score 낮음

Cluster 2: 소득 높고 spending_score 높음

Cluster 3: 나이가 많고 spending_score 낮음

문제 3. 군집 시각화
🔧 R 코드
library(ggplot2)

mall$cluster <- factor(km$cluster)

ggplot(mall, aes(annual_income, spending_score, color = cluster)) +
  geom_point(size = 3) +
  ggtitle("K-means Customer Segmentation")

📊 해석

K-means 군집 결과가 시각적으로 잘 분리되는지 확인 가능

군집 개수 선택에 대한 판단 근거로도 사용됨

문제 4. 최적 군집수 결정(Elbow Method)
🔧 R 코드
wss <- sapply(1:10, function(k){
  kmeans(mall_scaled, centers = k)$tot.withinss
})

plot(1:10, wss, type="b",
     xlab="Number of Clusters k",
     ylab="Within-cluster Sum of Squares")

📊 해석

그래프가 꺾이는 지점(elbow)을 최적 k값으로 선택

ADP 실기에서 Elbow Plot 해석 문제가 자주 나옴

문제 5. 실루엣 계수(Silhouette Score)

군집 품질을 평가하는 지표이다.

🔧 R 코드
library(cluster)

sil <- silhouette(km$cluster, dist(mall_scaled))
mean(sil[, 3])

📊 해석

1에 가까울수록 군집이 잘 분리됨

0~0.5는 보통, 0 이하이면 군집이 잘못된 경우

실기에서 "가장 좋은 k를 판단하라" 형태로 자주 등장

문제 6. 계층적 군집(Hierarchical Clustering)
🔧 R 코드
d <- dist(mall_scaled, method = "euclidean")

hc <- hclust(d, method = "ward.D2")

plot(hc, main = "Hierarchical Clustering Dendrogram")

📊 해석

Ward.D2는 군집 내 분산을 최소화하는 방식

덴드로그램은 군집 분리 과정을 시각적으로 표현

어느 높이에서 cuttree() 할지 판단 가능

문제 7. 3개의 군집으로 자르기
🔧 R 코드
cutree(hc, k = 3)

📊 해석

K-means와 비교하여 군집 구성 차이를 설명하는 문제가 실기에서 자주 등장

계층적 군집은 데이터 크기가 작을 때 매우 유용

문제 8. K-means vs Hierarchical 비교
📊 표로 정리
항목	K-means	Hierarchical
알고리즘	거리 기반 반복	트리 기반
장점	빠르고 대규모 데이터 적합	시각적 해석 용이
단점	초기 중심점 민감	데이터 많으면 비용 큼
군집수	사전에 k 지정	덴드로그램으로 판단
문제 9. 군집 기반 고객 세그먼트 해석

실기에서는 군집 번호의 의미를 해석하는 서술형 문제가 자주 등장한다.

예시:

Cluster 1: 저소득, 저소비 그룹 → 비용절감 타깃

Cluster 2: 고소득, 고소비 → VIP 고객 대상 전략 필요

Cluster 3: 고연령, 평균 소비 → 금융상품 타깃팅 가능

✔ Chapter 13 요약

K-means는 표준화 및 거리 기반 군집에 적합

Elbow Plot, Silhouette Score는 최적 k 선택의 핵심

계층적 군집은 시각적으로 군집 구조 이해에 용이

K-means vs Hierarchical 비교는 필수 암기 포인트

실기에서는 군집 결과 해석이 매우 자주 출제됨




Chapter 14. 텍스트 마이닝 기초(Text Mining Basics)**
# Chapter 14. 텍스트 마이닝(Text Mining) 기초

텍스트 마이닝은 비정형 데이터 분석의 핵심 분야로  
ADP 실기에서도 단답형 + 실습형 문제로 꾸준히 출제되는 영역이다.

본 장에서는 다음을 다룬다:

- 텍스트 정제(Text Preprocessing)  
- 불용어 제거(Stopwords)  
- 형태소 분석  
- 단어 빈도 계산  
- TF-IDF  
- 워드클라우드 생성  

---

## 📂 데이터셋 (reviews.csv)

```r
reviews <- read.csv(text = "
id,review
1,The product quality is great and delivery was fast
2,Very disappointed with the product, not worth the price
3,Excellent quality and amazing customer service
4,The delivery was slow and the packaging was damaged
5,Great value for the money, highly recommend
6,Poor quality and terrible support
7,Fast delivery, good packaging and reliable product
8,Not satisfied with the product, bad experience overall
9,Amazing performance and excellent build quality
10,Packaging was good but delivery took too long
")

문제 1. 텍스트 전처리 (소문자 변환 + 특수문자 제거)
🔧 R 코드
library(tm)

corpus <- VCorpus(VectorSource(reviews$review))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

corpus[[1]]$content

📊 해석

텍스트 분석의 첫 단계는 데이터를 정규화(normalization)하는 것

불필요한 문자 제거 후 단어 빈도 계산이 용이해진다

문제 2. 불용어(Stopword) 제거
🔧 R 코드
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus[[1]]$content

📊 해석

“the, and, with, was” 같은 의미 없는 단어 제거

분석 품질을 높이는 핵심 전처리 과정

문제 3. 문서-단어 행렬(Document-Term Matrix)
🔧 R 코드
dtm <- DocumentTermMatrix(corpus)
dtm

📊 해석

행(row): 문서

열(column): 단어

값: 단어 빈도(Frequency)

이는 텍스트 분석의 기본 기반 데이터 구조이다.

문제 4. 단어 빈도 계산
🔧 R 코드
freq <- colSums(as.matrix(dtm))
sort(freq, decreasing = TRUE)[1:10]

📊 해석

빈도 상위 단어(top words)는 문서 전체의 핵심 주제를 요약한다

예: "quality", "delivery", "product", "excellent" 등

문제 5. TF-IDF 계산

TF-IDF는 단순 빈도보다 더 정교하게 단어의 중요도를 판단한다.

🔧 R 코드
dtm_tfidf <- weightTfIdf(dtm)

inspect(dtm_tfidf[1:3, 1:5])

📊 해석

TF-IDF 값은 문서 내에서 중요하지만 전체 문서에서는 흔하지 않은 단어에 높은 점수를 부여

리뷰 특성 분석, 감성분석 등에서 매우 유용

문제 6. 워드클라우드 생성
🔧 R 코드
library(wordcloud)

wordcloud(names(freq), freq, max.words = 50, colors = brewer.pal(8, "Dark2"))

📊 해석

단어 중요도를 직관적으로 시각화

텍스트 데이터 탐색 과정에서 자주 활용

문제 7. 감성분석의 기초 개념(이론 문제 출제 포인트)

실기에는 감성 사전 기반 분석 개념이 자주 출제된다.

예시:

positive 단어 목록: good, great, excellent, amazing

negative 단어 목록: bad, poor, terrible, disappointed

🔧 (예시 코드)
positive <- c("great","excellent","amazing","good")
negative <- c("poor","bad","terrible","disappointed")

sentiment_score <- sapply(corpus, function(doc) {
    sum(str_count(doc$content, positive)) -
    sum(str_count(doc$content, negative))
})

sentiment_score

📊 해석

점수가 양수 → 긍정 리뷰

음수 → 부정 리뷰

이는 Rule-based Sentiment Analysis의 기본 개념

문제 8. 텍스트 마이닝 실기에서 자주 나오는 개념 요약
개념	설명
Tokenization	문장을 단어로 분해
Stopword Removal	자주 등장하지만 의미 없는 단어 제거
Stemming	단어의 원형으로 축소(run/running → run)
Lemmatization	문법 기반 원형 복원
TF	단어 빈도
IDF	문서 전체에서의 희귀성
TF-IDF	TF × IDF
✔ Chapter 14 요약

텍스트 전처리가 가장 중요한 단계

DTM(Document-Term Matrix) 생성은 모든 텍스트 분석의 출발점

TF-IDF는 단순 빈도보다 고급 분석에 적합

워드클라우드는 탐색적 분석에 매우 유용

감성분석은 기본 원리만 이해해도 실기 대비 충분



Chapter 15. 딥러닝 기초(Deep Learning Basics, Neural Networks)**
# Chapter 15. 딥러닝 기초 (Neural Networks & Deep Learning)

ADP 실기에서는 딥러닝 문제 자체가 길게 나오지는 않지만,  
**신경망 구조와 원리, R에서의 keras 활용, 하이퍼파라미터 개념** 등이 자주 출제된다.

본 장에서는 다음을 다룬다:

- 신경망(Neural Network)의 기본 구조  
- 퍼셉트론(Perceptron) 개념  
- 활성함수(Activation Function)  
- R keras를 사용한 간단한 모델 구현  
- Train/Test 분리  
- 모델 성능 평가  

---

## 📂 데이터셋 (bank_marketing.csv)

```r
bank <- read.csv(text = "
age,balance,loan,default,deposit
30,1000,No,No,1
45,2000,Yes,No,0
38,1500,No,No,1
50,500,Yes,Yes,0
28,1200,No,No,1
60,300,Yes,Yes,0
42,1800,Yes,No,0
33,1700,No,No,1
55,400,Yes,Yes,0
36,1600,No,No,1
")


변수 설명

deposit: 정기예금 가입 여부(1=가입, 0=미가입) ← 예측 대상

loan, default는 범주형이므로 factor 변환 필요

문제 1. 전처리 (Factor 변환 + Scaling)
🔧 R 코드
bank$loan <- as.factor(bank$loan)
bank$default <- as.factor(bank$default)
bank$deposit <- as.factor(bank$deposit)

library(caret)

scaled <- preProcess(bank[, c("age","balance")], method = c("center","scale"))
bank_scaled <- predict(scaled, bank)

📊 해석

신경망은 입력 값 크기의 차이에 매우 민감 → 반드시 정규화/표준화 필요

factor 변수는 원-핫 인코딩(one-hot encoding)으로 변환됨(keras에서 자동 처리)

문제 2. Train/Test 분리
🔧 R 코드
set.seed(123)
idx <- createDataPartition(bank$deposit, p = 0.7, list = FALSE)
train <- bank_scaled[idx, ]
test  <- bank_scaled[-idx, ]

문제 3. keras 모델 구현

R에서는 keras 패키지를 사용해 딥러닝 모델을 구축할 수 있다.

🔧 R 코드
library(keras)

# input shape: 4 inputs
model <- keras_model_sequential() %>%
  layer_dense(units = 8, activation = "relu", input_shape = 4) %>%
  layer_dense(units = 4, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
model

📊 해석

Dense layer: 완전연결층

hidden units: 8 → 4로 축소 (계층적 특징 추출)

activation: relu, sigmoid는 딥러닝에서 가장 기본

loss: binary_crossentropy (이진 분류일 때 필수)

문제 4. 학습(Fit) 수행
🔧 R 코드
history <- model %>% fit(
  as.matrix(train[, c("age","balance")]),
  as.numeric(train$deposit) - 1,
  epochs = 50,
  batch_size = 4,
  validation_split = 0.2
)

📊 해석

epoch: 전체 학습 반복 횟수

batch_size: 한 번에 학습하는 데이터 수

validation_split: 검증 데이터 분리 비율

문제 5. 학습 곡선(Training Curve) 시각화
🔧 R 코드
plot(history)

📊 해석

loss 감소, accuracy 증가 → 학습 정상

validation loss가 증가하기 시작하면 과적합 가능성

문제 6. 모델 예측 및 평가
🔧 R 코드
pred_prob <- model %>% predict(as.matrix(test[, c("age","balance")]))
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

table(Predicted = pred_class, Actual = as.numeric(test$deposit) - 1)

📊 해석

확률 출력 → 0.5 기준 분류

Confusion Matrix로 정확도 평가

실기에는 “예측 정확도 계산하라” 문제 출제됨

문제 7. 활성함수(Activation Function) 개념 정리
함수	특징	사용 위치
Sigmoid	0~1 범위, S자 형태	출력층 이진 분류
ReLU	0 이하 0, 양수는 그대로	은닉층 기본
Tanh	-1~1 범위	감정 분석 등 양/음 중요할 때

ADP 필기·실기 둘 다 자주 출제되는 개념이다.

문제 8. 신경망 구조 개념(퍼셉트론)

요약:

입력(Input)

가중치(Weight)

편향(Bias)

활성함수(Activation)

출력(Output)

출력 = 활성함수( 입력×가중치 + 편향 )

이 공식은 실기 서술형으로 종종 등장한다.

문제 9. 과적합 방지 기법
기법	설명
Dropout	일부 노드 임의 비활성화
EarlyStopping	검증 손실 증가 시 학습 중단
Regularization(L1/L2)	가중치 페널티 부여
Batch Normalization	입력 정규화

딥러닝의 핵심 개념으로 이해 필요.

✔ Chapter 15 요약

딥러닝은 선형모델을 확장한 구조이며 R에서는 keras 패키지로 쉽게 구성 가능

활성함수, epoch, batch size는 반드시 이해해야 하는 기본 개념

Confusion Matrix를 통한 분류 평가가 실기 정답 기준

과적합 방지를 위한 기법도 실기에서 서술형으로 출제되는 빈도가 높음
