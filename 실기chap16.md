Chapter 16. 생존 분석(Survival Analysis) 기초**
# Chapter 16. 생존 분석(Survival Analysis) 기초

생존분석은 특정 사건(event)까지 걸리는 시간(time-to-event)을 분석하는 기법이다.  
의료, 보험, 고객 이탈(Churn) 분야에서 널리 사용되며  
ADP 실기에서 **Kaplan-Meier 곡선, Log-rank 검정, Cox 비례위험모형**이 자주 출제된다.

---

## 📂 데이터셋 (survival_data.csv)

```r
surv <- read.csv(text = "
id,time,event,age,gender,treatment
1,5,1,65,M,A
2,10,0,70,F,A
3,7,1,50,M,B
4,12,1,55,F,A
5,3,1,60,M,B
6,20,0,48,F,A
7,15,1,52,M,B
8,9,1,58,F,A
9,14,0,63,M,B
10,6,1,49,F,A
")


변수 설명

time: 생존시간 (고객 유지기간, 병원 재입원까지 시간 등)

event: 사건 발생 여부 (1=발생, 0=중도절단(censored))

treatment: 그룹 비교 변수

age, gender: 공변량

문제 1. Surv 객체 생성

Survival 패키지에서는 분석을 위해 Surv() 객체를 생성해야 한다.

🔧 R 코드
library(survival)

S_obj <- Surv(time = surv$time, event = surv$event)
S_obj

📊 해석

Surv(time, event) 형식

event=1이면 사건 발생, event=0이면 중도절단(censoring)

생존분석에서 가장 기초적인 데이터 구조

문제 2. Kaplan-Meier 생존곡선(KM Curve)
🔧 R 코드
fit_km <- survfit(S_obj ~ 1, data = surv)
summary(fit_km)
plot(fit_km, main="Kaplan-Meier Survival Curve",
     xlab="Time", ylab="Survival Probability")

📊 해석

KM Curve는 시간에 따라 생존 확률이 어떻게 감소하는지 보여줌

꺾이는 지점 → 사건 발생이 있었던 시점

중도절단이 있는 경우 그래프에 표시

문제 3. 그룹별 생존함수 비교 (Treatment A vs B)
🔧 R 코드
fit_group <- survfit(S_obj ~ treatment, data = surv)
plot(fit_group, col=c("blue","red"), lwd=2)
legend("bottomleft", legend=c("A","B"), col=c("blue","red"), lwd=2)

📊 해석

그룹별 생존 곡선이 다르면 두 그룹의 차이가 존재

치료법 A가 더 효과적이면 생존 확률이 더 높게 유지됨

문제 4. Log-rank 검정

두 생존곡선의 차이가 통계적으로 유의한지 검정한다.

🔧 R 코드
survdiff(S_obj ~ treatment, data = surv)

📊 해석

p-value < 0.05 → 두 그룹 간 생존함수 차이가 유의함

ADP 실기에서 빈출되는 핵심 개념

문제 5. Cox 비례위험모형(Cox Proportional Hazards Model)
🔧 R 코드
cox_model <- coxph(S_obj ~ age + gender + treatment, data = surv)
summary(cox_model)

📊 해석 포인트

계수(coef)가 양수 → 위험도 증가

exp(coef) = Hazard Ratio(HR)

HR > 1 → 위험 증가

HR < 1 → 위험 감소

예: HR(treatmentB) = 1.5 → B 그룹은 사건 위험이 1.5배 높음

문제 6. 콕스모형 잔차검정 (Proportional Hazard Assumption)
🔧 R 코드
cox.zph(cox_model)
plot(cox.zph(cox_model))

📊 해석

p-value > 0.05 → 비례위험 가정이 만족됨

실기에서 “PH 가정이란?” 설명 문제가 출제됨

문제 7. 예측 생존곡선 그리기
🔧 R 코드
newdata <- data.frame(age = 55, gender="M", treatment="A")
pred_km <- survfit(cox_model, newdata = newdata)

plot(pred_km, col="blue", lwd=2,
     main="Predicted Survival Curve for New Patient")

📊 해석

공변량을 가진 개인별 생존 확률 추정 가능

실무에서도 매우 자주 사용

문제 8. 생존 분석 핵심 이론 요약 (필기 + 실기 대비)
개념	설명
생존함수 S(t)	t 시점까지 생존할 확률
위험함수 h(t)	짧은 시간 동안 사건 발생할 즉시 위험
KM Curve	생존함수의 비모수적 추정
Log-rank 검정	그룹 간 생존곡선 비교
Cox PH model	공변량을 포함한 반모수적 생존 모델
Hazard Ratio	exp(coefficient)
✔ Chapter 16 요약

생존분석은 의료·보험·고객이탈 분야에서 널리 사용

Surv(), survfit(), survdiff(), coxph()는 반드시 익혀야 할 핵심 함수

Log-rank 검정은 그룹 비교의 기본

Cox 모형의 해석 포인트는 Hazard Ratio(HR)

비례위험 가정(PH Assumption)은 실기 시험에서 자주 등장하는 개념




Chapter 17. 실험설계(Design of Experiments, DOE)**
# Chapter 17. 실험설계(Design of Experiments, DOE)

실험설계(Design of Experiments, DOE)는  
요인(variables)이 결과에 미치는 영향을 체계적으로 분석하는 기법이다.

ADP 실기에서는 다음 개념이 자주 출제된다:

- 완전확률배치법(CRD)  
- 무작화(Randomization)  
- 요인설계(Factorial Design)  
- 교호작용(Interaction)  
- ANOVA 해석  
- 반응표면법(RSM) 기초 개념  

---

## 📂 데이터셋 (fertilizer_yield.csv)

```r
yield <- read.csv(text = "
fertilizer,water,yield
Low,Low,20
Low,Medium,28
Low,High,35
Medium,Low,30
Medium,Medium,40
Medium,High,50
High,Low,45
High,Medium,55
High,High,65
")


변수 설명

fertilizer: 비료량 (Low / Medium / High)

water: 관수 수준 (Low / Medium / High)

yield: 작물 수확량

문제 1. 요인(factor) 변환

DOE에서는 요인(factor)이 반드시 범주형이어야 한다.

🔧 R 코드
yield$fertilizer <- as.factor(yield$fertilizer)
yield$water <- as.factor(yield$water)

str(yield)

문제 2. 2요인 분산분석 (Two-way ANOVA)

비료량과 물 공급량이 수확량에 미치는 영향을 분석한다.

🔧 R 코드
anova_model <- aov(yield ~ fertilizer * water, data = yield)
summary(anova_model)

📊 해석 포인트

표에서 유의해야 할 항목:

1) Main Effect(주효과)

fertilizer

water
→ 각각이 단독으로 yield에 영향을 주는지 판단

2) Interaction Effect(교호작용)

fertilizer:water
→ 비료 효과가 물 공급량에 따라 달라지는지 여부

p-value < 0.05 → 유의함

문제 3. 교호작용 시각화 (Interaction Plot)
🔧 R 코드
interaction.plot(yield$water, yield$fertilizer, yield$yield,
                 col = c("red","blue","green"),
                 lwd = 2,
                 ylab = "Yield", xlab = "Water Level",
                 trace.label = "Fertilizer")

📊 해석

선들이 평행하지 않으면 교호작용이 존재

실기에서 "Interaction이 존재하는지 해석하라" 문제가 자주 출제됨

문제 4. 다중비교(Tukey HSD)

비료 수준 간 차이 구간을 비교한다.

🔧 R 코드
TukeyHSD(anova_model)

📊 해석

각 그룹 간 평균 차이와 신뢰구간 확인

신뢰구간이 0을 포함하지 않으면 유의한 차이가 있음

문제 5. 모형 진단(ANOVA Residual Diagnostics)
🔧 R 코드
par(mfrow=c(2,2))
plot(anova_model)

📊 해석

Normal Q-Q: 잔차 정규성 확인

Residuals vs Fitted: 등분산성 확인

실기에서 잔차 진단 해석 문제가 출제됨

문제 6. 반응표면법(Response Surface Methodology) 기초 개념

※ 실기에서는 코딩 대신 개념 설명 문제가 출제됨.

✔ 핵심 개념
개념	설명
중심합성계획(CCD)	중심점을 포함한 2차 반응표면 설계
Box–Behnken	변수 조합을 최소화한 효율적 설계
반응표면(Response Surface)	입력 변화에 따라 출력이 변하는 곡면
최적화	최대 수확량/최소 비용 등 목적을 위한 최적 조건 탐색
문제 7. DOE 필수 용어 요약
용어	정의	ADP 실기 포인트
Randomization	실험 순서를 무작위로	편향 제거
Replication	반복 실험	변동성 추정
Blocking	유사 조건끼리 묶기	교란요인 제거
Factor	독립변수	categorical
Level	Factor의 값	Low/Med/High
Treatment	Factor 조합	실험 조건
문제 8. 교호작용 존재 시 해석 방법
✔ Interaction 존재 시 주효과 해석에 주의해야 한다.

예시 해석:

교호작용이 유의함
→ "비료 효과는 물 공급량에 따라 달라진다"
→ 단독 비료 효과는 별도로 해석하면 안 됨

교호작용이 없음
→ 주효과 해석만으로 충분

문제 9. 실기형 질문 예시

Q1. Two-way ANOVA 결과에서 교호작용이 유의하다면 어떻게 해석하는가?
→ 한 요인의 효과가 다른 요인의 수준에 따라 달라진다.

Q2. 실험설계에서 Randomization이 필요한 이유는?
→ 편향 제거 및 외부요인 통제.

Q3. replicate가 많아질수록 무엇을 더 정확히 추정할 수 있는가?
→ 오차(잔차) 분산.

✔ Chapter 17 요약

Two-way ANOVA는 DOE 문제의 핵심

교호작용(Interaction) 유무 판단이 가장 많이 출제됨

Tukey HSD는 사후검정의 표준 방법

DOE 핵심 요소(무작화, 반복, 블로킹)는 암기 필수

반응표면법(RSM)은 개념 위주로만 출제됨



Chapter 18. 품질관리(Quality Control) — 관리도 & 공정능력지수**
# Chapter 18. 품질관리(Quality Control: SPC & Capability Index)

품질관리(Statistical Process Control, SPC)는  
제조·서비스 공정의 안정성 여부를 판단하기 위해 통계적 기법을 사용하는 분석 방법이다.

ADP 실기에서 자주 출제되는 항목:

- 관리도(Control Chart)  
- X-bar 관리도, R 관리도  
- 공정능력지수(Cp, Cpk)  
- 상·하한 규격(USL/LSL)  
- 공정 안정성 해석  

---

## 📂 데이터셋 (process_sample.csv)

```r
process <- read.csv(text = "
sample,measurement
1,10.2
2,10.5
3,9.9
4,10.3
5,10.1
6,10.4
7,9.8
8,10.5
9,10.2
10,9.9
11,10.6
12,10.4
13,10.3
14,10.1
15,10.7
")


목표값(Target) = 10.0
관리한계(UCL/LCL)는 데이터에서 계산
규격한계(Spec Limits)는 다음과 같다고 가정:

LSL = 9.5

USL = 10.5

문제 1. 공정 평균 및 표준편차 계산
🔧 R 코드
xbar <- mean(process$measurement)
s <- sd(process$measurement)

xbar; s

📊 해석

평균이 목표값(10.0)에 근접한지 확인

표준편차는 공정의 변동성 정도를 나타냄

문제 2. X-bar 관리도 산출

관리도의 기본 구조:

중앙선(CL) = 공정 평균

상한(UCL) = xbar + 3σ

하한(LCL) = xbar – 3σ

🔧 R 코드
UCL <- xbar + 3*s
LCL <- xbar - 3*s

UCL; LCL

📊 해석

관측값이 관리한계를 벗어나면 공정이 통계적으로 불안정

실기에서 “이 관리도가 안정적인가?” 문제로 출제됨

문제 3. 관리도 시각화
🔧 R 코드
library(ggplot2)

ggplot(process, aes(x = sample, y = measurement)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = xbar, color="blue") +
  geom_hline(yintercept = UCL, color="red", linetype="dashed") +
  geom_hline(yintercept = LCL, color="red", linetype="dashed") +
  ggtitle("X-bar Control Chart")

📊 해석

점이 UCL 또는 LCL을 넘으면 이상 상태(out-of-control)

연속된 점이 한쪽으로 치우치면 “추세(Trend) 패턴” 가능성

문제 4. 공정능력지수 Cp, Cpk 계산

공정능력지수(Capability Index)는
공정이 규격한계(LSL/USL)를 얼마나 잘 만족하는지 나타내는 지표.

✔ Cp 계산
𝐶
𝑝
=
𝑈
𝑆
𝐿
−
𝐿
𝑆
𝐿
6
𝜎
Cp=
6σ
USL−LSL
	​

🔧 R 코드
LSL <- 9.5
USL <- 10.5

Cp <- (USL - LSL) / (6*s)
Cp

📊 Cp 해석
Cp 값	의미
Cp < 1.0	규격을 만족 못함(불량률 높음)
Cp = 1.0	3σ 수준, 최소 요구 기준
Cp ≥ 1.33	충분히 우수
Cp ≥ 1.67	매우 우수
✔ Cpk 계산
𝐶
𝑝
𝑘
=
min
⁡
(
𝑈
𝑆
𝐿
−
𝜇
3
𝜎
,
𝜇
−
𝐿
𝑆
𝐿
3
𝜎
)
Cpk=min(
3σ
USL−μ
	​

,
3σ
μ−LSL
	​

)
🔧 R 코드
Cpk <- min((USL - xbar)/(3*s), (xbar - LSL)/(3*s))
Cpk

📊 Cpk 해석

Cp는 공정의 “잠재력(potential)”

Cpk는 실제 평균 이동을 고려한 “실제 능력(actual capability)”

Cp > Cpk 이면 평균이 중앙에서 벗어나 있다는 뜻

문제 5. 공정능력지수 해석 문제 (실기 자주 출제)
예시 서술형 문제:

“Cp는 1.5이고 Cpk는 1.0이다. 이를 해석하라.”

모범 답안

Cp가 높다는 것은 공정 변동성은 우수함

Cpk가 Cp보다 낮으므로 공정 평균이 중앙값에서 벗어나 있다

중앙 이동(center shift)이 존재하며 공정 개선 필요

문제 6. 불량률(Defect Rate) 추정

정규분포 가정하에 USL/LSL 밖으로 나가는 비율 계산

🔧 R 코드
p_lower <- pnorm(LSL, mean=xbar, sd=s)
p_upper <- 1 - pnorm(USL, mean=xbar, sd=s)

defect_rate <- p_lower + p_upper
defect_rate

📊 해석

불량률이 높으면 규격 재조정 또는 공정 개선 필요

Cpk와 함께 계산하는 문제로 자주 나옴

문제 7. 관리도의 이상 패턴 해석 (필기/실기 핵심)
패턴	의미
한 점이 UCL/LCL 벗어남	공정 이상 발생
7개 점 연속 증가/감소	추세(Trend)
7개 점 연속 평균 위/아래	중심 이동(Shift)
주기적 패턴	기계적 문제·환경 요인

실기에서 서술형 문제로 매우 자주 등장한다.

문제 8. R에서 qcc 패키지 활용 (방법2)
🔧 R 코드
library(qcc)

qcc_obj <- qcc(process$measurement, type="xbar")
qcc_obj

📊 해석

관리도 한계 자동 계산

매우 간편하며 실무에서 자주 사용

✔ Chapter 18 요약

관리도(X-bar, R)는 공정의 안정성 확인 도구

공정능력지수 Cp, Cpk는 공정이 규격을 얼마나 잘 만족하는지 판단

Cp > Cpk → 평균 이동 존재

Cpk < 1 → 공정능력 부족

관리도 패턴 해석(Trend, Shift 등)은 실기에서 빈출



Chapter 19. 시계열 분석(Time Series Analysis) 심화**
# Chapter 19. 시계열 분석(Time Series Analysis) 심화

ADP 실기에서 시계열(Time Series)은 항상 출제되는 핵심 영역이다.  
특히 다음 세 가지 유형이 매우 자주 등장한다.

1) ARIMA 모델  
2) 계절성 모델(SARIMA)  
3) 예측 결과 해석 및 평가(MAPE, RMSE 등)  

본 장에서는 R의 `forecast` 패키지를 사용해  
실기형 문제 형식으로 시계열 모델링을 수행한다.

---

## 📂 데이터셋 (ts_sales.csv)

3년치 월간 매출 데이터:

```r
sales <- read.csv(text = "
month,sales
1,200
2,220
3,250
4,270
5,300
6,330
7,350
8,380
9,360
10,400
11,420
12,450
13,210
14,230
15,260
16,290
17,310
18,340
19,370
20,390
21,410
22,430
23,450
24,470
25,220
26,240
27,270
28,300
29,320
30,360
31,380
32,400
33,420
34,450
35,470
36,500
")

문제 1. 시계열 객체(ts) 생성
🔧 R 코드
ts_data <- ts(sales$sales, start=c(1), frequency=12)
ts_data

📊 해석

frequency=12 → 월별 시계열

실기에서 자주 묻는:
“계절성을 갖는 시계열은 frequency=12로 지정한다.”

문제 2. 시계열 분해(Decomposition: Trend, Seasonal, Random)
🔧 R 코드
decomp <- decompose(ts_data, type="additive")
plot(decomp)

📊 해석 포인트

Trend: 장기적인 증가·감소

Seasonal: 매년 반복되는 패턴

Random: 설명되지 않는 잡음

실기에서는 “시계열 구성요소를 설명하라” 문제로 출제된다.

문제 3. ACF/PACF 분석

AR, MA 차수를 판단하는 근거로 활용한다.

🔧 R 코드
acf(ts_data)
pacf(ts_data)

📊 출제 포인트
특징	의미
ACF가 서서히 감소	AR 모델 가능성
ACF가 특정 지점에서 절단	MA 모델 가능성
PACF 절단	AR
ACF 절단	MA
문제 4. 자동 ARIMA 모델 적합(auto.arima)
🔧 R 코드
library(forecast)

fit_arima <- auto.arima(ts_data)
fit_arima

📊 해석

(p, d, q)(P, D, Q)[12] 형식으로 SARIMA도 자동 탐색

실기에서 결과로 나온 ARIMA 계수를 해석하는 문제가 출제됨

예) ARIMA(1,1,1)

→ 1차 자기회귀 + 1차 차분 + 1차 MA 구성

문제 5. ARIMA 모델 예측(Forecast)
🔧 R 코드
fcast <- forecast(fit_arima, h=6)
plot(fcast)
fcast

📊 해석

h=6 → 6개월 예측

예측 구간(PI: Prediction Interval)이 함께 출력됨

실기에서는 “예측치가 얼마인가?” 또는 “구간을 해석하라” 출제

문제 6. 예측 정확도 평가

RMSE, MAE는 실기 필수 계산 문제

🔧 R 코드
actual <- tail(ts_data, 6)     # 실제값
pred   <- fcast$mean           # 예측값

rmse <- sqrt(mean((actual - pred)^2))
mae  <- mean(abs(actual - pred))

rmse; mae

📊 해석

RMSE는 오차 제곱의 평균 → 큰 오차에 민감

MAE는 절대오차 평균 → 직관적

실기에서는 “MAE와 RMSE 중 무엇이 더 큰 오차를 강조하는가?”
→ RMSE

문제 7. 계절성 있는 모델: SARIMA 수동 적합
🔧 R 코드
fit_sarima <- Arima(ts_data, order=c(1,1,1),
                    seasonal=c(1,1,1))
summary(fit_sarima)

📊 해석

계절 차분(1)로 계절 패턴 제거

Seasonal AR(1), Seasonal MA(1) 포함

출제 포인트:
SARIMA = ARIMA + Seasonality Components

문제 8. 정상성(STATIONARITY) 판단

ADF 검정(단위근검정)은 필기 + 실기 함께 출제된다.

🔧 R 코드
library(tseries)
adf.test(ts_data)

📊 해석

p < 0.05 → 정상성 존재

p ≥ 0.05 → 비정상 → 차분 필요

문제 9. 시계열 모델링 실기 시험 자주 나온 질문

Q1. 왜 차분(differencing)을 하는가?
→ 정상성을 확보하기 위해

Q2. ACF 절단 → MA(q) 추정 가능. 왜?
→ MA 모델은 q 시점 이후 상관이 0이 되기 때문

Q3. ARIMA 결과에서 AR1 = 0.8 의미 해석
→ 직전 시점의 영향력이 0.8만큼 반영됨

Q4. 예측 구간 해석
→ 미래값이 해당 구간 내에 있을 확률이 95%

✔ Chapter 19 요약

시계열 분석은 ARIMA·SARIMA 모델 설정이 핵심

정상성 확보 후 ACF/PACF로 차수 추정

auto.arima는 실기에서 가장 빠른 실전 접근

예측 정확도 지표(RMSE / MAE)는 반드시 계산할 줄 알아야 함

분해(Trend/Seasonal/Random) 개념도 서술형으로 자주 출제



Chapter 20. ADP 실기 유형 총정리 + 종합 실전 모의고사**
# Chapter 20. 실기 유형 총정리 + 종합 실전 모의고사

본 장은 ADP 실기에서 반복적으로 등장하는 문제 유형을 정리하고,  
마지막으로 실제 시험 난이도와 유사한 “종합 실전 모의고사”를 제공한다.

---

# 1. ADP 실기 자주 출제되는 10대 유형

| 번호 | 유형 | 설명 |
|------|-------|------|
| 1 | 데이터 전처리 | NA 처리, 이상치 처리, 파생변수 생성 |
| 2 | 기초 통계량 | 요약, 그룹별 비교, t-test, ANOVA |
| 3 | 회귀분석 | 다중회귀, 변수선택, VIF, 해석 |
| 4 | 로지스틱 회귀 | 오즈비, AUC, Confusion Matrix |
| 5 | 의사결정나무/랜덤포레스트 | 변수 중요도, 분기 기준 |
| 6 | 클러스터 분석 | K-means, 실루엣 계수, 군집 해석 |
| 7 | PCA | 주성분 설명력, 고유값 |
| 8 | 시계열 | ARIMA, 차분, 예측 |
| 9 | 텍스트 마이닝 | 전처리, TF-IDF, 감성분석 |
| 10 | 품질관리 | 관리도, Cp/Cpk |

---

# 2. 실기 필수 R 함수 총정리

### 📌 전처리

```r
is.na(), na.omit(), mutate(), filter(), select()

📌 모델링
lm(), glm(), randomForest(), kmeans(), prcomp(), auto.arima()

📌 그래프
ggplot(), geom_line(), geom_point(), boxplot(), plot()

📌 평가
confusionMatrix(), accuracy(), RMSE(), AUC()

3. 실전 데이터 제공 (final_exam.csv)
final <- read.csv(text = "
id,age,gender,income,purchase,score,region
1,25,F,3200,1,80,East
2,45,M,5500,0,60,West
3,33,F,4000,1,75,South
4,50,M,6200,0,55,East
5,29,F,3500,1,78,North
6,41,M,4800,0,62,West
7,37,F,4200,1,82,North
8,52,M,6800,0,58,South
9,31,F,3900,1,74,East
10,47,M,5100,0,61,West
")

4. 종합 실전 모의고사 (ADP 실기 스타일)

아래 15문항은 실제 ADP 실기 형식을 반영해 구성된 문제들이다.
직접 R로 풀이해보면 실전 대비 효과가 매우 크다.

📘 문제 1. 결측치 처리

income에서 결측치가 있다고 가정할 때, 중앙값으로 대체하는 R 코드를 작성하라.

📘 문제 2. 파생변수 생성

소비성향 변수 propensity = score / income 을 생성하라.

📘 문제 3. 그룹별 평균

지역(region)별 평균 score 를 계산하는 R 코드를 작성하라.

📘 문제 4. t-test

남녀 간 score 차이가 유의한지 t-test로 검정하라.

📘 문제 5. 회귀모형

score ~ age + income + gender 모델을 추정하고 해석하라.

📘 문제 6. 다중공선성 검사

VIF를 계산하라.

📘 문제 7. 로지스틱 회귀

purchase 를 종속변수로 로지스틱 회귀를 수행하라.
오즈비(OR)를 해석하라.

📘 문제 8. 의사결정나무

purchase 예측을 위한 decision tree 를 생성하고
가장 중요한 변수는 무엇인지 답하라.

📘 문제 9. 군집 분석

age, income, score 를 사용하여 K=2 군집 분석을 수행하라.

📘 문제 10. PCA

PCA 수행 후 PC1이 전체 분산의 몇 %를 설명하는지 계산하라.

📘 문제 11. 시계열

매출 데이터를 시계열로 설정할 때 frequency 를 설명하라.

📘 문제 12. 텍스트 마이닝

다음 문장에서 stopword 를 제거하는 R 코드를 작성하라.

"Data science is an exciting and rapidly growing field"

📘 문제 13. 품질관리

Cp, Cpk 의 차이를 설명하고, Cp > Cpk 상황을 해석하라.

📘 문제 14. 예측 모델 평가

ROC AUC 의 의미를 서술하라.

📘 문제 15. Confusion Matrix 해석

Accuracy 와 Recall 의 차이를 설명하라.

5. 종합 실전 모의고사 해설 (요약)
문제 1
final$income[is.na(final$income)] <- median(final$income, na.rm=TRUE)

문제 2
final$propensity <- final$score / final$income

문제 3
aggregate(score ~ region, data=final, mean)

문제 4
t.test(score ~ gender, data=final)

문제 5
summary(lm(score ~ age + income + gender, data=final))

문제 6
library(car)
vif(lm(score ~ age + income + gender, data=final))

문제 7
glm(purchase ~ age + income + score, data=final, family="binomial")

문제 8
library(rpart)
rpart(purchase ~ ., data=final)

문제 9
kmeans(final[, c("age","income","score")], centers=2)

문제 10
summary(prcomp(final[, c("age","income","score")], scale.=TRUE))

문제 11

월별 데이터이므로 frequency = 12

문제 12
library(tm)
removeWords("Data science is an exciting and rapidly growing field", stopwords("en"))

문제 13

Cp = 잠재능력
Cpk = 평균 이동 고려한 실제 능력
Cp > Cpk → 평균이 치우쳐 있음

문제 14

AUC는 모델이 분류를 얼마나 잘 구분하는지 측정한 지표
1.0에 가까울수록 우수

문제 15

Accuracy = 전체 정답 비율

Recall = 실제 양성 중에서 모델이 잡아낸 비율

✔ Chapter 20 요약

ADP 실기에서 반복되는 핵심 유형 10가지를 총정리

실제 출제되는 수준과 유사한 종합 실전 문제 제공

전처리 → 모델링 → 평가 → 품질관리 → 시계열까지 실전 대비 완성
