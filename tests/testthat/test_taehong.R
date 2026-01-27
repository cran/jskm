# rm(list = ls())
# 
# library(jskm)      # 내가 수정한 패키지 로드
# library(survival)
# library(survey)
# 
# # 3. 가중치 데이터(Survey Design) 생성
# data(pbc, package = "survival")
# pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
# biasmodel <- glm(randomized ~ age * edema, data = pbc, family = binomial)
# pbc$randprob <- fitted(biasmodel)
# 
# # 가중치가 포함된 설계 객체 생성 (이게 svyjskm의 핵심입니다)
# dpbc <- svydesign(id = ~1, prob = ~randprob, strata = ~edema,
#                   data = subset(pbc, randomized))
# 
# # 4. 가중치 생존 모델 생성 (rx 대신 sex 변수 사용 예시)
# # survey 패키지의 svykm 함수를 사용해야 합니다.
# s1 <- svykm(Surv(time, status > 0) ~ sex, design = dpbc)
# 
# # 5. 결과 확인 (jskm에서 바꾼 UI 디테일들이 svyjskm에도 적용되었는지 확인)
# p_svy <- svyjskm(s1,
#                  design = dpbc,      # 가중치 분석은 design 객체 전달이 중요함
#                  main = "svyjskm Development Test")
# 
# # 6. 그래프 출력
# print(p_svy)

# R 세션을 새로 시작(Ctrl+Shift+F10) 하는 것을 권장합니다.
rm(list = ls())

library(jskm)      # 내가 수정한 패키지 로드
library(survival)
library(survey)
colon_data <- survival::colon

# 생존 모델 생성
fit <- survfit(Surv(time, status) ~ rx, data = colon_data)

long_labels <- c(
  "Control Group with Standard Care and Follow-up",
  "Levamisole Treatment Group (Daily Oral Intake)",
  "Levamisole plus 5-FU Combination Therapy (High Dose)"
)

# 결과 확인
p <- jskm(fit, table = TRUE, data = colon, main = "Development Test")

# 그래프 출력
print(p)