# 필요한 패키지 로드
install.packages("readr")
install.packages("pROC")
install.packages("dplyr")
install.packages("openxlsx")

library(readr)
library(pROC)
library(dplyr)
library(openxlsx)

# 데이터 로드
data <- read_csv("C:/Users/owner/Desktop/Program-2_Negative.csv")

# Control 그룹과 PD 그룹으로 데이터 분리
control_data <- data %>% filter(Group == "control")
pd_data <- data %>% filter(Group == "PD")

# 평균 계산 (첫 번째 열은 Group이므로 제외)
control_means <- colMeans(control_data[-1], na.rm = TRUE)
pd_means <- colMeans(pd_data[-1], na.rm = TRUE)

# mz 값 분류
increased_mz <- names(control_means)[which(control_means > pd_means)]
decreased_mz <- names(control_means)[which(control_means < pd_means)]

# AUC 값 저장을 위한 리스트 생성
increased_roc_results <- list()
decreased_roc_results <- list()

# 각각의 mz 값에 대해 ROC 및 AUC 계산
for (mz in names(data)[-1]) {
  # Response 벡터를 이진 형식으로 설정 (두 레벨이 모두 존재하는지 확인)
  response <- factor(data$Group, levels = c("control", "PD"))
  predictor <- data[[mz]]
  
  # NA 값을 포함한 mz 데이터 필터링
  valid_indices <- !is.na(predictor)
  response_valid <- response[valid_indices]
  predictor_valid <- predictor[valid_indices]
  
  # Control 및 PD 그룹이 모두 존재하는지 확인
  if (length(unique(response_valid)) == 2) {
    roc_obj <- roc(response_valid, predictor_valid, levels = c("control", "PD"), quiet = TRUE)
    auc_value <- auc(roc_obj)
    
    # 증가한 mz와 감소한 mz에 따라 결과 저장
    if (mz %in% increased_mz) {
      increased_roc_results[[mz]] <- auc_value
    } else if (mz %in% decreased_mz) {
      decreased_roc_results[[mz]] <- auc_value
    }
  }
}

# 데이터가 제대로 수집됐는지 확인
print(length(increased_roc_results))  # 증가한 mz에 대한 AUC 값 수
print(length(decreased_roc_results))  # 감소한 mz에 대한 AUC 값 수

# 데이터 프레임으로 변환
if (length(increased_roc_results) > 0) {
  increased_roc_results_df <- data.frame(
    mz = names(increased_roc_results),
    AUC = unlist(increased_roc_results)
  )
} else {
  print("No increased mz values found.")
  increased_roc_results_df <- data.frame(mz = character(), AUC = numeric())
}

if (length(decreased_roc_results) > 0) {
  decreased_roc_results_df <- data.frame(
    mz = names(decreased_roc_results),
    AUC = unlist(decreased_roc_results)
  )
} else {
  print("No decreased mz values found.")
  decreased_roc_results_df <- data.frame(mz = character(), AUC = numeric())
}

# Excel 파일로 저장
write.xlsx(increased_roc_results_df, "Increased_mz_ROC_AUC_Results.xlsx", rowNames = FALSE)
write.xlsx(decreased_roc_results_df, "Decreased_mz_ROC_AUC_Results.xlsx", rowNames = FALSE)