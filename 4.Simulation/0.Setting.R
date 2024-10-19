# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE, quietly = T)
    }
  }
}

List.list = list()
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2")
List.list[[2]] = stat = c("fda", "MASS", "caret")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr", "readr", "caret", "readxl")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)

## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select




# 🟥 Define smoothing functions =========================================================================================================
# 필요한 패키지 로드
library(dplyr)
library(caret)
library(fda)
library(purrr)
library(crayon)

# 1) 데이터 샘플링 함수
bootstrap_sample <- function(data, sample_size = NULL) {
  if (is.null(sample_size)) {
    sample_size <- nrow(data)
  }
  
  data %>%
    group_by(DIAGNOSIS_FINAL) %>%
    sample_frac(size = sample_size / nrow(data), replace = TRUE) %>%
    ungroup()
}

# 2) 데이터 분할 함수 (Train/Test)
split_data <- function(data, train_ratio = 0.7) {
  set.seed(123)  # 재현성을 위해 시드 설정
  train_index <- createDataPartition(data$DIAGNOSIS_FINAL, p = train_ratio, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  list(train = train_data, test = test_data)
}

# 3) K-Fold 분할 함수
create_folds <- function(train_data, k = 5) {
  set.seed(123)
  folds <- createFolds(train_data$DIAGNOSIS_FINAL, k = k, returnTrain = TRUE)
  fold_list <- lapply(1:k, function(i) {
    validation_index <- setdiff(1:nrow(train_data), folds[[i]])
    list(
      train = train_data[folds[[i]], ],
      validation = train_data[validation_index, ]
    )
  })
  names(fold_list) <- paste0("Fold_", 1:k)
  fold_list
}

# 4) Smoothing 및 FPCA 함수
perform_smoothing_fpca <- function(train_data, validation_data, domain, n_order = 4, n_breaks = NULL, lambda = 1e-4, nharm = 5) {
  # Smoothing
  basis <- create.bspline.basis(rangeval = c(min(domain), max(domain)), norder = n_order, nbasis = ifelse(is.null(n_breaks), length(domain), n_breaks))
  fdPar_obj <- fdPar(basis, Lfdobj = int2Lfd(2), lambda = lambda)
  
  # Train 데이터 smoothing
  train_fd <- smooth.basis(argvals = domain, y = t(train_data), fdParobj = fdPar_obj)$fd
  
  # FPCA
  fpca_result <- pca.fd(train_fd, nharm = nharm, centerfns = TRUE)
  
  # Validation 데이터 smoothing
  validation_fd <- smooth.basis(argvals = domain, y = t(validation_data), fdParobj = fdPar_obj)$fd
  
  # Validation 데이터에서 FPC score 계산
  validation_scores <- inprod(validation_fd, fpca_result$harmonics)
  
  list(
    fpca_result = fpca_result,
    train_scores = fpca_result$scores,
    validation_scores = validation_scores
  )
}

# 5) Functional Logistic Regression 및 성능 평가 함수
evaluate_model <- function(train_scores, train_labels, validation_scores, validation_labels) {
  # 모델 적합
  model <- glm(train_labels ~ ., data = as.data.frame(train_scores), family = binomial)
  
  # 예측
  predictions <- predict(model, newdata = as.data.frame(validation_scores), type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # 성능 평가
  confusion <- confusionMatrix(as.factor(predicted_classes), as.factor(validation_labels))
  performance <- list(
    accuracy = confusion$overall['Accuracy'],
    kappa = confusion$overall['Kappa']
  )
  
  performance
}

# 6) 하이퍼파라미터 튜닝 및 최종 모델 평가 함수
tune_and_evaluate <- function(fold_results, test_scores, test_labels) {
  # 각 fold의 성능 수집
  performances <- map(fold_results, "performance")
  
  # 가장 성능이 좋은 하이퍼파라미터 선택 (예: accuracy 기준)
  best_fold <- which.max(map_dbl(performances, "accuracy"))
  
  # 최적 모델로 테스트 데이터 평가
  best_model <- fold_results[[best_fold]]$model
  predictions <- predict(best_model, newdata = as.data.frame(test_scores), type = "response")
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  # 최종 성능 평가
  confusion <- confusionMatrix(as.factor(predicted_classes), as.factor(test_labels))
  final_performance <- list(
    accuracy = confusion$overall['Accuracy'],
    kappa = confusion$overall['Kappa']
  )
  
  final_performance
}



# 전체 프로세스를 실행하는 메인 함수
run_analysis <- function(subject_data, domain, sample_size = NULL, train_ratio = 0.7, k_folds = 5, n_order = 4, n_breaks = NULL, lambda = 1e-4, nharm = 5) {
  # 1) 부트스트랩 샘플링
  sampled_data <- bootstrap_sample(subject_data, sample_size)
  
  # 2) Train/Test 분할
  split <- split_data(sampled_data, train_ratio)
  train_data <- split$train
  test_data <- split$test
  
  # 레이블 추출
  test_labels <- test_data$DIAGNOSIS_FINAL
  
  # 3) K-Fold 생성
  folds <- create_folds(train_data, k = k_folds)
  
  # 4) 각 Fold에 대해 Smoothing 및 FPCA 수행
  fold_results <- lapply(folds, function(fold) {
    train_fold <- fold$train
    validation_fold <- fold$validation
    
    # 데이터 준비 (예시로 필요한 변수만 추출)
    train_matrix <- as.matrix(train_fold[ , -which(names(train_fold) %in% c("RID", "DIAGNOSIS_FINAL"))])
    validation_matrix <- as.matrix(validation_fold[ , -which(names(validation_fold) %in% c("RID", "DIAGNOSIS_FINAL"))])
    
    # Smoothing 및 FPCA 수행
    fpca <- perform_smoothing_fpca(train_matrix, validation_matrix, domain, n_order, n_breaks, lambda, nharm)
    
    # 5) 모델 적합 및 성능 평가
    performance <- evaluate_model(fpca$train_scores, train_fold$DIAGNOSIS_FINAL, fpca$validation_scores, validation_fold$DIAGNOSIS_FINAL)
    
    list(
      model = glm(train_fold$DIAGNOSIS_FINAL ~ ., data = as.data.frame(fpca$train_scores), family = binomial),
      performance = performance
    )
  })
  
  # 7) 전체 Train 데이터로 FPCA 수행
  total_train_matrix <- as.matrix(train_data[ , -which(names(train_data) %in% c("RID", "DIAGNOSIS_FINAL"))])
  test_matrix <- as.matrix(test_data[ , -which(names(test_data) %in% c("RID", "DIAGNOSIS_FINAL"))])
  
  total_fpca <- perform_smoothing_fpca(total_train_matrix, test_matrix, domain, n_order, n_breaks, lambda, nharm)
  
  # 6) 최적 하이퍼파라미터로 테스트 데이터 평가
  final_performance <- tune_and_evaluate(fold_results, total_fpca$validation_scores, test_labels)
  
  final_performance
}

# 함수 실행 예시
# domain은 관측된 데이터의 도메인 (예: 시간 또는 공간 좌표)
# subject_data는 RID, DIAGNOSIS_FINAL 및 관측 데이터가 포함된 데이터프레임

# 예시 데이터 로드 (사용자의 실제 데이터로 대체해야 함)
# subject_data <- read.csv("your_subject_data.csv")
# domain <- seq(0, 1, length.out = ncol(subject_data) - 2)

# 결과 실행
# result <- run_analysis(subject_data, domain)
# print(result)






