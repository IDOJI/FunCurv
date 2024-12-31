# 🟩 Load data =========================================================================================================
path_data_list = "/Users/Ido/Library/CloudStorage/Dropbox/✴️DataAnalysis/FunCurv/3.Classification/1.FPCA"
path_all_data_list = list.files(path_data_list, full.names = T)
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/2.Classification with penalized logistic regression using FPCA scores/1.Group Penalty"







# 🟨 Group Penalty =========================================================================================================
# alphas = seq(0.1, 1, by = 0.1)  # alpha: 0.1에서 1까지 0.1 간격
# lambdas = 10^seq(3, -3, length.out = 100)  # lambda: log scale에서 100개의 값
# penalties = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP")
# 
# for(path_ith_data in path_all_data_list){
#   # path_ith_data = path_all_data_list[2]
#   ith_data = basename(path_ith_data)
#   path_save_new = file.path(path_save, ith_data)
#   dir.create(path_save_new, showWarnings = F)
#   exported_file = file.path(path_save_new, "classification results.rds")
#   if(file.exists(exported_file)){
#     next
#   }
#   
#   # Load the data
#   path_ith_data_fpca = list.files(path_ith_data, full.names = T, pattern = "fpca_scores")
#   fpca_scores = readRDS(path_ith_data_fpca)
#   fpca_scores_train = fpca_scores$train
#   fpca_scores_test = fpca_scores$test
#   fpca_scores_rep = fpca_scores$rep
#   
#   # Model fitting
#   opt_models_list = lapply(penalties, function(penalty){
#     find_opt_model_by_cv_grpreg(fpca_scores_train,
#                                 fpca_scores_test,
#                                 fpca_scores_rep,
#                                 alphas,
#                                 lambdas,
#                                 penalty = penalty)
#     
#   }) %>% setNames(penalties)
#   
#   
#   # save the results
#   saveRDS(opt_models_list, exported_file)
#   
# }





# 🟨 PROAUC 결과 추출 =========================================================================================================
# 필요한 패키지 로드
library(readr)
library(dplyr)
library(purrr)
library(writexl)

# 각 데이터 경로 불러오기
path_all_measures <- list.files(path_save, full.names = TRUE)

## 🟧 결과 추출 ================================================================
# 결과를 저장할 리스트 초기화
result_list <- list()

 
# 각 데이터 경로에 대해 반복 수행
for (path_ith_measure in path_all_measures) {
  path_ith_results <- list.files(path_ith_measure, full.names = TRUE)
  
  ith_PRAUC <- lapply(seq(path_ith_results), function(j) {
    ij_result <- readRDS(path_ith_results[j])
    
    # 각 방법론의 PRAUC 값을 추출
    ij_PRAUC <- lapply(ij_result$optimal_models, function(x) {
      return(x$best_pr_auc)
    }) %>% setNames(names(ij_result$optimal_models))
    
    return(ij_PRAUC)
  }) %>% setNames(tools::file_path_sans_ext(basename(path_ith_results)))
  
  # 데이터 프레임 형태로 변환하여 결과 리스트에 추가
  result_list[[basename(path_ith_measure)]] <- ith_PRAUC
}




## 🟧 최댓값만 확인  ================================================================
result_list %>% unlist %>% max
head(result_list)



## 🟧 결과 합치기 ================================================================
# 결과를 합쳐 테이블 형식으로 변환 (데이터셋 이름을 첫 번째 열로 추가)
combined_df <- map_dfr(names(result_list), function(dataset_name) {
  data <- result_list[[dataset_name]]
  map_dfr(names(data), function(method) {
    tibble(
      Dataset = dataset_name,
      Method = method,
      Hyperparameters = names(data[[method]]),
      PRAUC = unlist(data[[method]])
    )
  })
})


# View(combined_df)



## 🟧 결과 내보내기 ===================================================================
# 엑셀 파일 생성 및 시트 추가
wb <- createWorkbook()
addWorksheet(wb, "CombinedResults")

# 데이터프레임을 엑셀 시트에 작성
writeData(wb, "CombinedResults", combined_df)

# 최댓값인 PRAUC 행 찾기
max_prauc_row <- which.max(combined_df$PRAUC)

# max PRAUC 행에 색상 적용 (노란색)
highlight_style <- createStyle(bgFill = "yellow")
addStyle(
  wb, "CombinedResults",
  style = highlight_style,
  rows = max_prauc_row + 1, # 헤더가 있으므로 +1
  cols = 1:ncol(combined_df),
  gridExpand = TRUE
)

# 엑셀 파일 저장
path_export <- "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/2.Classification with penalized logistic regression using FPCA scores"
saveWorkbook(wb, file.path(path_export, "PRAUC_results.xlsx"), overwrite = TRUE)



## 🟩 최댓값 모형을 다시 적합해서 여러 값들 추출===========================================
set_label <- function(vector) {
  # "AD"가 존재하면, "AD"를 1로, 나머지를 0으로 설정
  if ("Dementia" %in% vector) {
    return(ifelse(vector == "Dementia", 1, 0))
  }
  
  # "AD"가 없고 "MCI"가 존재하면, "MCI"를 1로, 나머지를 0으로 설정
  if ("MCI" %in% vector) {
    return(ifelse(vector == "MCI", 1, 0))
  }
  
  # "AD"와 "MCI"가 모두 없으면, 모든 값을 0으로 설정
  return(rep(0, length(vector)))
}

a = 0.1
l = 0.174752840000768
pen = "grLasso"
# fpca = readRDS("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/MCI_CN___FunImgARglobalCWSF_Fisher Z FC/fpca_scores.rds")

# 필요한 패키지 로드
library(dplyr)
library(grpreg)
library(pROC)  # AUC 계산을 위해 pROC 패키지 사용
library(caret) # 민감도, 특이도, 정확도 계산을 위해 caret 패키지 사용

# 결과를 저장할 리스트 초기화
results_list <- list()

# 각 fold에 대해 반복 수행
for (k in seq_along(fpca$train)) {
  # k번째 fold의 train/test 데이터 설정
  kth_train <- fpca$train[[k]]
  kth_test <- fpca$test[[k]]
  
  # 모델 학습
  kth_results <- grpreg(
    X = kth_train %>% dplyr::select(-RID, -DX) %>% as.matrix, 
    y = kth_train %>% pull(DX) %>% set_label,
    group = fpca$rep[[k]], 
    penalty = pen,
    lambda = l,
    alpha = a
  )
  
  predictions <- predict(kth_results, kth_test %>% dplyr::select(-RID, -DX) %>% as.matrix, type = "response")
  actuals <- kth_test %>% pull(DX) %>% set_label
  
  # ROC 및 AUC 계산
  roc_obj <- roc(actuals, predictions)
  auc_value <- auc(roc_obj)
  
  # 이진 예측 (임계값 0.5 적용)
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  
  # 혼동 행렬 생성
  confusion_matrix <- confusionMatrix(
    factor(predicted_class, levels = c(0, 1)),
    factor(actuals, levels = c(0, 1))
  )
  
  # 민감도, 특이도, 정확도 추출
  sensitivity <- confusion_matrix$byClass["Sensitivity"]
  specificity <- confusion_matrix$byClass["Specificity"]
  accuracy <- confusion_matrix$overall["Accuracy"]
  
  # fold 결과 저장
  results_list[[k]] <- list(
    AUC = auc_value,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Accuracy = accuracy
  )
}
# fold별 결과를 데이터프레임으로 변환 후 평균 계산
results_df <- do.call(rbind, results_list) %>% as.data.frame()

# 데이터프레임의 모든 열을 numeric 타입으로 변환
results_df <- results_df %>% mutate(across(everything(), as.numeric))

# 각 메트릭의 평균값 계산
average_results <- colMeans(results_df, na.rm = TRUE)

# 최종 결과 출력
average_results









