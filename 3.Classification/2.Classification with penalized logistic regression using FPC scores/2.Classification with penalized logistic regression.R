# 🟨 FPCA + Demographics ===========================================================================================
## 🟩 Non-group penalty ================================================================================================


## 🟩 Group penalty ================================================================================================




# 🟨 FPCA만 사용한 경우 ===========================================================================================
## 🟩 Non-group penalty ================================================================================================




## 🟩 Group penalty ================================================================================================
path_data = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/FunImgARCWSF_FC/AAL3"


path_fold_data = list.files(path_data, pattern = "fold", full.names = T)
path_full_train_data = list.files(path_data, pattern = "train", full.names = T)
path_test_data = list.files(path_data, pattern = "test", full.names = T)

# Fitting model for each 

for(path_ith_fold in path_full_train_data){
  # path_ith_fold = path_full_train_data[1]
  ith_fold = readRDS(path_ith_fold)
  ith_fold$FPCA_ROI$ROI_001$selected_scores
  tmp = readRDS("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/FunImgARCWSF_zALFF/AAL3/fold_1_result.rds")
  
  
  combined_data = lapply(ith_fold, function(x){
    # x = ith_fold[[1]]
  })
  
  # 그룹 번호 매기기
  
  # ROI 추출
  
  # 전체 리스트에서 diagnosis 추출
  
  # model fitting (alpha + lambda) : AD:CN / AD:MCI / MCI:CN
  
  # validation으로 모델 performance 계산
  
  
}

# performance 결과 averaging


# 최종 결과 내보내기

# 성능 최적 하이퍼파라미터 선택

# test 데이터로 최종 성능 추출










# 데이터 준비
data(Birthwt)
X <- Birthwt$X    # 예측 변수
y <- Birthwt$low  # 이진 반응 변수
group <- Birthwt$group  # 그룹 정보

# 함수 호출
results <- fit_multiple_penalties(
  X = X,
  y = y,
  group = group,
  family = "binomial",
  alpha = 0.8,
  save_plots = TRUE,
  plot_dir = "my_plots",
  plot_names = c("grLasso" = "Lasso_Plot", "grMCP" = "MCP_Plot", "grSCAD" = "SCAD_Plot"),
  save_results = TRUE,
  results_filename = "model_results.rds"
)



# 함수 사용 예시
data(Birthwt)
X <- Birthwt$X    # Predictor variables
y <- Birthwt$low  # Binary response variable (low birth weight)
group <- Birthwt$group  # Grouping for the predictors

# 함수 호출
results <- fit_multiple_penalties(X, 
                                  y, 
                                  group, 
                                  family = "binomial", 
                                  save_plots = TRUE, 
                                  plot_dir = "my_plots")


























