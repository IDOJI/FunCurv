# 🟨 FPCA + Demographics ===========================================================================================
## 🟩 Non-group penalty ================================================================================================


## 🟩 Group penalty ================================================================================================




# 🟨 FPCA만 사용한 경우 ===========================================================================================
## 🟩 Non-group penalty ================================================================================================




## 🟩 Group penalty ================================================================================================
path_data = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/FunImgARCWSF_FC/AAL3"
path_subjects_list = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"

# subjects list
subjects_list = read.csv(path_subjects_list)


# path data list
path_fold_data = list.files(path_data, pattern = "fold", full.names = T)
names_fold_data = get_file_names_without_extension(path_data, "fold")
path_full_train_data = list.files(path_data, pattern = "train", full.names = T)
path_test_data = list.files(path_data, pattern = "test", full.names = T)


# Load Data
fold_data = lapply(path_fold_data, readRDS) %>% setNames(names_fold_data)
train_data = readRDS(path_full_train_data)
test_data = readRDS(path_test_data)


# input composition
X_folds <- list()
y_folds <- list()
group_folds <- list()

for(i in seq_along(fold_data)){
  
  ith_fold = fold_data[[i]]
  
  ith_fold_train_FPC = ith_fold$fold_1_Train_FPC_Scores
  ith_fold_validation_FPC = ith_fold$fold_1_Validation_FPC_Scores
  
  ith_fold_train_subjects = subjects_list %>% 
    filter(paste0("RID_", RID) %in% rownames(ith_fold_train_FPC))
  ith_fold_validation_subjects = subjects_list %>% 
    filter(paste0("RID_", RID) %in% rownames(ith_fold_validation_FPC))
  
  X_folds[[i]] = list(train = ith_fold_train_FPC, val = ith_fold_validation_FPC)
  y_folds[[i]] = list(train = ith_fold_train_subjects$DIAGNOSIS_FINAL)
}



# Fold 데이터 생성 (Train/Validation 데이터 분할)
folds <- 5
for (i in 1:folds) {
  # i=1
  idx <- sample(1:n, size = n/folds)
  X_folds[[i]] <- list(train = X[-idx, ], val = X[idx, ])
  y_folds[[i]] <- list(train = y[-idx], val = y[idx])
  group_folds[[i]] <- group
}


# Hyperparameter 튜닝 함수 실행
results <- fit_hyperparameters_classification(
  X_folds = X_folds,
  y_folds = y_folds,
  group_folds = group_folds,
  family = "binomial",
  penalties = c("grLasso", "grMCP"),
  alphas = c(0.5, 1),
  save_plots = TRUE,
  plot_dir = "hyperparam_performance_plots",  # 플롯을 저장할 디렉토리
  save_results = TRUE,
  results_filename = "hyperparam_tuning_results.rds"  # 결과를 저장할 RDS 파일명
)

# 저장된 RDS 파일 불러오기
loaded_results <- readRDS("hyperparam_tuning_results.rds")

# 결과 확인
print(loaded_results)






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


























