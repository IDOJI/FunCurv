# 🟨 FPCA + Demographics ===========================================================================================



# 🟨 FPCA만 사용한 경우 ===========================================================================================

path_data = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/FunImgARCWSF_FC/AAL3"


path_fold_data = list.files(path_data, pattern = "fold", full.names = T)
path_full_train_data = list.files(path_data, pattern = "train", full.names = T)


# Fitting model for each 

for(path_ith_fold in path_full_train_data){
  # path_ith_fold = path_full_train_data[1]
  ith_fold = readRDS(path_ith_fold)
  
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









###############################################################################
# 1.1.Loading packages
###############################################################################
# which_OS = Mac = "/Users/Ido/"
which_OS = Windows = "C:/Users/lleii/"
require(dplyr)
require(tidyverse)
require(fda)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)










data = read.csv("/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv")
data = data %>% filter(NFQ___BAND.TYPE=="SB")

# SEX
data$DEMO___SEX %>% table



# AGE of total
data$DEMO___AGE %>% sd
data$DEMO___AGE %>% mean

# AGE of each sex
age_F = data %>% filter(DEMO___SEX == "Female") %>% dplyr::select(DEMO___AGE) %>% unlist
age_M = data %>% filter(DEMO___SEX == "Male") %>% dplyr::select(DEMO___AGE) %>% unlist

mean(age_F)
sd(age_F)

mean(age_M)
sd(age_M)


# diagnosis
data$DEMO___DIAGNOSIS_NEW %>% table

Atlas_Labels[grep("Hippo", Atlas_Labels)]




#===============================================================================
# [Classification_1] msgl: Multinomial Sparse Group Lasso
#===============================================================================
path_Export = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/5.Classification___MSGL___0.9"

Classification___Multinomial___MSGL = function(Train_X, 
                                               Train_y,
                                               Test_X,
                                               Test_y,
                                               features.grouping,
                                               n_fold=5, 
                                               alpha_seq = seq(0, 1, by = 0.01), 
                                               lambda.min = 0.01, 
                                               d=100,
                                               lambda.min.rel=TRUE,
                                               standardize=FALSE,
                                               sparse.data = FALSE, 
                                               parallel.cores = 8,
                                               path_Export){
  #=============================================================================
  # Description
  #=============================================================================
  # Reference : Sparse group lasso and high dimensional multinomial classification 
  # Arguments
  # features.grouping : coefficient groups indicators
  # sparse.data : TRUE = X will be treated as sparse
  
  
  
  
  
  
  #=============================================================================
  # Test data
  #=============================================================================
  # "ROI___AAL3___FunImgARCWSF___Z.Standardization___SB"
  # i=7
  
  # "ROI___AAL3___FunImgARglobalCWSF___Z.Standardization___SB"
  # i=15
  # Train_X = Data_Splitted[[i]]$Train_Set$X
  # Train_y = Data_Splitted[[i]]$Train_Set$y
  # 
  # Test_X = Data_Splitted[[i]]$Test_Set$X
  # Test_y = Data_Splitted[[i]]$Test_Set$y

  # features.grouping = Data_Splitted[[i]]$Coef_Groups_Ind
  
  
  
  
  
  
  
  #=============================================================================
  # Fitting CV
  #=============================================================================
  Fit_CV.list = Classification___Multinomial___MSGL___Fitting.by.CV(X = Train_X,
                                                                    y = Train_y,
                                                                    features.grouping,
                                                                    n_fold,
                                                                    alpha_seq,
                                                                    lambda.min,
                                                                    d,
                                                                    lambda.min.rel,
                                                                    sparse.data,
                                                                    standardize,
                                                                    parallel.cores,
                                                                    path_Export)
  
  
  
  
  
  All_Fit_CV 
  
  
  
  #=============================================================================
  # Find the best model from CV list
  #=============================================================================
  Best_CV = Classification___Multinomial___MSGL___Select.Best.Model(Fit_CV.list, alpha_seq)
  
  
  
  
  #=============================================================================
  # Final fitting
  #=============================================================================
  sglOptim::get_coef(Best_CV$Best_CV)
  sglOptim::models(Best_CV$Best_CV)
  sglOptim::sgl_print(Best_CV$Best_CV)
   (Best_CV$Best_CV$)
  
  Final_Fit = msgl::fit(x = Train_X,
                        classes = Train_y,
                        sampleWeights = NULL,
                        grouping = features.grouping,
                        groupWeights = NULL,
                        parameterWeights = NULL,
                        alpha = Best_CV,
                        standardize = ,lambda = ,d = ,return_indices = ,intercept = ,sparse.data = ,algorithm.config)
    fit(x, classes, sampleWeights = NULL, grouping = NULL,
        groupWeights = NULL, parameterWeights = NULL, alpha = 0.5,
        standardize = TRUE, lambda, d = 100, return_indices = NULL,
        intercept = TRUE, sparse.data = is(x, "sparseMatrix"),
        algorithm.config = msgl.standard.config)
    
    
  
  
  
  # count the number of nonzero features in each model
  # Parmeters = parameters(fit)
  # N_Parmeters = sapply(features(fit), length)
  
    
    list(model.info = fit.cv, CV.Error = CV.Error)
  
  
  #=========================================================================
  # Prediction in Test_set
  #=========================================================================
  Classification___Multinomial___MSGL___Prediction = function(Test_X, Test_y, fitted_model){
    fitted_model = 
    
  }
  
  
  Classification___Multinomial___MSGL___Prediction(Test_X, Test_y, fitted_model){
    
    
  }
  
}



predict


# 각 람다에 대해 선택된 features


fit <- msgl::fit(X, y, alpha = .5, lambda = lambda)

# the nonzero features of model 1, 10 and 25
features(fit)[c(1,10,25)]



sglOptim::features()


data(SimData)


lambda <- msgl::lambda(x, classes, alpha = .5, d = 50, lambda.min = 0.05)
fit <- msgl::fit(x, classes, alpha = .5, lambda = lambda)

# the nonzero parameters of model 1, 10 and 25
parameters(fit)[c(1,10,25)]

# count the number of nonzero parameters in each model
sapply(parameters(fit), sum)


coef(fit)
features.msgl(fit.cv)




















