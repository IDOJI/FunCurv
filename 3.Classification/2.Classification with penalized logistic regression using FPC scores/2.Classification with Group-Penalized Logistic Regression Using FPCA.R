# 🟨 FPCA + Demographics ===========================================================================================
## 🟩 Non-group penalty ================================================================================================


## 🟩 Group penalty ================================================================================================




# 🟨 FPCA만 사용한 경우 ===========================================================================================
## 🟩 Non-group penalty ================================================================================================




## 🟩 Group penalty ================================================================================================
lambdas = exp(seq(-6, 0, length.out = 100))
alphas = seq(0, 1, length.out = 20)
alphas = alphas[alphas!=0]

path_data = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/AD, CN___FunImgARglobalCWSF_Fisher Z FC"
path_data = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/AD, CN___FunImgARglobalCWSF_zReHo"
path_splitted_subjects = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/2.Classification with penalized logistic regression using FPC scores"

group_penalty = TRUE
penalized_logistic_train_test = function(path_data, 
                                         path_splitted_subjects,
                                         group_penalty,
                                         alphas,
                                         lambdas,
                                         path_save){
  
  # Fold
  fold_results = penalized_logistic_grid_fold(path_data, 
                                              path_splitted_subjects,
                                              group_penalty = TRUE,
                                              alphas,
                                              lambdas, 
                                              path_save)
  
  # Find Optimal Combination
  extract_optimal_results = function(fold_results){
    
    create_alpha_lambda_dataframe <- function(input_vector) {
      # 정규 표현식을 사용하여 alpha와 lambda 값을 추출
      alpha_values <- as.numeric(sub(".*alpha_([0-9.]+)_lambda_.*", "\\1", input_vector))
      lambda_values <- as.numeric(sub(".*lambda_([0-9.]+)", "\\1", input_vector))
      
      # 데이터프레임 생성
      df <- data.frame(alpha = alpha_values, lambda = lambda_values)
      return(df)
    }
    hyperparameters_combination = fold_results$fitting_results[[1]] %>% names
    hyper_parameters_df = create_alpha_lambda_dataframe(hyperparameters_combination)
    
    metrics = fold_results$metrics
    
    optimal_metrics = list()
    
    
    # ROC AUC
    ROC_AUC_mean = metrics$ROC_AUC$Mean
    ROC_AUC_mean_max = ROC_AUC_mean %>% max
    ind_ROC_AUC_mean_max = which(ROC_AUC_mean == ROC_AUC_mean_max)
    selected_ROC_AUC_max = metrics$ROC_AUC[ind_ROC_AUC_mean_max, ] 
    if(length(ind_ROC_AUC_mean_max) > 1){
      selected_ROC_AUC_max = selected_ROC_AUC_max[which.min(selected_ROC_AUC_max$SD), ]
    }
    optimal_metrics[["ROC_AUC_max"]] = selected_ROC_AUC_max
    hyper_parameters_df
    
    # PR AUC
    PR_AUC_mean = metrics$PR_AUC$Mean
    ind_PR_AUC_mean_max = which.max(PR_AUC_mean)
    selected_PR_AUC_max = metrics$PR_AUC[ind_PR_AUC_mean_max, ] 
    if(length(ind_PR_AUC_mean_max) > 1){
      selected_PR_AUC_max = selected_PR_AUC_max[which.min(selected_PR_AUC_max$SD), ]
    }
    optimal_metrics[["PR_AUC_max"]] = cbind(selected_PR_AUC_max, ind)
    
    # F1 score
    if(nrow(metrics$F1_Score) > 1){
      F1_Score_mean = metrics$F1_Score$Mean
      ind_F1_Score_mean_max = which.max(F1_Score_mean)
      selected_F1_Score_max = metrics$F1_Score[ind_F1_Score_mean_max,]
      optimal_metrics[["F1_Score_max"]] = selected_F1_Score_max
    }  
    
    return(optimal_metrics)
  }
  
  
  names(metrics)
  metrics$F1_Score
  extract_optimal_results
  
  # Test results
  
  
  # Return
  
  
    
}















