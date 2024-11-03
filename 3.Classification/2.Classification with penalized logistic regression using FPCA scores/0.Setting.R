# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

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
List.list[[1]] = visual = c("crayon", "ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2", "PRROC", "pROC")
List.list[[2]] = stat = c("ROCR", "MLmetrics", "fda", "MASS", "caret", "pROC", "grpreg")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr", "readr", "caret", "readxl")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr")
List.list[[11]] = modeling = c("grpreg")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)

filter = dplyr::filter
select = dplyr::select



set_output_path = function(input_path) {
  # 운영체제 확인
  sys_name = Sys.info()["sysname"]
  
  # 경로 앞부분 변경
  if (sys_name == "Windows") {
    output_path = sub("^/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", "E:", input_path)
  } else if (sys_name == "Darwin") {  # macOS의 sysname은 'Darwin'입니다.
    output_path = sub("^E:", "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", input_path)
  } else {
    stop("지원되지 않는 운영체제입니다.")
  }
  
  return(output_path)
}


get_file_names_without_extension = function(path_data, pattern = "fold") {
  # 파일 목록 불러오기
  names_fold_data = list.files(path_data, pattern = pattern, full.names = FALSE)
  
  # 파일 확장자 제거하고 이름만 추출
  file_names = tools::file_path_sans_ext(names_fold_data)
  
  # 결과 반환
  return(file_names)
}


# 🟩 grpreg ====================================================================================================
## 🟥 ith fold : train & test =============================================================================================
train_test_grpreg = function(fpca_scores_train, 
                           fpca_scores_test, 
                           fpca_scores_rep,
                           ith_fold_name,
                           alpha,
                           lambda,
                           penalty){
  ith_fold = ith_fold_name
  ### 🟨 Extract data ==============================================================
  ith_fold_train = fpca_scores_train[[ith_fold]]
  ith_fold_test = fpca_scores_test[[ith_fold]]
  ith_fold_rep = paste0("ROI_", sprintf("%03d", fpca_scores_rep[[ith_fold]]))
  
  
  
  
  ### 🟨 Prepare the input ==============================================================
  dx_conversion = function(DX){
    DX = factor(DX, levels = c("Dementia", "MCI", "CN"))
    if(length(unique(DX)) == 2){
      if("Dementia" %in% DX){
        y = ifelse(DX == "Dementia", 1, 0)
      }else if("MCI" %in% DX){
        y = ifelse(DX == "MCI", 1, 0)
      }  
    }else{
      stop(" The number of groups are more than 2!")
    }
    y = factor(y, c(1,0))
    
    return(y)
  }
  ith_y_train = ith_fold_train$DX %>% dx_conversion()
  ith_y_test = ith_fold_test$DX %>% dx_conversion()
  
  ith_X_train = ith_fold_train %>% select(-RID, -DX)
  ith_X_test = ith_fold_test %>% select(-RID, -DX)
  
  
  
  ### 🟨 결과 저장 리스트 ==============================================================
  results = list()
  
  
  
  ### 🟨 Fitting the model ==============================================================
  # grpreg 모델 적합
  results$fitted_model = fit <- grpreg(X = ith_X_train %>% as.matrix, 
                                       y = ith_y_train %>% as.character, 
                                       group = ith_fold_rep, 
                                       alpha = alpha,
                                       lambda = lambda,
                                       penalty = penalty, 
                                       family = "binomial")
  
  
  
  
  
  
  ### 🟨 Predict the results ==============================================================
  results$predicted_responses =ith_predicted_responses = predict(fit, ith_X_test %>% as.matrix, type="response")
  
  
  
  
  
  
  ### 🟨 PR-AUC ==============================================================
  # 모델이 intercept만을 사용했는지 확인
  coef_values <- coef(fit)[-1]  # Intercept를 제외한 계수 값들
  
  if (all(coef_values == 0)) {
    
    results$PR_AUC <- "Only Intercept"
    
  } else {
    # PR-AUC 계산
    pr_auc_value <- PRAUC(y_pred = ith_predicted_responses, y_true = ith_y_test)
    
    # Precision-Recall Curve 생성
    pred <- ROCR::prediction(ith_predicted_responses, ith_y_test)
    perf <- ROCR::performance(pred, measure = "prec", x.measure = "rec") # (x축: recall, y축: precision)
    
    # PR Curve 데이터프레임 생성, NA 제거
    pr_df <- data.frame(
      recall = perf@x.values[[1]],
      precision = perf@y.values[[1]]
    ) %>% na.omit()  # NA 제거
    
    
    # 유효한 PR Curve 데이터가 있는지 확인
    if (nrow(pr_df) > 1) {
      # PR Curve 플로팅
      pr_plot <- ggplot(pr_df, aes(x = recall, y = precision)) +
        geom_line(color = "blue") +
        labs(title = paste("Precision-Recall Curve (PR-AUC =", round(pr_auc_value, 3), ")"),
             x = "Recall", y = "Precision") +
        theme_minimal()
      
      
      # 리스트에 PR-AUC 및 플롯 추가
      results$PR_AUC <- pr_auc_value
      results$PR_Plot <- pr_plot
    } else {
      # 데이터가 충분하지 않아 PR Curve를 그릴 수 없는 경우
      results$PR_AUC <- pr_auc_value
      results$PR_Plot <- "The data is not enough for PR Curve."
    }
  }
  return(results)
}



## 🟥 every fold : train & test =============================================================================================
find_opt_model_by_cv_grpreg <- function(fpca_scores_train, 
                                        fpca_scores_test, 
                                        fpca_scores_rep,
                                        alphas, 
                                        lambdas, 
                                        penalty) {
  
  # 결과 저장을 위한 리스트 초기화
  best_pr_auc <- -Inf  # 최대화할 값으로 초기화
  optimal_models <- list()  # 최적 조합을 저장할 리스트
  all_results <- list()  # 모든 조합의 PR AUC 결과를 저장할 리스트
  
  # 각 알파와 람다 조합에 대해 교차 검증 수행
  for (alpha in alphas) {
    for (lambda in lambdas) {
      
      # 조합 시작 시간 기록
      start_time <- Sys.time()
      
      # 각 fold에 대한 PR AUC 값을 저장할 리스트 초기화
      pr_auc_list <- list()
      grpreg_results <- list()
      
      # 각 fold에 대해 모델 학습 및 평가
      for (ith_fold in names(fpca_scores_train)) {
        
        # train_test_grpreg 함수 호출 시 에러가 발생할 경우 건너뛰도록 tryCatch 사용
        ith_results_grpreg <- tryCatch({
          train_test_grpreg(
            fpca_scores_train,
            fpca_scores_test,
            fpca_scores_rep,
            ith_fold,
            alpha,
            lambda,
            penalty  # 예시로 그룹 라쏘 사용
          )
        }, error = function(e) {
          message(red$bold(paste("Error encountered for alpha:", alpha, "and lambda:", lambda, "- Skipping this combination.")))
          return(NULL)  # 에러 발생 시 NULL 반환
        })
        
        # 에러가 발생하지 않은 경우에만 PR AUC를 저장
        if (!is.null(ith_results_grpreg) && ith_results_grpreg$PR_AUC != "Only Intercept") {
          pr_auc_list[[ith_fold]] <- ith_results_grpreg$PR_AUC
          grpreg_results[[ith_fold]] <- ith_results_grpreg
        }
      }
      
      # PR AUC가 계산된 fold가 있는 경우에만 평균 계산
      if (length(pr_auc_list) == length(fpca_scores_train)) {
        mean_pr_auc <- pr_auc_list %>% unlist() %>% mean()
        
        # 모든 조합의 PR AUC 결과 저장
        all_results[[paste("alpha", alpha, "lambda", lambda, sep = "_")]] <- list(
          pr_auc_values = pr_auc_list,
          mean_pr_auc = mean_pr_auc
        )
        
        # 최적 모델 업데이트 (최대 PR AUC 기준)
        if (mean_pr_auc > best_pr_auc) {
          best_pr_auc <- mean_pr_auc
          optimal_models <- list()  # 새로운 최적 값이 발견되면 리스트 초기화
          optimal_models[[paste("alpha", alpha, "lambda", lambda, sep = "_")]] <- list(
            best_model = grpreg_results,
            best_alpha = alpha,
            best_lambda = lambda,
            best_pr_auc = mean_pr_auc
          )
        } else if (mean_pr_auc == best_pr_auc) {
          # 동일한 mean_pr_auc 값을 갖는 조합을 리스트에 추가
          optimal_models[[paste("alpha", alpha, "lambda", lambda, sep = "_")]] <- list(
            best_model = grpreg_results,
            best_alpha = alpha,
            best_lambda = lambda,
            best_pr_auc = mean_pr_auc
          )
        }
      }
      
      # 조합 종료 시간 기록 및 소요 시간 계산
      end_time <- Sys.time()
      elapsed_time <- end_time - start_time
      
      # 소요 시간 출력
      cat(green$bold("Alpha:"), alpha, yellow$bold("Lambda:"), lambda,
          blue$bold("Elapsed Time:"), as.numeric(elapsed_time), "seconds\n")
      
    }
  }
  
  # 최적 모델 정보와 모든 조합의 PR AUC 결과 반환
  return(list(optimal_models = optimal_models, all_results = all_results))
}










