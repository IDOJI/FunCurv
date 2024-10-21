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
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2", "PRROC")
List.list[[2]] = stat = c("fda", "MASS", "caret", "pROC", "grpreg")
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



set_output_path <- function(input_path) {
  # 운영체제 확인
  sys_name <- Sys.info()["sysname"]
  
  # 경로 앞부분 변경
  if (sys_name == "Windows") {
    output_path <- sub("^/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", "E:", input_path)
  } else if (sys_name == "Darwin") {  # macOS의 sysname은 'Darwin'입니다.
    output_path <- sub("^E:", "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", input_path)
  } else {
    stop("지원되지 않는 운영체제입니다.")
  }
  
  return(output_path)
}


get_file_names_without_extension <- function(path_data, pattern = "fold") {
  # 파일 목록 불러오기
  names_fold_data <- list.files(path_data, pattern = pattern, full.names = FALSE)
  
  # 파일 확장자 제거하고 이름만 추출
  file_names <- tools::file_path_sans_ext(names_fold_data)
  
  # 결과 반환
  return(file_names)
}





# 🟥 ordinary logistic with Group penalty =========================================================================================================
## 🟨 fit_multiple_penalties
fit_multiple_penalties <- function(X, y, group, family = "binomial",
                                   penalties = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP"),
                                   alpha = 1, lambda = NULL,
                                   save_plots = TRUE, plot_dir = "plots", plot_names = NULL,
                                   save_results = FALSE, results_filename = "results.rds") {
  
  # 결과를 저장할 리스트 초기화
  results <- list()
  
  # 플롯을 저장할 폴더가 없으면 생성
  if (save_plots && !dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # 각 패널티에 대해 모델 적합
  for (penalty in penalties) {
    # 모델 적합: lambda가 NULL이면 자동으로 설정됨
    fit <- grpreg(X, y, group, penalty = penalty, family = family,
                  alpha = alpha, lambda = lambda)
    
    # BIC를 사용해 최적 모델 선택
    best_fit <- grpreg::select(fit, criterion = "BIC")
    
    # 회귀 계수 추출
    coefs <- coef(best_fit)
    
    # 예측값 및 AUC 계산
    if (family == "binomial") {
      # 예측 확률 계산
      preds <- predict(best_fit, X, type = "response")
      # AUC 계산
      auc_value <- pROC::auc(y, preds)
    } else {
      auc_value <- NA  # 이진 분류가 아닌 경우 AUC 계산 불가
    }
    
    # 결과 저장
    results[[penalty]] <- list(
      fit = fit,
      best_fit = best_fit,
      coefs = coefs,
      auc = auc_value
    )
    
    # trace plot 저장
    if (save_plots) {
      if (is.null(plot_names)) {
        plot_name <- paste0(penalty, "_trace_plot.png")
      } else {
        plot_name <- paste0(plot_names[penalty], ".png")
      }
      png_filename <- file.path(plot_dir, plot_name)
      png(filename = png_filename)
      plot(fit, main = paste0("Trace Plot for ", penalty))
      dev.off()
    }
  }
  
  # 결과를 RDS 파일로 저장
  if (save_results) {
    saveRDS(results, file = results_filename)
  }
  
  # 최종 결과 반환
  return(results)
}


## 🟨 fit_hyperparameters_classification
fit_hyperparameters_classification <- function(X_folds, 
                                               y_folds,
                                               group_folds, 
                                               family = "binomial", 
                                               penalties = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP"),
                                               alphas = c(0.5, 1), lambdas = NULL,
                                               save_plots = TRUE, plot_dir = "hyperparam_plots", 
                                               save_results = TRUE, results_filename = "hyperparam_results.rds") {
  
  # 결과를 저장할 리스트 초기화
  hyperparam_results <- list()
  
  # 플롯을 저장할 폴더가 없으면 생성
  if (save_plots && !dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  
  # 성능 지표들을 저장할 리스트 초기화
  auc_list <- list()
  f1_list <- list()
  precision_list <- list()
  recall_list <- list()
  pr_auc_list <- list()
  hyperparam_labels <- c()
  
  # 각 패널티, alpha, lambda 조합에 대해 모델 적합
  for (penalty in penalties) {
    for (alpha in alphas) {
      # lambda가 NULL이면 자동으로 설정
      lambda_seq <- if (is.null(lambdas)) NULL else lambdas
      # 성능 지표를 저장할 리스트 초기화
      auc_scores <- c()
      f1_scores <- c()
      precision_scores <- c()
      recall_scores <- c()
      pr_auc_scores <- c()
      
      # 각 fold에 대해 모델을 적합하고 성능 평가
      for (i in 1:length(X_folds)) {
        X_train <- X_folds[[i]]$train
        X_val <- X_folds[[i]]$val
        y_train <- y_folds[[i]]$train
        y_val <- y_folds[[i]]$val
        group <- group_folds[[i]]
        
        # 모델 적합
        fit_results <- fit_multiple_penalties(X_train, y_train, group, 
                                              family = family, penalties = penalty, 
                                              alpha = alpha, lambda = lambda_seq, 
                                              save_plots = FALSE, save_results = FALSE)
        
        # Validation 데이터에 대해 예측값 계산
        best_fit <- fit_results[[penalty]]$best_fit
        preds <- predict(best_fit, X_val, type = "response")
        pred_labels <- ifelse(preds > 0.5, 1, 0)  # 임계값 0.5 사용
        
        # 성능 지표 계산
        auc_value <- pROC::auc(y_val, preds)
        pr_curve <- PRROC::pr.curve(scores.class0 = preds, weights.class0 = y_val)
        pr_auc_value <- pr_curve$auc.integral
        
        f1_value <- caret::F1_Score(y_pred = pred_labels, y_true = y_val)
        precision_value <- caret::precision(y_pred = pred_labels, y_true = y_val)
        recall_value <- caret::recall(y_pred = pred_labels, y_true = y_val)
        
        auc_scores <- c(auc_scores, auc_value)
        f1_scores <- c(f1_scores, f1_value)
        precision_scores <- c(precision_scores, precision_value)
        recall_scores <- c(recall_scores, recall_value)
        pr_auc_scores <- c(pr_auc_scores, pr_auc_value)
      }
      
      # 각 hyper parameter 조합에 대한 평균 성능 지표 저장
      avg_auc <- mean(auc_scores)
      avg_f1 <- mean(f1_scores)
      avg_precision <- mean(precision_scores)
      avg_recall <- mean(recall_scores)
      avg_pr_auc <- mean(pr_auc_scores)
      
      # 리스트에 저장
      label <- paste0(penalty, "_alpha_", alpha, if (!is.null(lambda_seq)) paste0("_lambda_", lambda_seq) else "")
      auc_list[[label]] <- avg_auc
      f1_list[[label]] <- avg_f1
      precision_list[[label]] <- avg_precision
      recall_list[[label]] <- avg_recall
      pr_auc_list[[label]] <- avg_pr_auc
      hyperparam_labels <- c(hyperparam_labels, label)
    }
  }
  
  # 각 성능 지표별 플롯 생성
  if (save_plots) {
    # AUC Plot
    png_filename <- file.path(plot_dir, "AUC_performance_plot.png")
    png(filename = png_filename)
    plot(1:length(auc_list), unlist(auc_list), type = "o", col = "blue", xlab = "Hyperparameter Combinations", ylab = "AUC",
         main = "AUC Performance Across Hyperparameter Combinations", xaxt = "n")
    axis(1, at = 1:length(hyperparam_labels), labels = hyperparam_labels, las = 2, cex.axis = 0.7)
    dev.off()
    
    # F1-Score Plot
    png_filename <- file.path(plot_dir, "F1_performance_plot.png")
    png(filename = png_filename)
    plot(1:length(f1_list), unlist(f1_list), type = "o", col = "green", xlab = "Hyperparameter Combinations", ylab = "F1-Score",
         main = "F1-Score Performance Across Hyperparameter Combinations", xaxt = "n")
    axis(1, at = 1:length(hyperparam_labels), labels = hyperparam_labels, las = 2, cex.axis = 0.7)
    dev.off()
    
    # Precision Plot
    png_filename <- file.path(plot_dir, "Precision_performance_plot.png")
    png(filename = png_filename)
    plot(1:length(precision_list), unlist(precision_list), type = "o", col = "red", xlab = "Hyperparameter Combinations", ylab = "Precision",
         main = "Precision Performance Across Hyperparameter Combinations", xaxt = "n")
    axis(1, at = 1:length(hyperparam_labels), labels = hyperparam_labels, las = 2, cex.axis = 0.7)
    dev.off()
    
    # Recall Plot
    png_filename <- file.path(plot_dir, "Recall_performance_plot.png")
    png(filename = png_filename)
    plot(1:length(recall_list), unlist(recall_list), type = "o", col = "purple", xlab = "Hyperparameter Combinations", ylab = "Recall",
         main = "Recall Performance Across Hyperparameter Combinations", xaxt = "n")
    axis(1, at = 1:length(hyperparam_labels), labels = hyperparam_labels, las = 2, cex.axis = 0.7)
    dev.off()
    
    # PR-AUC Plot
    png_filename <- file.path(plot_dir, "PR_AUC_performance_plot.png")
    png(filename = png_filename)
    plot(1:length(pr_auc_list), unlist(pr_auc_list), type = "o", col = "orange", xlab = "Hyperparameter Combinations", ylab = "PR-AUC",
         main = "PR-AUC Performance Across Hyperparameter Combinations", xaxt = "n")
    axis(1, at = 1:length(hyperparam_labels), labels = hyperparam_labels, las = 2, cex.axis = 0.7)
    dev.off()
  }
  
  # 결과를 RDS 파일로 저장
  if (save_results) {
    saveRDS(hyperparam_results, file = results_filename)
  }
  
  return(hyperparam_results)
}

