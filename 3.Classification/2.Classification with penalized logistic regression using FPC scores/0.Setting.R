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





# 🟧 Sub-functions =========================================================================
extract_dignosis = function(path_subjects_train = NULL, 
                            path_subjects_test = NULL,
                            sub_train = NULL,
                            sub_test = NULL,
                            which_group_positive = "Dementia"){
  
  if(is.null(sub_train) && is.null(sub_test) && !is.null(path_subjects_train) && !is.null(path_subjects_test)){
    sub_train = readRDS(path_subjects_train)
    sub_test = readRDS(path_subjects_test)  
  }
  
  
  combined_results = list()
  
  combined_results$diagnosis_train = as.numeric(sub_train$DIAGNOSIS_FINAL == which_group_positive) %>% setNames(sub_train$RID)
  combined_results$diagnosis_test = as.numeric(sub_test$DIAGNOSIS_FINAL == which_group_positive) %>% setNames(sub_test$RID)
  combined_results$which_group_positive = which_group_positive
  
  return(combined_results)
}


extract_fpca_scores_and_group_numbers = function(path_fpca_train_test = NULL, 
                                                 fpca_train_test = NULL){

  if(is.null(fpca_train_test)){
    fpca_train_test = readRDS(path_fpca_train_test)  
  }  
  
  combined_results_list = list()
  
  train_scores_list = list()
  test_scores_list = list()
  train_numbers = list()
  test_numbers = list()
  
  for(i in seq_along(fpca_train_test)){
    ith_roi = fpca_train_test[[i]]
    train_scores_list[[i]] = ith_roi$fpca_scores_train %>% dplyr::select(-RID)
    test_scores_list[[i]] = ith_roi$fpca_scores_valid %>% dplyr::select(-RID)
    
    train_numbers[[i]] = rep(i, ncol(train_scores_list[[i]]))
    test_numbers[[i]] = rep(i, ncol(test_scores_list[[i]]))
  }
  
  combined_results_list[["train_scores"]] = do.call(bind_cols, train_scores_list) %>% cbind(RID = fpca_train_test$ROI_001$fpca_scores_train$RID, .)
  combined_results_list[["test_scores"]] = do.call(bind_cols, test_scores_list) %>% cbind(RID = fpca_train_test$ROI_001$fpca_scores_valid$RID, .)
  combined_results_list[["train_numbers"]] = train_numbers %>% unlist
  combined_results_list[["test_numbers"]] = test_numbers %>% unlist
  
  if(all(combined_results_list[["train_numbers"]] == combined_results_list[["test_numbers"]])){
    combined_results_list[["train_numbers"]] = combined_results_list[["test_numbers"]] = NULL
    combined_results_list[["group_numbers"]] = test_numbers %>% unlist
  }else{
    stop("the group numbers between train and test are different!")
  }
  
  
  return(combined_results_list)
}


# ROC_AUC와 PR_AUC 각각에 대해 결과 요약 데이터프레임을 추출하는 함수
# ROC_AUC와 PR_AUC 각각에 대해 결과 요약 데이터프레임을 추출하는 함수
extract_summary_metrics = function(fold_results) {
  
  # Initialize lists to store ROC_AUC and PR_AUC for each hyperparameter combination
  
  # Iterate over each fold
  ROC_AUC = list()
  PR_AUC = list()
  F1_Score = list()
  
  for (fold in names(fold_results)) {
    # fold = names(fold_results)[1]
    each_fold_roc_auc_list = list()
    each_fold_pr_auc_list = list()
    each_fold_F1score_list = list()
    
    
    # Iterate over each hyperparameter combination in the fold
    for (param_name in names(fold_results[[fold]])) {
      # param_name = names(fold_results[[fold]])[100]
      selected_param_combination = fold_results[[fold]][[param_name]]
      
      # Check if ROC_AUC exists, otherwise add NA
      each_fold_roc_auc_list[[param_name]] = selected_param_combination$ROC_AUC
      each_fold_pr_auc_list[[param_name]] = selected_param_combination$PR_AUC
      each_fold_F1score_list[[param_name]] = selected_param_combination$F1_Score
    }
    
    
    ROC_AUC[[fold]] = each_fold_roc_auc_list %>% 
      sapply(function(x) {
        x %>% unlist() %>% as.numeric()
      }) %>%
      as.data.frame() %>%
      setNames(paste0(fold, "_ROC_AUC")) %>%
      as_tibble()
    
    PR_AUC[[fold]] = each_fold_pr_auc_list %>%
      sapply(function(x) {
        x %>% unlist() %>% as.numeric()
      }) %>%
      as.data.frame() %>%
      setNames(paste0(fold, "_PR_AUC")) %>%
      as_tibble()
    
    F1_Score[[fold]] = each_fold_F1score_list %>%
      sapply(function(x) {
        x %>% unlist() %>% as.numeric()
      }) %>%
      as.data.frame() %>%
      setNames(paste0(fold, "_F1_Score")) %>%
      as_tibble()
      
    
  }# Fold
  
  
  mean_sd_by_row = function(data_list){
    data_list %>% 
      do.call(cbind, .) %>% 
      rowwise() %>%
      filter(all(!is.na(c_across(everything())))) %>%
      mutate(
        Mean = mean(c_across(everything()), na.rm = TRUE),
        SD = sd(c_across(everything()), na.rm = TRUE)
      ) %>%
      ungroup()
  }
  
  
  F1_Score_df = F1_Score %>% mean_sd_by_row
  ROC_AUC_df = ROC_AUC %>% mean_sd_by_row 
  PR_AUC_df = PR_AUC %>% mean_sd_by_row 
  
  
  return(list(ROC_AUC = ROC_AUC_df, 
              PR_AUC = PR_AUC_df,
              F1_Score = F1_Score_df))
}



# 🟨 Optimal model ============================================================================
extract_optimal_results = function(metrics){
  
  values = sapply(results, function(x) {
    if (is.list(x) && measure %in% names(x)) {
      x[[measure]]
    } else {
      NULL
    }
  }) %>% unlist
  
  max_value = max(values)
  ind = which(values == max_value)
  
  list(max_value = max_value, which_max_ind = ind) %>% return
}







# 🟩 penalized logistic regression ========================================================================================
# penalized_logistic 함수 정의
penalized_logistic = function(train_X, train_y, test_X, test_y, alpha, lambda, plotting = FALSE){
  
  # Save results
  results_list = list()
  results_list[["y_real"]] = test_y
  results_list[["hyper-para_alpha"]] = alpha
  results_list[["hyper-para_lambda"]] = lambda
  
  # Model fitting
  results_list[["fitted_model"]] = fit = glmnet(x = as.matrix(train_X), y = train_y, 
                                                  family = "binomial", alpha = alpha, 
                                                  lambda = lambda)
  results_list[["pred_prob"]] = pred_prob = as.numeric(predict(fit, newx = as.matrix(test_X), 
                                                                 type = "response"))
  
  # Check if all predicted probabilities are the same
  if (length(unique(pred_prob)) != 1) {
    # Calculate AUC
    results_list[["ROC_curve"]] = roc_curve = roc(test_y, pred_prob, quiet = T)
    results_list[["ROC_AUC"]] = auc_value = auc(roc_curve) %>% unlist %>% as.numeric
    
    # Calculate PR-AUC
    results_list[["PR_curve"]] = pr_curve = pr.curve(scores.class0 = pred_prob[test_y == 1], 
                                                       scores.class1 = pred_prob[test_y == 0], 
                                                       curve = TRUE)
    results_list[["PR_AUC"]] = pr_curve$auc.integral
    
    # Convert probabilities to binary predictions using 0.5 threshold
    pred_class = ifelse(pred_prob >= 0.5, 1, 0)
    
    # Calculate Precision, Recall, and F1 Score
    tp = sum(pred_class == 1 & test_y == 1)
    fp = sum(pred_class == 1 & test_y == 0)
    fn = sum(pred_class == 0 & test_y == 1)
    
    precision = ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall = ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    f1_score = ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), NA)
    
    results_list[["F1_Score"]] = f1_score
    
    # ROC Curve 캡처하여 저장
    if(plotting){
      plot(roc_curve, main = "ROC Curve")
      results_list[["ROC_curve_plot"]] = recordPlot()  # 현재 plot을 캡처하여 저장
      
      # PR Curve 캡처하여 저장
      plot(pr_curve, main = "Precision-Recall Curve")
      results_list[["PR_curve_plot"]] = recordPlot()   # 현재 plot을 캡처하여 저장  
    }
  }else{
    
    results_list[["ROC_curve"]] = NA
    results_list[["ROC_AUC"]] = NA
    results_list[["PR_curve"]] = NA
    results_list[["PR_AUC"]] = NA 
    results_list[["F1_Score"]] = NA
    
  }
  return(results_list)
}











# 🟩 Group penalty logistic regression =========================================================================================================
group_penalized_logistic = function(train_X, train_y, test_X, test_y, groups, 
                               lambda = NULL, alpha, 
                               plotting = FALSE){
  
  # Save results
  results_list = list()
  results_list[["y_real"]] = test_y
  results_list[["hyper-para_lambda"]] = lambda
  
  # Model fitting using grpreg
  if(is.null(lambda)){
    results_list[["fitted_model"]] = fit = grpreg(X = as.matrix(train_X), 
                                                    y = train_y, 
                                                    group = groups, 
                                                    penalty = "grLasso", 
                                                    family = "binomial",
                                                    alpha = alpha)  
  }else{
    results_list[["fitted_model"]] = fit = grpreg(X = as.matrix(train_X), 
                                                    y = train_y, 
                                                    group = groups, 
                                                    penalty = "grLasso", 
                                                    family = "binomial", 
                                                    lambda = lambda,
                                                    alpha = alpha)
  }
  
  
  
  # Prediction on test set
  results_list[["pred_prob"]] = pred_prob = as.numeric(predict(fit, as.matrix(test_X), type = "response"))
  
  # Check if all predicted probabilities are the same
  if (length(unique(pred_prob)) != 1) {
    # Calculate AUC
    results_list[["ROC_curve"]] = roc_curve = roc(test_y, pred_prob, quiet = TRUE)
    results_list[["ROC_AUC"]] = auc_value = as.numeric(auc(roc_curve))
    
    # Calculate PR-AUC
    results_list[["PR_curve"]] = pr_curve = pr.curve(scores.class0 = pred_prob[test_y == 1], 
                                                       scores.class1 = pred_prob[test_y == 0], 
                                                       curve = TRUE)
    results_list[["PR_AUC"]] = pr_curve$auc.integral
    
    # Convert probabilities to binary predictions using 0.5 threshold
    pred_class = ifelse(pred_prob >= 0.5, 1, 0)
    
    # Calculate Precision, Recall, and F1 Score
    tp = sum(pred_class == 1 & test_y == 1)
    fp = sum(pred_class == 1 & test_y == 0)
    fn = sum(pred_class == 0 & test_y == 1)
    
    precision = ifelse((tp + fp) > 0, tp / (tp + fp), 0)
    recall = ifelse((tp + fn) > 0, tp / (tp + fn), 0)
    f1_score = ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), NA)
    
    results_list[["F1_Score"]] = f1_score
    
    # ROC Curve capture
    if(plotting){
      plot(roc_curve, main = "ROC Curve")
      results_list[["ROC_curve_plot"]] = recordPlot()  # Store ROC plot
      
      # PR Curve capture
      plot(pr_curve, main = "Precision-Recall Curve")
      results_list[["PR_curve_plot"]] = recordPlot()   # Store PR plot
    }
  } else {
    # Assign NA if all predicted probabilities are the same
    results_list[["ROC_curve"]] = NA
    results_list[["ROC_AUC"]] = NA
    results_list[["PR_curve"]] = NA
    results_list[["PR_AUC"]] = NA
    results_list[["F1_Score"]] = NA
  }
  
  return(results_list)
}









# 🟪 Grid and Fold ======================================================================================================
penalized_logistic_grid = function(train_X, train_y, groups = NULL, alphas, lambdas = NULL, test_X, test_y, plotting = FALSE) {
  library(crayon)  
  results = list()
  
  for (alpha in alphas) {
    # alpha = alphas[1]
    for (lambda in lambdas %||% c(NULL)) {  # lambdas가 NULL일 때 NA로 설정
      # lambda = lambdas[1]
      # 각 조합을 고유한 이름으로 사용
      param_name = if (is.null(lambda)) {
        sprintf("alpha_%.4f_lambda_NULL", round(alpha, 4))
      } else {
        sprintf("alpha_%.4f_lambda_%.4f", round(alpha, 4), round(lambda, 4))
      }
      
      # 실행 시간 측정 시작
      start_time = Sys.time()
      
      # 에러가 발생하면 현재 alpha와 lambda 값을 출력하고 함수 중단
      tryCatch({
        # 하이퍼파라미터 조합에 대해 penalized_logistic 함수 호출
        if (is.null(groups)) {
          results[[param_name]] = penalized_logistic(
            train_X = train_X,
            train_y = train_y,
            test_X = test_X,
            test_y = test_y,
            alpha = alpha,
            lambda = lambda,
            plotting = plotting
          )  
        } else {
          # lambda가 NULL일 때, group_penalized_logistic에서 자동 설정
          results[[param_name]] = group_penalized_logistic(
            train_X = train_X,
            train_y = train_y,
            test_X = test_X,
            test_y = test_y,
            groups = groups,
            alpha = alpha,
            lambda = if (is.null(lambda)) NULL else lambda,
            plotting = plotting
          )
        }
        
        # 실행 시간 측정 종료
        end_time = Sys.time()
        elapsed_time = round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)
        
        # 결과 메시지 출력
        cat(green("Completed"), "with alpha =", yellow(alpha), 
            if (!is.null(lambda)) paste("and lambda =", yellow(lambda)), 
            "in", blue(elapsed_time), "seconds.\n")
        
      }, error = function(e) {
        cat(red("Error occurred with alpha =", alpha), 
            if (!is.null(lambda)) paste("and lambda =", lambda), "\n")
        print(e)  # 에러 메시지 출력
        results[[param_name]] = NA
      })
    }
  }
  
  return(results)
}

penalized_logistic_grid_fold = function(path_data, 
                                         path_splitted_subjects,
                                         group_penalty = TRUE,
                                         alphas,
                                         lambdas,
                                         path_save = NULL){
  
  # Define final results save path
  path_final_results = file.path(path_save, basename(path_data), paste0("Classification", 
                                                                ifelse(group_penalty, "_GroupPen", "_Pen", "_Train"), 
                                                                ".rds"))
  
  # Check if final results file already exists; if so, load and return
  if (file.exists(path_final_results)) {
    
    cat(green("Final results file already exists. Loading saved results.\n"))
    
    return(readRDS(path_final_results))
    
  }
  
  target_groups = strsplit(basename(path_data), "___")[[1]][1]
  
  path_fold = list.files(path_data, pattern = "Fold_", full.names = TRUE)
  
  # Load subjects list
  subjects = file.path(path_splitted_subjects, target_groups) %>% 
    list.files(pattern = "train_seed", full.names = TRUE) %>% 
    readRDS()
  
  fold_results = list()
  
  path_fold_save = file.path(path_save, basename(path_data))
  
  dir.create(path_fold_save, showWarnings = FALSE, recursive = TRUE)
  
  # Model fitting
  for (k in seq_along(path_fold)) {
    
    # Define the save path for each fold result
    path_fold_save_file = file.path(path_fold_save, paste0(k, "th_fold.rds"))
    
    # Check if fold result file exists; if so, load and skip
    if (file.exists(path_fold_save_file)) {
      
      fold_results[[paste0(k, "th_fold")]] = readRDS(path_fold_save_file)
      
      cat(green(sprintf("Fold %d already exists. Loaded saved results.\n", k)))
      
      next  # Skip to the next iteration
    }
    
    # Sub list
    sub_train = subjects[[paste0("Fold_", k, "_Train")]]
    sub_valid = subjects[[paste0("Fold_", k, "_Validation")]]
    
    # Extract Diagnosis
    diagnosis = extract_dignosis(
      sub_train = sub_train,
      sub_test = sub_valid,
      which_group_positive = ifelse(grepl("AD", target_groups), "Dementia",
                                    ifelse(grepl("MCI", target_groups), "MCI", "CN"))
    )
    
    # Extract PC scores
    kth_fold_scores = extract_fpca_scores_and_group_numbers(path_fold[k])
    
    # Define groups if group_penalty is enabled
    groups_return = function(group_penalty, group_numbers) {
      if (group_penalty) return(group_numbers) else return(NULL)
    }
    
    # Logistic regression with penalized grid search
    fold_results[[paste0(k, "th_fold")]] = penalized_logistic_grid(
      train_X = kth_fold_scores$train_scores %>% dplyr::select(-RID) %>% as.matrix(),
      train_y = diagnosis$diagnosis_train,
      alphas = alphas,
      lambdas = lambdas,
      test_X = kth_fold_scores$test_scores %>% dplyr::select(-RID) %>% as.matrix(),
      test_y = diagnosis$diagnosis_test,
      groups = groups_return(group_penalty, kth_fold_scores$group_numbers),
      plotting = FALSE
    )
    
    # Save the results for the current fold
    saveRDS(fold_results[[paste0(k, "th_fold")]], path_fold_save_file)
    
    # Completion message for each fold
    cat(red(sprintf("Fold %d has completed.\n", k)))
  }
  
  # Find intersection of available parameter combinations across all folds
  for (k in seq_along(fold_results)) {
    if (k == 1) {
      combinations = names(fold_results[[k]])
    } else {
      combinations = intersect(combinations, names(fold_results[[k]]))
    }
  }
  
  # Filter results to only keep intersecting combinations
  fold_results_filtered = lapply(fold_results, function(x) x[names(x) %in% combinations])
  
  # Calculate summary metrics
  results = extract_summary_metrics(fold_results_filtered)
  final_results = list(fitting_results = fold_results_filtered, metrics = results)
  
  # Save final results
  if (!is.null(path_save)) {
    # Save final results
    saveRDS(final_results, path_final_results)
    
    # Print completion message
    cat(green("Final results have been successfully saved to:"), path_final_results, "\n")
  }
  
  # Delete temporary fold result files
  unlink(path_fold_save, recursive = TRUE, force = TRUE)
  
  return(final_results)
}





