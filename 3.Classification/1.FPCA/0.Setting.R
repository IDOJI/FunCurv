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
tmp = readRDS("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/AD, CN___FunImgARCWSF_Fisher Z FC/FPCA_Train_and_TestAAL3.rds")




List.list = list()
List.list[[1]] = visual = c("crayon", "ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2", "PRROC")
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



# 🟩 sub-functions for FPCA =========================================================================================================
## 🟨 smoothing 결과에서 특정 RID 데이터만 추출
extract_smoothed_fd_of_specific_rids <- function(fd_obj, rid){
  rid_indices = paste0("RID_", sprintf("%04d", sort(rid)))
  
  # Extract the relevant smoothed coefficients for the specified subjects
  # fdobj <- fdSmooth_obj$fd
  fdobj = fd_obj
  # Check if all requested rid_indices are present in the fdnames and coefficients
  if(all(rid_indices %in% fdobj$fdnames$reps) && all(rid_indices %in% colnames(fdobj$coefs))) {  
    
    # Extract the smoothed functional data for the specified subjects
    extracted_fd <- fd(coef = fdobj$coefs[, rid_indices], 
                       basisobj = fdobj$basis, 
                       fdnames = list(time = fdobj$fdnames$time, 
                                      reps = rid_indices, 
                                      values = fdobj$fdnames$values))  
    return(extracted_fd)  
  } else {
    # Raise an error if any rid_indices are not found
    stop("Error: One or more specified RID indices are not found in the smoothed results.")
  }
}

## 🟨 FD obj 뺄셈 정의
subtract_fd_mean <- function(fd_obj, mean_fd) {
  # 두 객체의 basis가 동일한지 확인
  if (!identical(fd_obj$basis, mean_fd$basis)) {
    stop("Both fd objects must have the same basis.")
  }
  
  # validation_fd_obj의 계수 행렬과 mean_function의 계수 행렬 가져오기
  coef_obj <- fd_obj$coefs
  mean_coef <- mean_fd$coefs
  
  # mean_function의 계수를 fd_obj의 피험자 수에 맞게 반복 확장
  mean_coef_expanded <- matrix(
    rep(mean_coef, ncol(coef_obj)),
    nrow = nrow(coef_obj),
    ncol = ncol(coef_obj)
  )
  
  # 계수 행렬의 뺄셈 수행
  new_coefs <- coef_obj - mean_coef_expanded
  
  # 새 fd 객체 생성
  result_fd <- fd(coef = new_coefs, 
                  basisobj = fd_obj$basis, 
                  fdnames = fd_obj$fdnames)
  
  return(result_fd)
}




## 🟨 다른 FPCA harmonics에 따라 FPC score를 구하는 함수
extract_fpca_scores_of_test_data = function(fd_obj, pca.fd_obj, nharm){
  
  # Validation 데이터 중심화 (Train 데이터의 평균 함수 사용)
  centered_test_fd <- subtract_fd_mean(
    fd_obj = fd_obj, 
    mean_fd = pca.fd_obj$meanfd
  )
  
  # Validation 데이터의 FPC 점수 계산
  fpc_scores <- inprod(centered_test_fd, pca.fd_obj$harmonics)
  
  # 필요한 harmonic 개수만 선택
  colnames(fpc_scores) <- paste0("FPC_", seq_len(ncol(fpc_scores)))
  
  return(fpc_scores[,1:nharm])
}





# 🟧 Classification by CV =========================================================================
conduct_fpca_on_smoothed_results = function(demographics,
                                            target_diagnosis = c("Dementia", "MCI"),
                                            smoothed_data,
                                            save_path,
                                            fold_seed = 4649,
                                            n_fold = 5){
  ## 🟨 folding data by stratified k-fold CV =====================================================================================
  demographics_new = demographics %>% 
    filter(EPI___BAND.TYPE == "SB") %>% 
    filter(DIAGNOSIS_FINAL %in% target_diagnosis)
  
  
  # stratified k-fold cross-validation 설정
  set.seed(fold_seed)  # 결과 재현성을 위한 시드 설정
  
  # stratified k-fold 분할 생성
  folds <- createFolds(demographics_new$DIAGNOSIS_FINAL, 
                       k = n_fold, 
                       list = TRUE, 
                       returnTrain = TRUE)
  
  
  # 각 폴드에 대해 훈련 및 테스트 데이터를 나누고, 모델링 예시
  folded_data = list()
  for(i in 1:n_fold){
    # i=1
    # 훈련 데이터와 테스트 데이터 나누기
    train_index <- folds[[i]]
    folded_data[[paste0("Fold_", i)]] = list(train_demo = demographics_new[train_index, ], 
                                             test_demo = demographics_new[-train_index, ])
  }
  
  
  
  
  
  
  ## 🟨 Extract smoothed data & apply FPCA =============================================================================
  fpca_train_list = list()
  fpca_scores_train_list = list()
  fpca_scores_test_list = list()
  fpca_scores_rep_list = list()
  
  # 전체 시작 시간
  start_time <- Sys.time()
  
  # for 루프 시작
  for (ith_fold in names(folded_data)) {
    
    # fold별 시작 시간
    fold_start_time <- Sys.time()
    
    ith_fold_demo <- folded_data[[ith_fold]]
    ith_fold_fpca_train <- list()
    ith_fold_fpca_scores_train <- list()
    ith_fold_fpca_scores_test <- list()
    ith_fold_fpca_scores_rep <- c()
    
    for (kth_roi in names(smoothed_data)) {
      
      # ROI별 시작 시간
      roi_start_time <- Sys.time()
      
      # 상태 메시지 출력
      cat(sprintf("Fold: %s, ROI: %s - FPCA 수행 중...\n", ith_fold, kth_roi))
      
      kth_smoothed_data <- smoothed_data[[kth_roi]]
      kth_smoothed_fd <- kth_smoothed_data$fdSmooth_obj$fd
      ith_fold_smoothed_results_train <- extract_smoothed_fd_of_specific_rids(fd_obj = kth_smoothed_fd, rid = ith_fold_demo$train_demo$RID)
      ith_fold_smoothed_results_test <- extract_smoothed_fd_of_specific_rids(fd_obj = kth_smoothed_fd, rid = ith_fold_demo$test_demo$RID)
      
      # check RID
      if (!all(ith_fold_smoothed_results_train$fdnames$reps == sort(ith_fold_smoothed_results_train$fdnames$reps))) {
        stop("!!! check RID")
      }
      if (!all(ith_fold_smoothed_results_test$fdnames$reps == sort(ith_fold_smoothed_results_test$fdnames$reps))) {
        stop("!!! check RID")
      }
      
      # train 데이터에 대해 FPCA 수행
      initial_nharm <- 50
      portion <- 0.9
      ith_fold_fpca_train[[kth_roi]] <- kth_train_fpca_results <- pca.fd(fdobj = ith_fold_smoothed_results_train, nharm = initial_nharm, centerfns = TRUE)
      
      # 누적 분산 비율 계산 및 필요한 harmonic 개수 선택
      cumulative_variance <- cumsum(kth_train_fpca_results$varprop)
      selected_harm <- which(cumulative_variance >= portion)[1]
      
      # 필요한 harmonic과 score 추출 (Train 데이터)
      selected_harmonics <- kth_train_fpca_results$harmonics[1:selected_harm]
      ith_fold_fpca_scores_train[[kth_roi]] <- kth_train_fpc_scores <- as.data.frame(kth_train_fpca_results$scores[, 1:selected_harm])
      colnames(kth_train_fpc_scores) <- paste0(kth_roi, "_FPC_", seq_len(ncol(kth_train_fpc_scores)))
      ith_fold_fpca_scores_rep <- rep(which(names(smoothed_data) %in% kth_roi), times = ncol(kth_train_fpc_scores))
      
      # FPCA scores of test data
      ith_fold_fpca_scores_test[[kth_roi]] <- kth_test_fpc_scores <- extract_fpca_scores_of_test_data(fd_obj = ith_fold_smoothed_results_test, pca.fd_obj = kth_train_fpca_results, nharm = selected_harm)
      
      # ROI별 소요 시간 출력
      roi_end_time <- Sys.time()
      cat(sprintf("Fold: %s, ROI: %s - 완료. 소요 시간: %.2f 초\n", ith_fold, kth_roi, as.numeric(difftime(roi_end_time, roi_start_time, units = "secs"))))
    }
    
    fpca_train_list[[ith_fold]] <- ith_fold_fpca_train
    fpca_scores_train_list[[ith_fold]] <- ith_fold_fpca_scores_train %>% 
      do.call(bind_cols, .) %>% 
      cbind(RID = ith_fold_demo$train_demo$RID, 
            DX = ith_fold_demo$train_demo$DIAGNOSIS_FINAL, .)
    fpca_scores_test_list[[ith_fold]] <- ith_fold_fpca_scores_test %>% 
      do.call(bind_cols, .) %>% 
      cbind(RID = ith_fold_demo$test_demo$RID, 
            DX = ith_fold_demo$test_demo$DIAGNOSIS_FINAL, .)
    fpca_scores_rep_list[[ith_fold]] <- ith_fold_fpca_scores_rep
    
    # fold별 소요 시간 출력
    fold_end_time <- Sys.time()
    cat(sprintf("Fold: %s - 완료. 소요 시간: %.2f 초\n", ith_fold, as.numeric(difftime(fold_end_time, fold_start_time, units = "secs"))))
  }
  
  # 전체 소요 시간 출력
  end_time <- Sys.time()
  cat(sprintf("전체 FPCA 수행 완료. 총 소요 시간: %.2f 초\n", as.numeric(difftime(end_time, start_time, units = "secs"))))
  fpca_scores = list(train = fpca_scores_train_list,
                     test = fpca_scores_test_list,
                     rep = fpca_scores_rep_list)
  
  saveRDS(fpca_scores, file.path(save_path, "fpca_scores.rds"))
  saveRDS(fpca_train_list, file.path(save_path, "fpca_train_results.rds"))
}















