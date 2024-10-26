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












# 🟥 Basic functions =========================================================================================================
## 🟨 RID 변경
change_rid = function(rid){
  sprintf("RID_%04d", rid)
}


## 🟨 선택 ROI개수 벡터 
the_number_of_repeated_roi = function(df){
  roi_numbers <- as.numeric(sub("ROI_(\\d+)_FPC_\\d+", "\\1", colnames(df)))
  return(roi_numbers)
}

## 🟨 경로 자동 변환 함수 정의
convert_path <- function(input_path) {
  if (.Platform$OS.type == "windows") {
    # macOS 경로를 Windows 경로로 변환
    return(gsub("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", "E:", input_path))
  } else {
    # Windows 경로를 macOS 경로로 변환
    return(gsub("E:", "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", input_path))
  }
}




# 🟥 sub-functions for FPCA =========================================================================================================
## 🟨 smoothing 결과에서 특정 RID 데이터만 추출
extract_smoothed_fd <- function(fd_obj, rid){
  rid_indices = rid %>% sort %>% change_rid()
  
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




# 🟥 FPCA main functions  ============================================================================================
## 🟩 1) 주어진 데이터에서 FPCA를 수행하는 함수 ============================================================================================
FPCA = function(fd_obj, 
                initial_nharm = 50,
                portion = 0.9){
  # 데이터에 대해 FPCA 수행
  fpca_results <- pca.fd(fd_obj, nharm = initial_nharm, centerfns = TRUE)
  
  # 누적 분산 비율 계산 및 필요한 harmonic 개수 선택
  cumulative_variance <- cumsum(fpca_results$varprop)
  selected_harm <- which(cumulative_variance >= portion)[1]
  
  # 필요한 harmonic과 score 추출 (Train 데이터)
  selected_harmonics <- fpca_results$harmonics[1:selected_harm]
  fpc_scores <- as.data.frame(fpca_results$scores[, 1:selected_harm])
  colnames(fpc_scores) <- paste0("FPC_", seq_len(ncol(fpc_scores)))
  
  combined_results = list()
  combined_results[["fpca_results"]] = fpca_results
  combined_results[["selected_fpc_scores"]] = fpc_scores
  combined_results[["selected_harmonics"]] = selected_harmonics
  
  return(combined_results)
}

  


## 🟩 2) 다른 FPCA harmonics에 따라 FPC score를 구하는 함수 ============================================================================================
FPCA_scores = function(fd_obj, fpca_results){
  
  # Validation 데이터 중심화 (Train 데이터의 평균 함수 사용)
  centered_test_fd <- subtract_fd_mean(
    fd_obj = fd_obj, 
    mean_fd = fpca_results$fpca_results$meanfd
  )
  
  # Validation 데이터의 FPC 점수 계산
  fpc_scores <- inprod(centered_test_fd, fpca_results$selected_harmonics)
  
  # 필요한 harmonic 개수만 선택
  colnames(fpc_scores) <- paste0("FPC_", seq_len(ncol(fpc_scores)))

  return(fpc_scores)
}



# 🟪 FPCA for train & valid  ============================================================================================
FPCA_train_valid = function(fd_obj, 
                            train_rid, 
                            valid_rid = NULL,
                            fpc_name = NULL){
  # RID sorting
  train_rid = train_rid %>% sort
  valid_rid = valid_rid %>% sort
  
  # Results
  combined_results = list()
  
  # Train
  train_fd_obj = extract_smoothed_fd(fd_obj, train_rid)
  train_fpca_results = FPCA(fd_obj = train_fd_obj)
  combined_results[["train_fpca"]] = train_fpca_results
  combined_results[["fpca_scores_train"]] = train_fpca_results$selected_fpc_scores %>% 
    as.data.frame %>% 
    setNames(paste0(fpc_name, "_", names(.))) %>% 
    mutate(RID = train_rid) %>% 
    relocate(RID)
  
  
  # Valid
  if(!is.null(valid_rid)){
    valid_fd_obj = extract_smoothed_fd(fd_obj, valid_rid)
    valid_fpca_scores = FPCA_scores(fd_obj = valid_fd_obj, fpca_results = train_fpca_results)
    combined_results[["fpca_scores_valid"]] = valid_fpca_scores %>% 
      as.data.frame %>% 
      setNames(paste0(fpc_name, "_", names(.))) %>% 
      mutate(RID = valid_rid) %>% 
      relocate(RID)
  }
  return(combined_results)
}


# 🟪 FPCA for every ROI  ============================================================================================
FPCA_train_valid_every_roi = function(smoothed_results, 
                                      train_rid, 
                                      valid_rid = NULL, 
                                      path_save = NULL, 
                                      file_name = NULL){
  library(crayon)
  library(tictoc)
  
  train_rid = train_rid %>% sort
  valid_rid = valid_rid %>% sort
  
  # 최종 파일 경로 설정
  if (!is.null(path_save) && !is.null(file_name)) {
    final_file_path = file.path(path_save, paste0(file_name, ".rds"))
    
    # 최종 파일이 존재하면 계산을 건너뛰고 불러옴
    if (file.exists(final_file_path)) {
      cat(green("[INFO] "), yellow("Final results file already exists at "), cyan(final_file_path), "\n")
      return(readRDS(final_file_path))
    }
  }
  
  results = list()  # 결과를 저장할 리스트 초기화
  
  for (roi_name in names(smoothed_results)) {
    # roi_name = names(smoothed_results)[1]
    # 개별 ROI 파일 경로 생성
    if (!is.null(path_save)) {
      
      path_save_tmp = file.path(path_save, file_name)
      
      dir.create(path_save_tmp, showWarnings = F, recursive = T)
      
      roi_file_path = file.path(path_save_tmp, paste0(roi_name, ".rds"))
      
    }
    
    # 시간 측정 시작
    tic(roi_name)
    
    if (!is.null(path_save) && file.exists(roi_file_path)) {
      
      # 파일이 존재하면 건너뛰고 파일을 불러옴
      cat(green("[INFO] "), yellow("Skipping calculation for "), cyan(roi_name), yellow(": file already exists.\n"))
      ith_result <- readRDS(roi_file_path)
      
    } else {
      # FPCA 계산 진행
      cat(green("[INFO] "), yellow("Starting FPCA calculation for "), cyan(roi_name), "\n")
      ith_result <- FPCA_train_valid(fd_obj = smoothed_results[[roi_name]]$fdSmooth_obj$fd, 
                                     train_rid, 
                                     valid_rid,
                                     fpc_name = roi_name)
      
      # 결과 파일 저장 (path_export가 NULL이 아닌 경우에만)
      if (!is.null(path_save)) {
        saveRDS(ith_result, file = roi_file_path)
        cat(green("[INFO] "), yellow("Saved FPCA result for "), cyan(roi_name), yellow(" to "), cyan(roi_file_path), "\n")
      }
    }
    
    # 결과 리스트에 저장
    results[[roi_name]] <- ith_result
    
    # 시간 측정 종료 및 표시
    cat(green("[TIME] "), yellow("Time taken for "), cyan(roi_name), ":\n")
    toc()
  }
  
  # 최종 결과 저장 및 개별 ROI 파일 삭제
  if (!is.null(path_save) && !is.null(final_file_path)) {
    # 최종 결과 저장
    saveRDS(results, final_file_path)
    cat(green("[INFO] "), yellow("Saved final FPCA results to "), cyan(final_file_path), "\n")
    
    # 폴더 자체 삭제
    if (dir.exists(path_save_tmp)) {
      unlink(path_save_tmp, recursive = TRUE)
      cat(green("[INFO] "), yellow("Deleted folder "), cyan(path_save_tmp), "\n")
    }
  }
  return(results)
}


# 🟪 FPCA for all data  ============================================================================================
apply_FPCA_to_all_measures = function(path_all_splitted_subjects,
                                      path_all_smoothed_results,
                                      path_export,
                                      atlas_name = "AAL3"){
  
  paths_target_groups = list.files(path_all_splitted_subjects, full.names = T)
  paths_target_measures = list.files(path_all_smoothed_results, full.names = T)
  
  for(path_target_groups in paths_target_groups){
    # path_target_groups = paths_target_groups[1]
    target_groups = basename(path_target_groups)
    
    for(path_target_measure in paths_target_measures){
      # path_target_measure = paths_target_measures[1]
      target_measure = basename(path_target_measure)
      
      path_target_export = file.path(path_export, paste0(target_groups, "___", target_measure))
      
      smoothed_results = path_target_measure %>% 
        file.path(atlas_name) %>% 
        list.files(full.names=T, pattern = "\\.rds$") %>% 
        readRDS()
      
      
    
      # Subjects lists
      all_train_subjects = path_target_groups %>% 
        list.files(pattern = "all_train", full.names = T) %>% 
        readRDS
      test_subjects = path_target_groups %>% 
        list.files(pattern = "test_seed", full.names = T) %>% 
        readRDS
      train_validation_subjects = path_target_groups %>% 
        list.files(pattern = "train_seed", full.names = T) %>% 
        readRDS
      
      # Combine Resulst
      combined_results = list()
      
      # train & validation
      all_roi_FPCA_train_validation = list()
      for (i in 1:(length(train_validation_subjects)/2)) {
        # i번째 fold의 train과 validation 데이터를 추출
        train_data <- train_validation_subjects[[paste0("Fold_", i, "_Train")]]
        validation_data <- train_validation_subjects[[paste0("Fold_", i, "_Validation")]]
        
        # Train & Validation
        all_roi_FPCA_train_validation[[paste0("Fold_", i)]] = FPCA_train_valid_every_roi(smoothed_results, 
                                                                                         train_rid = train_data$RID, 
                                                                                         valid_rid = validation_data$RID, 
                                                                                         path_save = path_target_export, 
                                                                                         file_name = paste0("FPCA_Train & Validation_Fold_", i, "_",atlas_name))
      }
      combined_results[["FPCA_Train & Validation"]] = all_roi_FPCA_train_validation
      
      
      
      
      
      # All train & Test
      combined_results[["FPCA_All Train"]] = FPCA_train_valid_every_roi(smoothed_results, 
                                                                        train_rid = all_train_subjects$RID, 
                                                                        valid_rid = test_subjects$RID, 
                                                                        path_save = path_target_export, 
                                                                        file_name = paste0("FPCA_Train_and_Test", atlas_name))
    }
    
  }
  
  return(combined_results)
  
}




