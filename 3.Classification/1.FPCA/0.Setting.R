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




# 🟥 Define smoothing functions =========================================================================================================
library(fda)
library(crayon)
library(tictoc)
## 🟨 선택 ROI개수 벡터 ==========================================================================
the_number_of_repeated_roi = function(df){
  roi_numbers <- as.numeric(sub("ROI_(\\d+)_FPC_\\d+", "\\1", colnames(df)))
  return(roi_numbers)
}

## 🟨 경로 자동 변환 함수 정의 ==========================================================================
convert_path <- function(input_path) {
  if (.Platform$OS.type == "windows") {
    # macOS 경로를 Windows 경로로 변환
    return(gsub("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", "E:", input_path))
  } else {
    # Windows 경로를 macOS 경로로 변환
    return(gsub("E:", "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk", input_path))
  }
}


## 🟨 FD obj 뺄셈 정의 ==========================================================================
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


# 🟥 각 fold ============================================================================================
## 🟨 각 fold의 각 ROI에 FPCA 적용하는 함수 ==========================================================================
perform_fpca_for_fold_each_roi = function(roi_name, 
                                          train_smoothing_results, 
                                          validation_smoothing_results, 
                                          initial_nharm, 
                                          portion, 
                                          atlas_dir, 
                                          fold_name,
                                          export_each_roi = TRUE) {
  # Train 및 Validation 데이터의 fd 객체 가져오기
  train_fd_obj <- train_smoothing_results[[roi_name]]$fdSmooth_obj$fd
  validation_fd_obj <- validation_smoothing_results[[roi_name]]$fdSmooth_obj$fd
  
  # Train 데이터에 대해 FPCA 수행
  fpca_train_results <- pca.fd(train_fd_obj, nharm = initial_nharm, centerfns = TRUE)
  
  # 누적 분산 비율 계산 및 필요한 harmonic 개수 선택
  cumulative_variance <- cumsum(fpca_train_results$varprop)
  selected_harm <- which(cumulative_variance >= portion)[1]
  
  # 필요한 harmonic과 score 추출 (Train 데이터)
  selected_harmonics <- fpca_train_results$harmonics[1:selected_harm]
  train_scores <- as.data.frame(fpca_train_results$scores[, 1:selected_harm])
  colnames(train_scores) <- paste0(roi_name, "_FPC_", seq_len(ncol(train_scores)))
  
  # Validation 데이터 중심화 (Train 데이터의 평균 함수 사용)
  centered_validation_fd <- subtract_fd_mean(
    fd_obj = validation_fd_obj, 
    mean_fd = fpca_train_results$meanfd
  )
  
  # Validation 데이터의 FPC 점수 계산
  validation_scores_full <- inprod(centered_validation_fd, selected_harmonics)
  
  # 필요한 harmonic 개수만 선택
  validation_scores <- as.data.frame(validation_scores_full)
  colnames(validation_scores) <- paste0(roi_name, "_FPC_", seq_len(ncol(validation_scores)))
  
  # 결과를 리스트로 저장
  roi_result <- list(
    fpca_train_results = fpca_train_results,
    harmonics = selected_harmonics,
    train_scores = train_scores,
    validation_scores = validation_scores,
    var_explained = cumulative_variance[selected_harm]
  )
  
  
  # export 파일 경로, 이름
  output_dir <- file.path(atlas_dir, fold_name)
  output_file <- file.path(output_dir, paste0(roi_name, ".rds"))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if(export_each_roi){
    saveRDS(roi_result, output_file)
    print_message(sprintf("Saved FPCA results for %s to %s", roi_name, output_file), crayon::green)  
  }
  
  return(roi_result)
}


## 🟨 각 fold에 FPCA 적용하는 함수 ==========================================================================
perform_fpca_for_fold <- function(train_smoothing_results, 
                                  validation_smoothing_results, 
                                  initial_nharm = 50, 
                                  portion = 0.9, 
                                  atlas_dir, 
                                  fold_name,
                                  export_each_roi = T) {
  
  dir.create(atlas_dir,showWarnings = F, recursive = T)
  
  # 결과를 저장할 리스트 초기화
  roi_results <- list()
  
  # ROI 이름 목록 얻기
  roi_names <- names(train_smoothing_results)
  
  # 각 ROI에 대해 처리
  for (roi_name in roi_names) {
    # roi_name = roi_names[1]
    # ROI 이름 설정 및 처리 시작
    print_message(sprintf("Processing ROI: %s", roi_name), crayon::silver)
    
    # export 파일 경로, 이름
    output_dir <- file.path(atlas_dir, fold_name)
    output_file <- file.path(output_dir, paste0(roi_name, ".rds"))
    
    # 파일이 존재하는 경우 결과를 바로 로드
    if (file.exists(output_file)) {
      roi_results[[roi_name]] <- readRDS(output_file)
      next
    }
    
    # 파일이 존재하지 않으면 FPCA 처리 수행
    roi_results[[roi_name]] <- perform_fpca_for_fold_each_roi(
      roi_name = roi_name,
      train_smoothing_results = train_smoothing_results,
      validation_smoothing_results = validation_smoothing_results,
      initial_nharm = initial_nharm,
      portion = portion,
      atlas_dir = atlas_dir,
      fold_name = fold_name,
      export_each_roi = export_each_roi
    )
  }
  
  
  
  
  return(roi_results)
  
}


## 🟨 메시지 출력 함수 ==========================================================================
print_message <- function(message, color_func = crayon::green) {
  cat(color_func(message), "\n")
}




## 🟨 Extract FPC score of test data ==========================================================================
compute_FPC_scores_of_test <- function(test_smoothing_result_path, 
                                       atlas_dir,
                                       export_test = T) {
  
  # output file name
  path_test_fpca_results <- file.path(atlas_dir, "test_fpca_results.rds")
  if (file.exists(path_test_fpca_results)) {
    cat(crayon::green("test FPCA 결과 파일이 존재합니다.\n"))
    return(NULL)
  }
  
  # Load train results
  total_train_fpc_results_path <- list.files(atlas_dir, "total_train_fpca", full.names = T)
  cat(crayon::blue("Training FPCA results loaded from: "), crayon::yellow(total_train_fpc_results_path), "\n")
  total_train_fpc <- readRDS(total_train_fpc_results_path)
  
  # load smoothing results
  test_smoothing <- readRDS(test_smoothing_result_path)
  cat(crayon::blue("Test smoothing results loaded from: "), crayon::yellow(test_smoothing_result_path), "\n")
  
  # 각 ROI 마다 결과 적용
  total_test_fpca <- lapply(names(test_smoothing), function(roi_name) {
    # roi_name = names(total_train_fpc$Combined_FPCA_ROI)[1]
    
    # Test 데이터 중심화 (Train 데이터의 평균 함수 사용)
    centered_roi_obj_fd <- subtract_fd_mean(
      fd_obj = test_smoothing[[roi_name]]$fdSmooth_obj$fd,
      mean_fd = total_train_fpc$Combined_FPCA_ROI[[roi_name]]$fpca_results$meanfd
    )
    
    # Validation 데이터의 FPC 점수 계산
    test_fpc_scores <- inprod(centered_roi_obj_fd, total_train_fpc$Combined_FPCA_ROI[[roi_name]]$selected_harmonics) %>% 
      as.data.frame() %>% 
      setNames(paste0(roi_name, "_FPC_", seq_len(ncol(.))))
    
    cat(crayon::blue("FPC scores computed for ROI: "), crayon::yellow(roi_name), "\n")
    
    return(test_fpc_scores)
  }) %>% do.call(cbind, .)
  
  rownames(total_test_fpca) = test_smoothing$ROI_001$fdSmooth_obj$y %>% colnames
  
  # ROI 숫자 반복 처리
  repeated_roi = the_number_of_repeated_roi(total_test_fpca)
  
  
  # 결과 저장
  cat(crayon::blue("Saving FPCA results to: "), crayon::yellow(path_test_fpca_results), "\n")
  combined_data = list(total_test_fpca = total_test_fpca, 
                       repeated_ROI = repeated_roi)
  
  if(export_test){
    saveRDS(combined_data, path_test_fpca_results)  
  }
  
  return(combined_data)
  
  cat(crayon::green("FPCA 결과 저장 완료.\n"))
}



## 🟨 각 ROI에 대해 FPCA 수행 및 저장하는 함수 ==========================================================================
process_single_roi = function(roi_obj, roi_name, output_dir, initial_nharm, portion, export_each_roi = TRUE) {
  # 🟩 portion 값 검사: 0과 1 사이에 있는지 확인
  if (portion <= 0 || portion >= 1) {
    stop(
      crayon::red(sprintf(
        "Error: 'portion' must be a value between 0 and 1. Given: %.2f", portion
      ))
    )
  }
  
  # 각 ROI의 결과 저장 경로 설정
  output_file <- file.path(output_dir, paste0(roi_name, ".rds"))
  
  # 이미 파일이 존재하면 계산을 건너뛰기
  if (file.exists(output_file)) {
    print_message(sprintf("Skipping %s: File already exists.", roi_name), crayon::yellow)
    return(readRDS(output_file))
  }
  
  # FPCA 수행
  fd_obj <- roi_obj$fdSmooth_obj$fd
  fpca_results <- pca.fd(fd_obj, nharm = initial_nharm, centerfns = TRUE)
  
  # 누적 분산 비율 계산 및 필요한 harmonic 개수 선택
  cumulative_variance <- cumsum(fpca_results$varprop)
  selected_harm <- which(cumulative_variance >= portion)[1]
  
  # 메시지 출력: 텍스트의 부분별로 다른 색상 적용
  print_message(
    paste0(
      crayon::blue("Selected "), 
      crayon::yellow(selected_harm),
      crayon::blue(" harmonics to explain at least "),
      crayon::magenta(sprintf("%.2f%%", portion * 100)),
      crayon::blue(" of the variance.")
    )
  )
  
  # 필요한 harmonic과 score 추출
  selected_harmonics <- fpca_results$harmonics[1:selected_harm]
  selected_scores <- as.data.frame(fpca_results$scores[, 1:selected_harm])
  colnames(selected_scores) = paste0(roi_name, "_FPC_", seq_len(ncol(selected_scores)))
  rownames(selected_scores) = colnames(roi_obj$fdSmooth_obj$y)
  
  
  # 결과를 리스트로 저장
  result <- list(
    fpca_results = fpca_results,
    selected_harmonics = selected_harmonics,
    selected_scores = selected_scores,
    selected_var_explained = cumulative_variance[selected_harm]
  )
  
  # 결과를 RDS 파일로 저장
  if(export_each_roi){
    saveRDS(result, output_file)  
  }
  
  print_message(sprintf("Saved FPCA results for %s to %s", roi_name, output_file))
  
  return(result)
}


## 🟨 모든 ROI에 대해 FPCA 수행 및 결과 저장하는 함수 ==========================================================================
perform_fpca_for_all_roi <- function(path_smoothing_results,
                                     initial_nharm = 50, 
                                     portion = 0.9, 
                                     atlas_dir,
                                     export_each_roi = T,
                                     export_total_train = T) {
  # 아웃풋 경로 설정
  base_folder_name <- basename(dirname(path_smoothing_results))
  
  # Output 파일 이름
  final_output_file <- file.path(atlas_dir, paste0(base_folder_name, "_fpca_results.rds"))
  
  # 파일 존재 여부 확인 및 조기 종료
  if (file.exists(final_output_file)) {
    print_message(sprintf("File already exists: %s. Exiting without computation.", 
                          final_output_file), crayon::yellow)
    if(export_total_train){
      return(final_output_file %>% readRDS)
    }
  }
  
  output_dir = file.path(atlas_dir, base_folder_name)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print_message(sprintf("Created directory: %s", output_dir), crayon::green)
  }
  
  
  # 데이터 읽어오기
  smoothing_results = path_smoothing_results %>% readRDS
  
  
  # FPCA
  tictoc::tic("FPCA computation completed")
  all_results <- lapply(names(smoothing_results), function(roi_name) {
    # roi_name = names(smoothing_results)[1]
    roi_obj <- smoothing_results[[roi_name]]
    
    
    process_single_roi(roi_obj, 
                       roi_name,
                       output_dir,
                       initial_nharm, 
                       portion,
                       export_each_roi)
    
  }) %>% setNames(paste0(names(smoothing_results)))
  tictoc::toc()
  
  combined_pc_scores <- do.call(cbind, lapply(names(all_results), function(ith_ROI) {
    return(all_results[[ith_ROI]]$selected_scores)
  }))
  
  
  # combined
  combined_results = list(Combined_FPCA_ROI = all_results, FPC_Scores = combined_pc_scores)
  combined_results[[repeated_roi]] = the_number_of_repeated_roi(combined_results[[combined_pc_scores]])
  
  
  if(export_total_train){
    saveRDS(combined_results, final_output_file)
  }
  
  
  
  # output_dir 폴더 삭제 코드
  if (dir.exists(output_dir)) {
    unlink(output_dir, recursive = TRUE)
    print_message(sprintf("Deleted folder and its contents: %s", output_dir), crayon::red)
  } else {
    print_message(sprintf("Folder does not exist: %s", output_dir), crayon::yellow)
  }
  
  print_message(sprintf("Saved combined FPCA results to %s", final_output_file), crayon::green)
  return(combined_results)
}










# 🟥 여러 아틀라스에 FPCA 실행 ====================================================================================
## 🟨 메시지 출력 함수 ==========================================================================
print_processing_message <- function(atlas_name, message_color) {
  print_message(sprintf("Processing Atlas: %s", atlas_name), message_color)
}


## 🟨 디렉토리 생성 함수 ==========================================================================
create_atlas_directory <- function(output_path, atlas_name) {
  atlas_dir <- file.path(output_path, atlas_name)
  dir.create(atlas_dir, showWarnings = FALSE, recursive = TRUE)
  return(atlas_dir)
}


## 🟨 최종 결과 로드 또는 스킵 함수 ==========================================================================
load_or_skip_final_results <- function(final_output_file, atlas_name) {
  if (file.exists(final_output_file)) {
    print_message(sprintf("Final results for atlas %s already exist. Loading results...", atlas_name), crayon::yellow)
    return(readRDS(final_output_file))
  }
  return(NULL)
}


## 🟨 폴더 존재 여부 확인 함수 ==========================================================================
check_required_folds_exist <- function(train_folds_paths, validation_folds_paths, atlas_name) {
  required_folds <- paste0("fold_", 1:5)
  all_folds_exist <- all(required_folds %in% basename(train_folds_paths)) &&
    all(required_folds %in% basename(validation_folds_paths))
  
  if (!all_folds_exist) {
    print_message(sprintf("Skipping Atlas %s: Not all folds found", atlas_name), crayon::red)
    return(FALSE)
  }
  return(TRUE)
}


## 🟨 각 폴드에 대해 FPCA 수행 및 결과 저장 ==========================================================================
process_fold <- function(k, train_folds_paths, validation_folds_paths, atlas_dir, fold_name, initial_nharm, portion, export_each_roi = TRUE, export_each_fold = TRUE) {
  fold_result_file <- file.path(atlas_dir, paste0(fold_name, "_result.rds"))
  
  if (file.exists(fold_result_file)) {
    print_message(sprintf("Loading existing results for %s", fold_name), crayon::blue)
    return(readRDS(fold_result_file))
  }
  
  print_message(sprintf("Processing %s", fold_name), crayon::cyan)
  
  
  # 경로 추출
  train_fold_path <- train_folds_paths[k]
  validation_fold_path <- validation_folds_paths[k]
  
  
  
  # smoothing 결과 로드
  train_smoothing_result_file <- list.files(train_fold_path, pattern = "\\.rds$", full.names = TRUE)
  train_smoothing_results <- readRDS(train_smoothing_result_file)
  
  validation_smoothing_result_file <- list.files(validation_fold_path, pattern = "\\.rds$", full.names = TRUE)
  validation_smoothing_results <- readRDS(validation_smoothing_result_file)
  
  
  
  # 전체 결과 저장 list
  combined_fold_result = list()
  
  
  
  # 각 smoothing 결과에 fpca 적용
  combined_fold_result[[paste0(fold_name, "_fpca_results")]] <- perform_fpca_for_fold(
    train_smoothing_results,
    validation_smoothing_results,
    initial_nharm,
    portion,
    atlas_dir,
    fold_name,
    export_each_roi
  )
  
  
  # Extract Scores
  combined_fold_result[[paste0(fold_name, "_Train_FPC_Scores")]] = lapply(combined_fold_result[[paste0(fold_name, "_fpca_results")]], function(x){
    x$train_scores
  }) %>% 
    do.call(bind_cols, .) %>% 
    `rownames<-`(colnames(train_smoothing_results$ROI_001$fdSmooth_obj$y))
  
  combined_fold_result[[paste0(fold_name, "_Validation_FPC_Scores")]] = lapply(combined_fold_result[[paste0(fold_name, "_fpca_results")]], function(x){
    x$validation_scores
  }) %>% 
    do.call(bind_cols, .) %>% 
    `rownames<-`(colnames(validation_smoothing_results$ROI_001$fdSmooth_obj$y))
  
  
  combined_fold_result[["repeated_roi"]] = the_number_of_repeated_roi(combined_fold_result[[paste0(fold_name, "_Train_FPC_Scores")]])
  
  
  
  # 파일 저장
  if(export_each_fold){
    saveRDS(combined_fold_result, fold_result_file)  
  }
  
  # 파일 및 폴더 전체 삭제 코드
  folder_path <- file.path(atlas_dir, fold_name)
  
  # 해당 폴더와 그 안의 모든 파일/폴더 삭제
  if (dir.exists(folder_path)) {
    unlink(folder_path, recursive = TRUE)
    print_message(sprintf("Deleted folder and its contents: %s", folder_path), crayon::red)
  } else {
    print_message(sprintf("Folder does not exist: %s", folder_path), crayon::yellow)
  }
  
  return(combined_fold_result)
}


## 🟨 임시 파일 정리 함수 ==========================================================================
clean_temp_files <- function(train_folds_paths, atlas_dir) {
  print_message("Cleaning up temporary files...", crayon::magenta)
  for (k in seq_along(train_folds_paths)) {
    fold_result_file <- file.path(atlas_dir, paste0("fold_", k, "_result.rds"))
    if (file.exists(fold_result_file)) {
      file.remove(fold_result_file)
    }
  }
}


## 🟨 최종 FPCA 수행 함수 ==========================================================================
# 최종 FPCA 수행 함수
process_atlas <- function(atlas_path, 
                          output_path, 
                          initial_nharm, 
                          portion, 
                          export_each_roi = T, 
                          export_each_fold = TRUE, 
                          export_total_train = TRUE,
                          export_test = TRUE) {
  
  # 아틀라스의 이름을 추출 (디렉토리 경로에서 마지막 부분)
  atlas_name <- basename(atlas_path)
  
  # 현재 아틀라스 처리 중이라는 메시지를 출력
  print_processing_message(atlas_name, crayon::blue)
  
  # 아틀라스 결과를 저장할 디렉토리를 생성
  atlas_dir <- create_atlas_directory(output_path, atlas_name)
  
  
  # 🟪 Train & Validation ===============================================================================
  # train과 validation 폴드의 경로를 각각 가져옴
  train_folds_paths <- list.dirs(file.path(atlas_path, "train"), recursive = FALSE)
  validation_folds_paths <- list.dirs(file.path(atlas_path, "validation"), recursive = FALSE)
  
  # 모든 필수 폴드(fold_1 ~ fold_5)가 존재하는지 확인
  if (!check_required_folds_exist(train_folds_paths, validation_folds_paths, atlas_name)) {
    return(NULL)  # 만약 존재하지 않는다면 NULL을 반환하고 처리 생략
  }
  
  # Final results 
  final_results = list()
  
  
  # 각 fold의 FPCA 결과를 저장할 리스트를 초기화
  fold_results = list()
  
  # 각 폴드에 대해 반복문을 돌며 FPCA 수행
  for(k in seq_along(train_folds_paths)){
    # k=1
    # fold 이름을 "fold_k" 형태로 만듦
    fold_name <- paste0("fold_", k)
    
    # 각 폴드에 대해 FPCA 수행하고 결과를 fold_results에 저장
    fold_results[[fold_name]] = process_fold(
      k, train_folds_paths, validation_folds_paths, atlas_dir, fold_name,
      initial_nharm, portion, export_each_roi, export_each_fold
    )
    
  }
  final_results[["Fold_FPCA"]] = fold_results
  
  
  
  # 🟪 Test & Total Train ===============================================================================
  # total_train 데이터 FPCA 구하기
  total_train_smoothing_result_path <- list.files(file.path(atlas_path, "total_train"), pattern = "\\.rds$", full.names = TRUE)
  final_results[["Total_Train_FPCA"]] = perform_fpca_for_all_roi(path_smoothing_results = total_train_smoothing_result_path, 
                                                                 initial_nharm,
                                                                 portion,
                                                                 atlas_dir,
                                                                 export_each_roi,
                                                                 export_total_train)
  
  
  # test데이터 FPC 
  test_smoothing_result_path <- list.files(file.path(atlas_path, "test"), pattern = "\\.rds$", full.names = TRUE)
  final_results[["Total_Train_FPCA"]] = compute_FPC_scores_of_test(test_smoothing_result_path, 
                                                                   atlas_dir,
                                                                   export_test)

  
  
  # 🟪 최종 결과 저장 ===============================================================================
  cat(
    crayon::blue("FPCA for "), 
    crayon::yellow(atlas_name), 
    crayon::blue(" has been successfully completed.\n")
  )
  
  # # 임시 파일을 정리 (중간에 생성된 임시 파일들 삭제)
  # clean_temp_files(train_folds_paths, atlas_dir)
  return(final_results)
}




## 🟨 여러 아틀라스에 FPCA 실행 ==========================================================================
perform_fpca_for_multiple_atlases <- function(input_paths, 
                                              output_path, 
                                              initial_nharm = 50, 
                                              portion = 0.9,
                                              export_each_roi = TRUE, 
                                              export_each_fold = TRUE, 
                                              export_total_train = TRUE,
                                              export_test = TRUE,
                                              return_total = TRUE){
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
  
  # atlas 폴더만 선택해서 읽어오기
  all_atlas_paths <- list.dirs(input_paths, full.names = TRUE, recursive = FALSE)
  
  
  # 아틀라스별 로 FPCA 수행
  results_list <- lapply(all_atlas_paths, function(atlas_path) {
    # atlas_path = all_atlas_paths[1]
    process_atlas(atlas_path, output_path, initial_nharm, portion,
                  export_each_roi,
                  export_each_fold,
                  export_total_train,
                  export_test)
  }) %>% setNames(basename(all_atlas_paths))
  
  if(return_total){
    return(results_list)
  }
}


