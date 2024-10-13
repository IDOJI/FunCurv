# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 🟨Install and loading Packages ================================
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




# 🟥 Define smoothing functions =========================================================================================================
library(fda)
library(crayon)
library(tictoc)
## 🟨 여러 아틀라스에 FPCA 실행 ==========================================================================
perform_fpca_for_multiple_atlases <- function(input_paths, output_path, initial_nharm = 50, portion = 0.9, export_each_roi = FALSE) {
  # 여러 경로에서 모든 아틀라스 파일 목록 수집
  all_atlas_paths <- unlist(lapply(input_paths, function(input_path) {
    list.files(input_path, full.names = TRUE)
  }))
  
  # 아틀라스별로 FPCA 수행
  results_list <- lapply(all_atlas_paths, function(atlas_path) {
    smoothing_result_paths <- list.files(file.path(atlas_path, "train"), pattern = "\\.rds$", full.names = TRUE)
    
    perform_fpca_for_all(
      path_smoothing_results = smoothing_result_paths,
      initial_nharm = initial_nharm,
      portion = portion,
      output_base_dir = file.path(output_path, basename(atlas_path)),
      export.each.roi = export_each_roi
    )
  })
  
  return(invisible(results_list))
}


## 🟨 메시지 출력 함수 ==========================================================================
print_message <- function(message, color_func = crayon::green) {
  cat(color_func(message), "\n")
}

## 🟨 각 ROI에 대해 FPCA 수행 및 저장하는 함수 ==========================================================================
process_single_roi <- function(roi_obj, roi_name, output_dir, export.each.roi, initial_nharm, portion) {
  
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
  if (file.exists(output_file) && export.each.roi) {
    print_message(sprintf("Skipping %s: File already exists.", roi_name), crayon::yellow)
    return(readRDS(output_file))
  }
  
  # FPCA 수행
  fd_obj <- roi_obj$fdSmooth_obj$fd
  fpca_results <- pca.fd(fd_obj, nharm = initial_nharm, centerfns = TRUE)
  
  # 누적 분산 비율 계산 및 필요한 harmonic 개수 선택
  cumulative_variance <- cumsum(fpca_results$varprop)
  selected_harm <- which(cumulative_variance >= portion)[1]
  
  # 메시지 출력
  print_message(
    sprintf("Selected %d harmonics to explain at least %.2f%% of the variance.", 
            selected_harm, portion * 100),
    crayon::green
  )
  
  # 필요한 harmonic과 score 추출
  selected_harmonics <- fpca_results$harmonics[1:selected_harm]
  selected_scores <- as.data.frame(fpca_results$scores[, 1:selected_harm])
  colnames(selected_scores) <- paste0("FPC_", seq_len(ncol(selected_scores)))
  
  # 결과를 리스트로 저장
  result <- list(
    harmonics = selected_harmonics,
    scores = selected_scores,
    var_explained = cumulative_variance[selected_harm]
  )
  
  # 결과를 RDS 파일로 저장
  if (export.each.roi) {
    saveRDS(result, output_file)
    print_message(sprintf("Saved FPCA results for %s to %s", roi_name, output_file))
  }
  
  return(result)
}
## 🟨 모든 ROI에 대해 FPCA 수행 및 결과 저장하는 함수 ==========================================================================
perform_fpca_for_all <- function(path_smoothing_results, initial_nharm = 50, portion = 0.9, output_base_dir, export.each.roi = FALSE) {
  smoothing_results <- readRDS(path_smoothing_results)
  base_folder_name <- basename(dirname(path_smoothing_results))
  output_dir <- file.path(output_base_dir, base_folder_name)
  
  if (is.null(names(smoothing_results))) {
    names(smoothing_results) <- paste0("ROI_", 1:length(smoothing_results))  
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    print_message(sprintf("Created directory: %s", output_dir), crayon::green)
  }
  
  final_output_file <- file.path(output_dir, paste0(base_folder_name, "_fpca_all_results.rds"))
  combined_pc_scores_file <- file.path(output_dir, paste0(base_folder_name, "_combined_pc_scores.rds"))
  
  # 파일 존재 여부 확인 및 조기 종료
  if (file.exists(final_output_file) && file.exists(combined_pc_scores_file)) {
    print_message(sprintf("Files already exist: %s and %s. Exiting without computation.", 
                          final_output_file, combined_pc_scores_file), crayon::yellow)
    return(invisible(NULL))
  }
  
  tictoc::tic("FPCA computation completed")
  all_results <- lapply(names(smoothing_results), function(roi_name) {
    roi_obj <- smoothing_results[[roi_name]]
    process_single_roi(roi_obj, roi_name, output_dir, export.each.roi, initial_nharm, portion)
  }) %>% setNames(paste0("FPCA_", names(smoothing_results)))
  tictoc::toc()
  
  combined_pc_scores <- do.call(cbind, lapply(names(all_results), function(ith_ROI) {
    names(all_results[[ith_ROI]]$scores) <- paste0(ith_ROI, "_FPC_", seq_len(ncol(all_results[[ith_ROI]]$scores)))
    return(all_results[[ith_ROI]]$scores)
  }))
  
  saveRDS(all_results, final_output_file)
  saveRDS(combined_pc_scores, combined_pc_scores_file)
  
  print_message(sprintf("Saved combined FPCA results to %s", final_output_file), crayon::green)
}








