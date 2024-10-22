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


filter = dplyr::filter
select = dplyr::select


# 경로 자동 변환 함수 정의
adjust_path <- function(path) {
  # 운영체제에 따라 기본 경로 앞부분 설정
  if (.Platform$OS.type == "windows") {
    # macOS 경로를 Windows 경로로 변환
    path <- sub("^/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/", "E:/", path)
  } else if (.Platform$OS.type == "unix" && grepl("darwin", R.version$os)) {
    # 이미 macOS 경로이므로 그대로 유지
    path <- path
  } else {
    stop("지원되지 않는 운영체제입니다.")
  }
  
  # 최종 경로 반환
  return(path)
}




# 🟪 Options =============================================================================
get_atlas_options <- function(atlas_name) {
  switch(atlas_name,
         "AAL3" = list(n_order = 4, n_breaks = 300, lambdas = exp(seq(-5, -4, 0.5))),
         "1000Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-4, -3, 0.5))),
         "900Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-3, -2, 0.5))),
         "800Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-3, -2, 0.5))),
         "700Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-3, -2, 0.5))),
         "600Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-2, -1, 0.5))),
         "500Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-2, -1, 0.5))),
         "400Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-2, -1, 0.5))),
         "300Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-2, -1, 0.5))),
         "200Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-2, -1, 0.5))),
         "100Parcels" = list(n_order = 4, n_breaks = NULL, lambdas = exp(seq(-2, -1, 0.5))),
         stop(paste("Unknown atlas:", atlas_name)))
}




# 🟥 Sub-functions =========================================================================================================
## 🟨 fit length
fit_length <- function(x.vec, fit.num) {
  # x.vec가 numeric이면 character로 변환
  if (is.numeric(x.vec)) {
    x.vec <- as.character(x.vec)
  }
  
  # fit.num보다 길이가 긴 항목이 있는지 확인
  if (any(nchar(x.vec) > fit.num)) {
    stop("fit.num should be larger!")
  }
  
  # sprintf를 사용하여 자릿수를 맞춤
  New_x.vec <- sprintf(paste0("%0", fit.num, "s"), x.vec)
  
  return(New_x.vec)
}


## 🟨 모든 열을 numeric으로
convert_all_to_numeric <- function(df) {
  df[] <- lapply(df, function(col) {
    as.numeric(as.character(col))
  })
  return(df)
}

## 🟨 RID 변경
change_rid = function(rid){
  sprintf("RID_%04d", rid)
}





## 🟨 파일별 옵션 선택 함수 정의
# get_options_for_file <- function(file_name) {
#   if (grepl("AAL3", file_name)) {
#     return(options_for_each_atlas_list[["AAL3"]])
#   }
#   parcels_pattern <- stringr::str_extract(file_name, "\\d+Parcels")
#   if (!is.na(parcels_pattern) && parcels_pattern %in% names(options_for_each_atlas_list)) {
#     
#     return(options_for_each_atlas_list[[parcels_pattern]])
#     
#   }
#   return(list())
# }





# 🟥 Single Smoothing Functions =========================================================================================================
## 🟨 Single : smoothing by bspline gcv
smoothing_by_bspline_gcv <- function(df,
                                     domain, 
                                     n_order, 
                                     lambdas,
                                     n_breaks = NULL,
                                     path_export = NULL, 
                                     roi_name,
                                     width = 2000,
                                     overwrite = TRUE,
                                     generate_plots = TRUE,
                                     save_each_ROI = TRUE) {  # 추가된 옵션
  dir.create(path_export, showWarnings = F, recursive = T)
  file_name = "smoothing_result"
  library(magrittr)
  library(fda)
  library(crayon)
  
  X <- df %>% convert_all_to_numeric %>% as.matrix()
  
  if (is.null(n_breaks)) {
    n_breaks <- nrow(X)
  }
  
  
  
  ### 🟠 find an optimal lambda =============================================================================================
  opt_lambda <- lambdas[which.min(sapply(lambdas, function(ith_lambda) {
    tryCatch({
      fdPar_obj <- fdPar(fdobj = create.bspline.basis(
        rangeval = c(min(domain), max(domain)), 
        norder = n_order, 
        breaks = seq(from = min(domain), to = max(domain), length.out = n_breaks)), 
        Lfdobj = int2Lfd(2), lambda = ith_lambda)
      mean(smooth.basis(argvals = domain, y = X, fdParobj = fdPar_obj)$gcv)
    }, error = function(e) {
      NA
    })
  }))]
  
  
  ### 🟠 Before smoothing plot =============================================================================================
  if (generate_plots && !is.null(path_export)) {
    file_path_before <- file.path(path_export, paste0(roi_name, "_", file_name, "_before.png"))
    if (file.exists(file_path_before) && !overwrite) {
      
      cat(crayon::yellow("Skipping initial plot: File already exists at"), crayon::bold(file_path_before), "\n")
      
    } else {
      
      png(filename = file_path_before, width = width, height = 600)
      matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, 
              main = "Original Data Before Smoothing", xaxt = "n")
      # add_rotated_x_labels(domain)  # 회전된 x축 레이블 추가
      dev.off()
      cat(crayon::green("Saved plot before smoothing at:"), crayon::bold(file_path_before), "\n")
    }
  }
  
  
  
  
  ### 🟠 Smoothing using the optimal parameter =============================================================================================
  rds_file_path <- file.path(path_export, paste0(roi_name, "_smoothed.rds"))
  
  if (file.exists(rds_file_path)) {
    # 파일이 이미 존재하는 경우 메시지 출력 및 파일 로드
    cat(crayon::yellow("[INFO] Existing smoothed results file found for ROI:"), crayon::bold(roi_name), "\n")
    smoothing_result <- readRDS(rds_file_path)
  } else {
    # 파일이 존재하지 않으면 새로운 스무딩 결과 생성
    opt_fdPar_obj <- fdPar(
      fdobj = create.bspline.basis(rangeval = c(min(domain), max(domain)), 
                                   norder = n_order, 
                                   breaks = seq(from = min(domain), to = max(domain), length.out = n_breaks)), 
      Lfdobj = int2Lfd(2), lambda = opt_lambda
    )
    opt_fdSmooth_obj <- smooth.basis(argvals = domain, y = X, fdParobj = opt_fdPar_obj)
    
    smoothing_result <- list(fdSmooth_obj = opt_fdSmooth_obj, lambda = opt_lambda)
  }
  
  
  
  
  ### 🟠 After smoothing plot =============================================================================================
  opt_lambda_exp <- paste0("exp(", round(log(opt_lambda), 2), ")")
  
  # After 파일 이름에 옵션 포함
  # n_order = 4
  file_name_after <- paste0(
    file_name, "_n_order=", n_order,
    "_lambda=", opt_lambda_exp,
    "_n_breaks=", n_breaks, "_after"
  )
  
  if (generate_plots && !is.null(path_export)) {
    file_path_after <- file.path(path_export, paste0(roi_name, "_", file_name_after, ".png"))
    if (file.exists(file_path_after) && !overwrite) {
      cat(crayon::yellow("Skipping plot after smoothing: File already exists at"), crayon::bold(file_path_after), "\n")
    } else {
      png(filename = file_path_after, width = width, height = 600)
      plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, 
           main = paste("Optimal Smoothing with lambda =", opt_lambda), xaxt = "n")
      # add_rotated_x_labels(domain)  # 회전된 x축 레이블 추가
      dev.off()
      cat(crayon::green("Saved plot after smoothing at:"), crayon::bold(file_path_after), "\n")
    }
  }
  
  
  
  ### 🟠 Save the results =============================================================================================
  if(save_each_ROI){
    
    if (!file.exists(rds_file_path) || overwrite) {
      tryCatch({
        saveRDS(smoothing_result, rds_file_path)
        cat(crayon::green("[INFO] Saved result for ROI:"), crayon::bold(roi_name), "\n")
      }, error = function(e) {
        cat(crayon::red("[ERROR] Failed to save result for ROI:"), crayon::bold(roi_name), "\n")
      })
    } else {
      cat(crayon::yellow("[INFO] RDS file already exists and overwrite is FALSE for ROI:"), crayon::bold(roi_name), "\n")
    }
  }
  
  ### 🟠 Return =============================================================================================
  return(smoothing_result)
}




# 🟥 Define smoothing functions =========================================================================================================
## 🟨 smoothing multiple ROIs =======================================================================
smoothing_multiple_ROIs <- function(path_ith_FC,
                                    target_RID,  
                                    n_order, 
                                    n_breaks = NULL, 
                                    lambdas, 
                                    path_export, 
                                    width = 2000,
                                    overwrite = FALSE,
                                    max_plots) {
  
  ### 🟩 경로 설정 ===================================================================================
  atlas_name <- tools::file_path_sans_ext(basename(path_ith_FC))
  if (!dir.exists(path_export)) {
    tryCatch({
      dir.create(path_export, recursive = TRUE)
      cat(crayon::green("Created export directory at:"), crayon::bold(path_export), "\n")
    }, error = function(e) {
      stop(crayon::red("Error: Failed to create export directory at:"), crayon::bold(path_export), "\n")
    })
  }
  
  
  
  
  
  ### 🟩 이미 저장된 전체 결과가 있는지 확인 ====================================================================
  # export_path에 "combine"과 "results" 둘 다 포함하는 .rds 파일이 존재하는지 확인
  rds_files <- list.files(path_export, pattern = "\\.rds$", full.names = TRUE)
  
  # "combine"과 "results" 둘 다 포함하는 파일이 있는지 확인
  combined_results_file <- rds_files[grepl("combine", rds_files) & grepl("results", rds_files)]
  
  # 파일 존재 & 파일 용량 > 0 & overwrite = T가 아닌 경우
  if (length(combined_results_file) > 0 && file.info(combined_results_file[1])$size > 0 && !overwrite) {
    cat(crayon::blue("[INFO] Combined smoothed results already exist.\n"))
    invisible(return(NULL))
  } else if (length(combined_results_file) > 0 && overwrite) {
    lapply(combined_results_file, file.remove)  # 여러 파일을 삭제할 수 있도록 수정
    cat(crayon::yellow("[INFO] Overwrite is TRUE. Deleted existing combined smoothed results.\n"))
  }     
  
  
  
  
  ### 🟩 데이터 처리 ==========================================================================================
  FC <- readRDS(path_ith_FC)
  FC_filtered <- lapply(FC, function(X) {
    X %>% select(all_of(c(names(X)[1:2], target_RID)))
  }) %>% setNames(names(FC))
  
  ### 🟩 Smoothing each ROI ==========================================================================================
  results_file <- file.path(path_export, paste0("combined_smoothing_results_", atlas_name, ".rds"))
  
  if (!file.exists(results_file)) {
    
    smoothing_results <- lapply(seq_along(FC_filtered), function(i) {
      
      roi_name <- names(FC_filtered)[i]
      
      cat(crayon::cyan("[INFO] Processing ROI:"), crayon::bold(roi_name), "\n")
      
      ith_ROI_FC <- FC_filtered[[roi_name]]
      
      # 스무딩 수행
      ith_smoothing_result <- smoothing_by_bspline_gcv(df = ith_ROI_FC[,-c(1,2)], 
                                                       domain = ith_ROI_FC %>% select(ends_with("Dist")) %>% unlist %>% as.numeric %>% setNames(ith_ROI_FC$ROI), 
                                                       n_order, 
                                                       lambdas, 
                                                       n_breaks, 
                                                       path_export, 
                                                       roi_name, 
                                                       width, 
                                                       overwrite, 
                                                       generate_plots = i <= max_plots)
      
      return(ith_smoothing_result)    
      
    }) %>% setNames(names(FC))
    
    saveRDS(smoothing_results, results_file)
    cat(crayon::green("[INFO] Saved combined smoothing results at:"), crayon::bold(results_file), "\n")
    
    # ROI별 개별 RDS 파일 삭제
    roi_rds_files <- list.files(path_export, pattern = "_smoothed.rds$", full.names = TRUE)
    file.remove(roi_rds_files)
    cat(crayon::green("[INFO] Deleted individual ROI smoothing result files.\n"))  
    
  }
}


## 🟨 각 atlas에 서로 다른 옵션 적용하는 함수 =======================================================================
apply_smoothing_to_all_atlas_files <- function(path_all_FC, 
                                               train_folded,
                                               test,
                                               options_for_each_atlas_list, 
                                               common_options = list(), 
                                               target_group,
                                               filtering_words = character()) {  # 적절한 이름으로 수정
  # Load FC files list
  all_FC_file_list <- list.files(path_all_FC, full.names = TRUE)
  
  # filtering_words에 지정된 문자열이 포함된 파일만 필터링
  if (length(filtering_words) > 0) {
    filtered_FC_file_list <- all_FC_file_list[sapply(all_FC_file_list, function(file) {
      all(sapply(filtering_words, function(word) grepl(word, file)))
    })]
  } else {
    filtered_FC_file_list <- all_FC_file_list
  }
  
  # 각 atlas 파일에 대해 처리
  for (path_ith_FC in filtered_FC_file_list) {
    # path_ith_FC = filtered_FC_file_list[1]
    
    # 🟢 Atlas별 폴더 생성 ====================================================================
    atlas_name = tools::file_path_sans_ext(basename(path_ith_FC))
    new_fold = paste(target_group, 
                     tools::file_path_sans_ext(basename(path_all_FC)),
                     sep = "___")
    
    atlas_export_path = file.path(common_options$path_export, new_fold)
    
    
    if (!dir.exists(atlas_export_path)) {
      
      dir.create(atlas_export_path, recursive = TRUE)
      cat(crayon::green("Created export directory for atlas:"), crayon::bold(atlas_export_path), "\n")
      
    }
    
    
    
    # 🟢 getting option for the atlas ====================================================================
    final_options = c(common_options, options_for_each_atlas_list)
    
    
    
    # 🟢 Test 데이터 처리 ====================================================================
    # subjects = read.csv("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv")
    # tmp = subjects %>% filter(paste0("RID_", fit_length(RID, 4)) %in% test_RID)
    test_RID <- change_rid(test$RID)
    test_path <- file.path(atlas_export_path, "test")
    dir.create(test_path, showWarnings = FALSE)
    cat(crayon::cyan("[INFO] Processing Test Data for Atlas:"), crayon::bold(atlas_name), "\n")
    final_options$path_export = test_path
    test_params = c(list(path_ith_FC = path_ith_FC,
                         target_RID = test_RID), 
                    final_options)
    do.call(smoothing_multiple_ROIs, test_params)
    
    
    
    
    # 🟢 전체 Train 데이터 smoothing  ====================================================================
    train = rbind(train_folded$Fold_1_Train, train_folded$Fold_1_Validation)
    train_RID = change_rid(train$RID)
    # tmp = subjects %>% filter(paste0("RID_", fit_length(RID, 4)) %in% train_RID)
    train_path <- file.path(atlas_export_path, "total_train")
    dir.create(train_path, showWarnings = FALSE)
    cat(crayon::cyan("[INFO] Processing Total Train Data for Atlas:"), crayon::bold(atlas_name), "\n")
    final_options$path_export = train_path
    train_params = c(list(path_ith_FC = path_ith_FC,
                          target_RID = train_RID), 
                     final_options)
    do.call(smoothing_multiple_ROIs, train_params)
    
    
    
    # 🟢 각 폴드에 대해 Train 및 Validation 데이터 처리  ====================================================================
    for (fold in seq(1, 5)) {
      # fold = 1
      train_data <- train_folded[[paste0("Fold_", fold, "_Train")]]
      validation_data <- train_folded[[paste0("Fold_", fold, "_Validation")]]
      
      train_RID <- change_rid(train_data$RID)
      validation_RID <- change_rid(validation_data$RID)
      
      # Train 데이터 처리
      train_path <- file.path(atlas_export_path, "train", paste0("fold_", fold))
      dir.create(train_path, recursive = TRUE, showWarnings = FALSE)
      cat(crayon::cyan("[INFO] Processing Train Data for Fold:"), fold, "-", crayon::bold(atlas_name), "\n")
      final_options$path_export = train_path
      train_params = c(list(path_ith_FC = path_ith_FC, 
                             target_RID = train_RID), 
                        final_options)
      do.call(smoothing_multiple_ROIs, train_params)
      
      # Validation 데이터 처리
      validation_path <- file.path(atlas_export_path, "validation", paste0("fold_", fold))
      dir.create(validation_path, recursive = TRUE, showWarnings = FALSE)
      cat(crayon::cyan("[INFO] Processing Validation Data for Fold:"), fold, "-", crayon::bold(atlas_name), "\n")
      final_options$path_export = validation_path
      validation_params <- c(list(path_ith_FC = path_ith_FC, 
                                  target_RID = validation_RID), 
                             final_options)
      do.call(smoothing_multiple_ROIs, validation_params)
      
    }
  }
}



## 🟨 apply_by_target_group ========================================================================================
apply_by_target_group = function(target_group,
                                 path_test_train_subjects_list,
                                 path_curves_by_distance,
                                 path_export,
                                 atlas_name){
  path_measure_folders_list = list.files(path_curves_by_distance, full.names = T)
  results = lapply(path_measure_folders_list, function(path_ith_measure){
    # path_ith_measure = list.files(path_curves_by_distance, full.names = T)[4]
    path_target_test_train_list = file.path(path_test_train_subjects_list, target_group)
    
    apply_smoothing_to_all_atlas_files(path_all_FC = path_ith_measure,
                                       train_folded = list.files(path_target_test_train_list, pattern = "train_seed", full.names = T) %>% readRDS,
                                       test = list.files(path_target_test_train_list, pattern = "test_seed", full.names = T) %>% readRDS,
                                       options_for_each_atlas_list = get_atlas_options(atlas_name),
                                       common_options = list(path_export = path_export, 
                                                             overwrite = FALSE, 
                                                             max_plots = 5),
                                       target_group,
                                       filtering_words = atlas_name)
  })
}
