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



# 🟥 Define smoothing functions =========================================================================================================
## 🟨 각 atlas에 서로 다른 옵션 적용하는 함수 =======================================================================
apply_smoothing_to_atlas_files <- function(base_path, 
                                           options_for_each_atlas_list, 
                                           common_options = list()) {
  # 경로에서 파일 목록 가져오기
  file_list <- list.files(base_path, full.names = TRUE)
  
  # 파일별 옵션을 선택하는 함수
  get_options_for_file <- function(file_name) {
    # AAL3에 대해 개별 옵션 처리
    if (grepl("AAL3", file_name)) {
      return(options_for_each_atlas_list[["AAL3"]])
    }
    
    # Parcels 숫자에 따른 공통 옵션 처리
    parcels_pattern <- stringr::str_extract(file_name, "\\d+Parcels")
    if (!is.na(parcels_pattern) && parcels_pattern %in% names(options_for_each_atlas_list)) {
      return(options_for_each_atlas_list[[parcels_pattern]])
    }
    
    # 기본 옵션을 반환 (일치하는 옵션이 없을 경우)
    return(list())
  }
  
  # 파일 리스트에 대해 반복 처리
  for (path_atlas in file_list) {
    # 파일명 추출
    file_name <- tools::file_path_sans_ext(basename(path_atlas))
    
    # 파일에 맞는 옵션 가져오기
    specific_options <- get_options_for_file(file_name)
    
    # 공통 옵션과 파일별 옵션 병합
    final_options <- modifyList(common_options, specific_options)
    
    # 필요한 매개변수를 리스트로 준비
    params <- c(list(path_FC_atlas = path_atlas), final_options)
    
    # train_RID 누락 방지 확인
    if (!"train_RID" %in% names(params)) {
      stop("[ERROR] train_RID가 누락되었습니다. 매개변수를 확인하세요.")
    }
    
    # smoothing 함수 호출
    cat(crayon::cyan("[INFO] Processing file:"), crayon::bold(file_name), "\n")
    do.call(smoothing_multiple_ROIs, params)
  }
}


## 🟨 Multiple : smoothing by bspline gcv =======================================================================
smoothing_multiple_ROIs <- function(path_FC_atlas, 
                                    train_RID,
                                    n_order, 
                                    n_breaks = NULL, 
                                    lambdas, 
                                    path_export, 
                                    save_each_ROI = FALSE,
                                    width = 2000,
                                    overwrite = TRUE) {
  ### 🟩 경로 설정 =====================================================
  atlas_name <- tools::file_path_sans_ext(basename(path_FC_atlas))
  atlas_export_path <- file.path(path_export, atlas_name)
  
  # Create the export directory if it doesn't exist
  if (!dir.exists(atlas_export_path)) {
    tryCatch({
      dir.create(atlas_export_path, recursive = TRUE)
      cat(crayon::green("Created export directory at:"), crayon::bold(atlas_export_path), "\n")
    }, error = function(e) {
      stop(crayon::red("Error: Failed to create export directory at:"), crayon::bold(atlas_export_path), "\n")
    })
  }
  
  ### 🟩 Train 및 Test 결과 파일 경로 설정 =====================================================
  train_export_path <- file.path(atlas_export_path, "train")
  test_export_path <- file.path(atlas_export_path, "test")
  
  dir.create(train_export_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(test_export_path, recursive = TRUE, showWarnings = FALSE)
  
  train_results_file <- file.path(train_export_path, "results_train_smoothed.rds")
  test_results_file <- file.path(test_export_path, "results_test_smoothed.rds")
  
  ### 🟩 Train 데이터 처리 =====================================================
  if (file.exists(train_results_file)) {
    cat(crayon::blue("[INFO] Skipping train processing for atlas:"),
        crayon::bgMagenta(atlas_name), "\n")
  } else {
    # 데이터 나누기 및 처리
    FC <- readRDS(path_FC_atlas)
    
    
    FC_train <- lapply(FC, function(X) {
      X %>% select(all_of(c(names(X)[1:2], train_RID))) %>% rename(Dist = Euclid_Dist)
    }) %>% setNames(names(FC))
    
    # train_RID %in% names(FC_train$ROI_001)
    
    results_train <- lapply(names(FC_train), function(roi_name) {
      # roi_name = names(FC_train)[1]
      process_single_ROI(
        roi_name, FC_train, atlas_export_path = train_export_path, n_order, lambdas, 
        n_breaks, width, overwrite, save_each_ROI
      )
    }) %>% setNames(names(FC_train))
    
    results_train <- results_train[!sapply(results_train, is.null)]
    if (length(results_train) > 0) {
      saveRDS(results_train, train_results_file)
      cat(crayon::green("[INFO] Saved train results at:"), crayon::bold(train_results_file), "\n")
    } else {
      cat(crayon::yellow("[INFO] No new train results to save.\n"))
    }
    results_train = NULL
  }
  
  ### 🟩 Test 데이터 처리 =====================================================
  if (file.exists(test_results_file)) {
    cat(crayon::blue("[INFO] Skipping test processing for atlas:"),
        crayon::bgMagenta(atlas_name), "\n")
  } else {
    # 데이터 나누기 및 처리
    if (!exists("FC")) {
      FC <- readRDS(path_FC_atlas)
    }
    
    FC_test <- lapply(FC, function(X) {
      test_columns <- setdiff(names(X), train_RID)
      X %>% select(all_of(test_columns))
    }) %>% setNames(names(FC))
    
    results_test <- lapply(names(FC_test), function(roi_name) {
      process_single_ROI(
        roi_name, FC_test, test_export_path, n_order, lambdas, 
        n_breaks, width, overwrite, save_each_ROI
      )
    }) %>% setNames(names(FC_test))
    
    results_test <- results_test[!sapply(results_test, is.null)]
    if (length(results_test) > 0) {
      saveRDS(results_test, test_results_file)
      cat(crayon::green("[INFO] Saved test results at:"), crayon::bold(test_results_file), "\n")
    } else {
      cat(crayon::yellow("[INFO] No new test results to save.\n"))
    }
    results_test = NULL
  }
}



## 🟨 Single ROI =======================================================================
process_single_ROI <- function(roi_name, FC, atlas_export_path, n_order, lambdas, 
                               n_breaks, width, overwrite, save_each_ROI) {
  cat(crayon::cyan("[INFO] Processing ROI:"), bold(roi_name), "\n")
  # kth_ROI = FC_train[[roi_name]]
  kth_ROI <- FC[[roi_name]]
  domain <- kth_ROI %>% select(ends_with("Dist")) %>% unlist %>% as.numeric
  names(domain) = kth_ROI$ROI
  kth_ROI = kth_ROI %>% select(-ROI, -ends_with("Dist"))
  
  file_name <- paste0(roi_name, "_smoothed_result.png")
  file_path <- file.path(atlas_export_path, file_name)
  
  # 이미 존재하는 경우 처리 건너뛰기
  if (file.exists(file_path) && file.info(file_path)$size > 0 && !overwrite) {
    cat(crayon::yellow("[INFO] Skipping ROI:"), bold(roi_name), "\n")
    return(NULL)
  }
  
  if (save_each_ROI) {
    rds_file_path <- file.path(atlas_export_path, paste0(roi_name, "_smoothed.rds"))
    if (file.exists(rds_file_path) && file.info(rds_file_path)$size > 0) {
      cat(crayon::yellow("[INFO] ROI already processed:"), crayon::bold(roi_name), "\n")
      return(readRDS(rds_file_path))
    }
  }
  
  # 스무딩 수행
  smoothing_result <- smoothing_by_bspline_gcv(
    kth_ROI, domain, n_order, lambdas, n_breaks, 
    path_export = atlas_export_path, 
    file_name = roi_name, 
    width = width, overwrite = overwrite
  )
  
  if (save_each_ROI) {
    rds_file_path <- file.path(atlas_export_path, paste0(roi_name, "_smoothed.rds"))
    tryCatch({
      saveRDS(smoothing_result, rds_file_path)
      cat(crayon::green("[INFO] Saved result for ROI:"), bold(roi_name), "\n")
    }, error = function(e) {
      cat(crayon::red("[ERROR] Failed to save result for ROI:"), bold(roi_name), "\n")
    })
  }
  
  return(smoothing_result)
}




## 🟨 Single : smoothing by bspline gcv =======================================================================
smoothing_by_bspline_gcv <- function(kth_ROI,
                                     domain, 
                                     n_order, 
                                     lambdas,
                                     n_breaks = NULL,
                                     path_export = NULL, 
                                     file_name = "smoothing_result",
                                     width = 2000,
                                     overwrite = TRUE) {
  library(magrittr)
  library(fda)
  library(crayon)
  
  # Convert to numeric
  convert_all_to_numeric <- function(df) {
    df[] <- lapply(df, function(col) {
      as.numeric(as.character(col))
    })
    return(df)
  }
  
  # Convert kth_ROI to matrix
  X <- kth_ROI %>% convert_all_to_numeric %>% as.matrix()
  
  if (is.null(n_breaks)) {
    n_breaks <- nrow(X)
  }
  
  # Lambda 표시를 위한 exp 형태 생성
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
  
  opt_lambda_exp <- paste0("exp(", round(log(opt_lambda), 2), ")")
  
  # After 파일 이름에 옵션 포함
  file_name_after <- paste0(
    file_name, "_n_order=", n_order,
    "_lambda=", opt_lambda_exp,
    "_n_breaks=", n_breaks, "_after"
  )
  
  # Helper function to add rotated x-axis labels
  add_rotated_x_labels <- function(domain) {
    axis(1, at = domain, labels = FALSE)  # 기본 x축 눈금 추가
    text(x = round(domain, 3), y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
         labels = domain, srt = 45, adj = 1, xpd = TRUE)  # 레이블 회전 추가
  }
  
  # Before smoothing plot
  if (!is.null(path_export)) {
    file_path_before <- file.path(path_export, paste0(file_name, "_before.png"))
    if (file.exists(file_path_before) && !overwrite) {
      cat(yellow("Skipping initial plot: File already exists at"), bold(file_path_before), "\n")
    } else {
      png(filename = file_path_before, width = width, height = 600)
      matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, 
              main = "Original Data Before Smoothing", xaxt = "n")
      add_rotated_x_labels(domain)  # 회전된 x축 레이블 추가
      dev.off()
      cat(green("Saved plot before smoothing at:"), bold(file_path_before), "\n")
    }
  }
  
  # Smoothing using the optimal parameter
  opt_fdPar_obj <- fdPar(
    fdobj = create.bspline.basis(rangeval = c(min(domain), max(domain)), 
                                 norder = n_order, 
                                 breaks = seq(from = min(domain), to = max(domain), length.out = n_breaks)), 
    Lfdobj = int2Lfd(2), lambda = opt_lambda
  )
  opt_fdSmooth_obj <- smooth.basis(argvals = domain, y = X, fdParobj = opt_fdPar_obj)
  
  # After smoothing plot
  if (!is.null(path_export)) {
    file_path_after <- file.path(path_export, paste0(file_name_after, ".png"))
    if (file.exists(file_path_after) && !overwrite) {
      cat(yellow("Skipping plot after smoothing: File already exists at"), bold(file_path_after), "\n")
    } else {
      png(filename = file_path_after, width = width, height = 600)
      plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, 
           main = paste("Optimal Smoothing with lambda =", opt_lambda), xaxt = "n")
      add_rotated_x_labels(domain)  # 회전된 x축 레이블 추가
      dev.off()
      cat(green("Saved plot after smoothing at:"), bold(file_path_after), "\n")
    }
  }
  
  return(list(fdSmooth_obj = opt_fdSmooth_obj, lambda = opt_lambda))
}
