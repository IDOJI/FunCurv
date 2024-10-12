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
      cat(crayon::green("Created export directory at:"), bold(atlas_export_path), "\n")
    }, error = function(e) {
      stop(crayon::red("Error: Failed to create export directory at:"), bold(atlas_export_path), "\n")
    })
  }
  
  
  
  ### 🟩 데이터 나누기 =====================================================
  # Read the FC data from the RDS file
  FC <- readRDS(path_FC_atlas)
  
  # Split FC by train and test
  FC_train <- lapply(FC, function(X) {
    X %>% select(all_of(c(names(X)[1:2], train_RID)))
  }) %>% setNames(names(FC))
  
  FC_test <- lapply(FC, function(X) {
    test_columns <- setdiff(names(X), train_RID)
    X %>% select(all_of(test_columns))
  }) %>% setNames(names(FC))
  
  
  ### 🟩 경로 생성 =====================================================
  # Train과 Test 결과를 저장할 경로 생성
  train_export_path <- file.path(atlas_export_path, "train")
  test_export_path <- file.path(atlas_export_path, "test")
  
  dir.create(train_export_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(test_export_path, recursive = TRUE, showWarnings = FALSE)
  
  
  ### 🟩 Train 데이터 처리 =====================================================
  results_train <- lapply(names(FC_train), function(roi_name) {
    process_single_ROI(
      roi_name, FC_train, train_export_path, n_order, lambdas, 
      n_breaks, width, overwrite, save_each_ROI
    )
  }) %>% setNames(names(FC_train))
  # Train 결과 저장
  results_train <- results_train[!sapply(results_train, is.null)]
  if (length(results_train) > 0) {
    train_results_file <- file.path(train_export_path, "results_train_smoothed.rds")
    saveRDS(results_train, train_results_file)
    cat(crayon::green("[INFO] Saved train results at:"), bold(train_results_file), "\n")
  } else {
    cat(crayon::yellow("[INFO] No new train results to save.\n"))
  }
  results_train = NULL
  
  
  
  ### 🟩 Test 데이터 처리 =====================================================
  # Test 데이터 처리
  results_test <- lapply(names(FC_test), function(roi_name) {
    process_single_ROI(
      roi_name, FC_test, test_export_path, n_order, lambdas, 
      n_breaks, width, overwrite, save_each_ROI
    )
  }) %>% setNames(names(FC_test))
  
  # Test 결과 저장
  results_test <- results_test[!sapply(results_test, is.null)]
  if (length(results_test) > 0) {
    test_results_file <- file.path(test_export_path, "results_test_smoothed.rds")
    saveRDS(results_test, test_results_file)
    cat(crayon::green("[INFO] Saved test results at:"), bold(test_results_file), "\n")
  } else {
    cat(crayon::yellow("[INFO] No new test results to save.\n"))
  }
  results_test = NULL
  
}


## 🟨 Single ROI =======================================================================
process_single_ROI <- function(roi_name, FC, atlas_export_path, n_order, lambdas, 
                               n_breaks, width, overwrite, save_each_ROI) {
  cat(crayon::cyan("[INFO] Processing ROI:"), bold(roi_name), "\n")
  
  kth_ROI <- FC[[roi_name]]
  domain <- kth_ROI$Dist
  kth_ROI$ROI <- kth_ROI$Dist <- NULL
  
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
  library(crayon)  # Load the crayon package for colored output
  
  # Convert kth_ROI to matrix
  X <- kth_ROI %>% as.matrix()
  
  if (is.null(n_breaks)) {
    n_breaks <- nrow(X)
  }
  
  # Initial plot before smoothing
  if (!is.null(path_export)) {
    file_path_before <- file.path(path_export, paste0(file_name, "_before.png"))
    
    # Check if the initial plot already exists
    if (file.exists(file_path_before) && !overwrite) {
      cat(yellow("Skipping initial plot: File already exists at"), bold(file_path_before), "\n")
    } else {
      png(filename = file_path_before, width = width, height = 600)
      matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, main = "Original Data Before Smoothing")
      dev.off()
      cat(green("Saved plot before smoothing at:"), bold(file_path_before), "\n")
    }
  } else {
    matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, main = "Original Data Before Smoothing")
  }
  
  # Define smoothing result file path
  smoothing_result_file <- file.path(path_export, paste0(file_name, "_smoothing_result.rds"))
  
  # Check if the smoothing result file already exists
  if (!is.null(path_export) && file.exists(smoothing_result_file) && !overwrite) {
    cat(yellow("Skipping smoothing and plotting: File already exists at"), bold(smoothing_result_file), "\n")
    return(readRDS(smoothing_result_file))  # Return the saved result if it exists
  }
  
  # Create B-spline basis
  fdobj_basis <- create.bspline.basis(
    rangeval = c(min(domain), max(domain)),
    norder = n_order,
    breaks = seq(from = min(domain), to = max(domain), length.out = n_breaks)
  )
  
  # Find the optimal GCV
  gcvs <- sapply(lambdas, function(ith_lambda) {
    tryCatch({
      fdPar_obj <- fdPar(fdobj = fdobj_basis, Lfdobj = int2Lfd(2), lambda = ith_lambda)
      fdSmooth_obj <- smooth.basis(argvals = domain, y = X, fdParobj = fdPar_obj)
      mean(fdSmooth_obj$gcv)
    }, error = function(e) {
      cat(red("Error occurred for lambda =", ith_lambda, "\n"))
      NA
    })
  })
  
  # Smoothing using the optimal parameter
  opt_lambda <- lambdas[which.min(gcvs)]
  opt_fdPar_obj <- fdPar(fdobj = fdobj_basis, Lfdobj = int2Lfd(2), lambda = opt_lambda)
  opt_fdSmooth_obj <- smooth.basis(argvals = domain, y = X, fdParobj = opt_fdPar_obj)
  
  # Plot after smoothing
  if (!is.null(path_export)) {
    file_path_after <- file.path(path_export, paste0(file_name, "_after.png"))
    
    # Check if the final plot already exists
    if (file.exists(file_path_after) && !overwrite) {
      cat(yellow("Skipping plot after smoothing: File already exists at"), bold(file_path_after), "\n")
    } else {
      png(filename = file_path_after, width = width, height = 600)
      plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, main = paste("Optimal Smoothing with lambda =", opt_lambda))
      dev.off()
      cat(green("Saved plot after smoothing at:"), bold(file_path_after), "\n")
    }
  } else {
    plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, main = paste("Optimal Smoothing with lambda =", opt_lambda))
  }
  
  # Save smoothing result
  # if (!is.null(path_export)) {
  #   saveRDS(opt_fdSmooth_obj, smoothing_result_file)
  #   cat(green("Saved smoothing result at:"), bold(smoothing_result_file), "\n")
  # }
  
  return(list(fdSmooth_obj = opt_fdSmooth_obj, lambda = opt_lambda))
}
