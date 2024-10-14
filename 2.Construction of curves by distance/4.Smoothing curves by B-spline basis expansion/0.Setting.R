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
## 🟨 RID 변경 =======================================================================
change_rid = function(rid){
  sprintf("RID_%04d", rid)
}


## 🟨 각 atlas에 서로 다른 옵션 적용하는 함수 =======================================================================
apply_smoothing_to_atlas_files <- function(base_path, 
                                           train_folded,
                                           test,
                                           options_for_each_atlas_list, 
                                           common_options = list()) {
  
  # 경로에서 파일 목록 가져오기
  file_list <- list.files(base_path, full.names = TRUE)
  
  # 파일별 옵션 선택 함수 정의
  get_options_for_file <- function(file_name) {
    if (grepl("AAL3", file_name)) {
      return(options_for_each_atlas_list[["AAL3"]])
    }
    parcels_pattern <- stringr::str_extract(file_name, "\\d+Parcels")
    if (!is.na(parcels_pattern) && parcels_pattern %in% names(options_for_each_atlas_list)) {
      return(options_for_each_atlas_list[[parcels_pattern]])
    }
    return(list())
  }
  
  # 각 atlas 파일에 대해 처리
  for (path_atlas in file_list) {
    file_name <- tools::file_path_sans_ext(basename(path_atlas))
    specific_options <- get_options_for_file(file_name)
    
    # 중복된 path_export 제거
    final_options <- modifyList(common_options, specific_options)
    final_options$path_export <- NULL  # path_export 제거
    
    # 🟢 Atlas별 폴더 생성
    atlas_export_path <- file.path(common_options$path_export, file_name)
    if (!dir.exists(atlas_export_path)) {
      dir.create(atlas_export_path, recursive = TRUE)
      cat(crayon::green("Created export directory for atlas:"), crayon::bold(atlas_export_path), "\n")
    }
    
    # 🟢 Test 데이터 처리
    test_RID <- change_rid(test$RID)
    # dim(test)
    test_path <- file.path(atlas_export_path, "test")
    dir.create(test_path, showWarnings = FALSE)
    cat(crayon::cyan("[INFO] Processing Test Data for Atlas:"), crayon::bold(file_name), "\n")
    test_params <- c(list(path_FC_atlas = path_atlas, 
                          target_RID = test_RID, 
                          path_export = test_path), 
                     final_options)
    do.call(smoothing_multiple_ROIs, test_params)
    
    # 🟢 각 폴드에 대해 Train 및 Validation 데이터 처리
    for (fold in seq(1, 5)) {
      
      train_data <- train_folded[[paste0("Fold_", fold, "_Train")]]
      validation_data <- train_folded[[paste0("Fold_", fold, "_Validation")]]
      
      train_RID <- change_rid(train_data$RID)
      validation_RID <- change_rid(validation_data$RID)
      
      # Train 데이터 처리
      train_path <- file.path(atlas_export_path, "train", paste0("fold_", fold))
      dir.create(train_path, recursive = TRUE, showWarnings = FALSE)
      cat(crayon::cyan("[INFO] Processing Train Data for Fold:"), fold, "-", crayon::bold(file_name), "\n")
      train_params <- c(list(path_FC_atlas = path_atlas, 
                             target_RID = train_RID, 
                             path_export = train_path), 
                        final_options)
      do.call(smoothing_multiple_ROIs, train_params)
      
      # Validation 데이터 처리
      validation_path <- file.path(atlas_export_path, "validation", paste0("fold_", fold))
      dir.create(validation_path, recursive = TRUE, showWarnings = FALSE)
      cat(crayon::cyan("[INFO] Processing Validation Data for Fold:"), fold, "-", crayon::bold(file_name), "\n")
      validation_params <- c(list(path_FC_atlas = path_atlas, 
                                  target_RID = validation_RID, 
                                  path_export = validation_path), 
                             final_options)
      do.call(smoothing_multiple_ROIs, validation_params)
    }
  }
}



## 🟨 Multiple : smoothing by bspline gcv =======================================================================
smoothing_multiple_ROIs <- function(path_FC_atlas, 
                                    target_RID,  
                                    n_order, 
                                    n_breaks = NULL, 
                                    lambdas, 
                                    path_export, 
                                    save_each_ROI = FALSE,
                                    width = 2000,
                                    overwrite = TRUE,
                                    max_plots = Inf) {  # 추가된 옵션
  ### 🟩 경로 설정 =====================================================
  atlas_name <- tools::file_path_sans_ext(basename(path_FC_atlas))
  export_path <- file.path(path_export)
  
  if (!dir.exists(export_path)) {
    tryCatch({
      dir.create(export_path, recursive = TRUE)
      cat(crayon::green("Created export directory at:"), crayon::bold(export_path), "\n")
    }, error = function(e) {
      stop(crayon::red("Error: Failed to create export directory at:"), crayon::bold(export_path), "\n")
    })
  }
  
  results_file <- file.path(export_path, paste0("results_smoothed_", atlas_name, ".rds"))
  
  ### 🟩 데이터 처리 =====================================================
  if (file.exists(results_file) && !overwrite) {
    cat(crayon::blue("[INFO] Skipping processing for atlas:"), crayon::bgMagenta(atlas_name), "\n")
    return(NULL)
  }
  
  FC <- readRDS(path_FC_atlas)
  
  FC_filtered <- lapply(FC, function(X) {
    X %>% select(all_of(c(names(X)[1:2], target_RID)))
  }) %>% setNames(names(FC))
  
  results <- list()
  roi_index <- 0  # ROI 인덱스 초기화
  
  for (roi_name in names(FC_filtered)) {
    roi_index <- roi_index + 1
    generate_plots <- roi_index <= max_plots  # 첫 max_plots 개의 ROI에 대해서만 플롯 생성
    
    result <- process_single_ROI(
      roi_name, FC_filtered, export_path, n_order, lambdas, 
      n_breaks, width, overwrite, save_each_ROI, generate_plots
    )
    
    if (!is.null(result)) {
      results[[roi_name]] <- result
    }
  }
  
  results <- results[!sapply(results, is.null)]
  if (length(results) > 0) {
    saveRDS(results, results_file)
    cat(crayon::green("[INFO] Saved results at:"), crayon::bold(results_file), "\n")
  } else {
    cat(crayon::yellow("[INFO] No new results to save.\n"))
  }
}

## 🟨 Single ROI =======================================================================
process_single_ROI <- function(roi_name, FC, atlas_export_path, n_order, lambdas, 
                               n_breaks, width, overwrite, save_each_ROI, generate_plots) {
  require(crayon)
  cat(crayon::cyan("[INFO] Processing ROI:"), crayon::bold(roi_name), "\n")
  
  kth_ROI <- FC[[roi_name]]
  domain <- kth_ROI %>% select(ends_with("Dist")) %>% unlist %>% as.numeric
  names(domain) = kth_ROI$ROI
  kth_ROI = kth_ROI %>% select(-ROI, -ends_with("Dist"))
  
  file_name <- paste0(roi_name, "_smoothed_result")
  
  if (save_each_ROI) {
    rds_file_path <- file.path(atlas_export_path, paste0(roi_name, "_smoothed.rds"))
    if (file.exists(rds_file_path) && !overwrite) {
      cat(crayon::yellow("[INFO] RDS file already exists for ROI:"), crayon::bold(roi_name), "\n")
      return(readRDS(rds_file_path))
    }
  }
  
  # 스무딩 수행
  smoothing_result <- smoothing_by_bspline_gcv(
    kth_ROI, domain, n_order, lambdas, n_breaks, 
    path_export = atlas_export_path, 
    file_name = file_name, 
    width = width, overwrite = overwrite, 
    generate_plots = generate_plots  # 추가된 매개변수 전달
  )
  
  if (save_each_ROI) {
    rds_file_path <- file.path(atlas_export_path, paste0(roi_name, "_smoothed.rds"))
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
                                     overwrite = TRUE,
                                     generate_plots = TRUE) {  # 추가된 옵션
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
    labels <- if (!is.null(names(domain))) names(domain) else as.character(domain)
    axis(1, at = domain, labels = FALSE)  # 기본 x축 눈금 추가
    text(x = domain, y = par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
         labels = labels, srt = 45, adj = 1, xpd = TRUE)  # 레이블 회전 추가
  }
  
  # Before smoothing plot
  if (generate_plots && !is.null(path_export)) {
    file_path_before <- file.path(path_export, paste0(file_name, "_before.png"))
    if (file.exists(file_path_before) && !overwrite) {
      cat(crayon::yellow("Skipping initial plot: File already exists at"), crayon::bold(file_path_before), "\n")
    } else {
      png(filename = file_path_before, width = width, height = 600)
      matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, 
              main = "Original Data Before Smoothing", xaxt = "n")
      add_rotated_x_labels(domain)  # 회전된 x축 레이블 추가
      dev.off()
      cat(crayon::green("Saved plot before smoothing at:"), crayon::bold(file_path_before), "\n")
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
  if (generate_plots && !is.null(path_export)) {
    file_path_after <- file.path(path_export, paste0(file_name_after, ".png"))
    if (file.exists(file_path_after) && !overwrite) {
      cat(crayon::yellow("Skipping plot after smoothing: File already exists at"), crayon::bold(file_path_after), "\n")
    } else {
      png(filename = file_path_after, width = width, height = 600)
      plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, 
           main = paste("Optimal Smoothing with lambda =", opt_lambda), xaxt = "n")
      add_rotated_x_labels(domain)  # 회전된 x축 레이블 추가
      dev.off()
      cat(crayon::green("Saved plot after smoothing at:"), crayon::bold(file_path_after), "\n")
    }
  }
  
  return(list(fdSmooth_obj = opt_fdSmooth_obj, lambda = opt_lambda))
}
