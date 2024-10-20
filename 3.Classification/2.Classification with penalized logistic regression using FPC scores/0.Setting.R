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
List.list[[2]] = stat = c("fda", "MASS", "caret", "pROC")
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




# 🟥 ordinary logistic with Group penalty =========================================================================================================
# 필요한 패키지 로드
library(grpreg)
library(ggplot2)
# 필요한 패키지 로드
library(grpreg)
library(pROC)

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
    # 모델 적합
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

    

