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








# 🟥 Define Functions ##########################################################################
## 🟨 그룹 비율을 유지하면서 test train 나누기 함수 ==============================================
split_data_by_diagnosis <- function(data, diagnosis_column, train_ratio = 0.7, seed = 42) {
  set.seed(seed)  # 시드 설정으로 재현성 확보
  
  # diagnosis 별로 비율을 유지한 채로 데이터 분할
  train_indices <- createDataPartition(data[[diagnosis_column]], p = train_ratio, list = FALSE)
  
  # train과 test 데이터셋 분리
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  return(list(train = train_data, test = test_data))
}

## 🟨 그룹 비율을 유지하면서 fold 나누기 함수 ==============================================
create_stratified_folds <- function(data, group_column, k = 5, seed = 42) {
  set.seed(seed)  # 시드 설정으로 재현성 확보
  
  # 각 그룹에 대해 동일한 비율로 인덱스를 나누기 위해, group_column 기준으로 fold를 생성
  folds <- createFolds(data[[group_column]], k = k, list = TRUE, returnTrain = FALSE)
  
  return(folds)  # 각 fold에 대한 인덱스 리스트 반환
}
## 🟨 Train 데이터를 5-fold로 나누는 함수 ==============================================
# 
split_train_by_folds <- function(data, folds) {
  # Fold별 데이터 저장을 위한 리스트 초기화
  fold_data <- list()
  
  for (i in 1:length(folds)) {
    # 각 fold의 인덱스 가져오기
    fold_indices <- folds[[i]]
    
    # Fold 인덱스를 기준으로 train과 validation 데이터 나누기
    validation_data <- data[fold_indices, ]   # 현재 fold의 인덱스에 해당하는 validation 데이터
    train_data <- data[-fold_indices, ]       # 나머지 데이터는 train 데이터
    
    # Fold 데이터를 리스트에 저장
    fold_data[[paste0("Fold_", i, "_Train")]] <- train_data
    fold_data[[paste0("Fold_", i, "_Validation")]] <- validation_data
  }
  
  return(fold_data)  # 각 fold의 train과 validation 데이터 리스트 반환
}

