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
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2", "PRROC")
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

get_file_names_without_extension <- function(path_data, pattern = "fold") {
  # 파일 목록 불러오기
  names_fold_data <- list.files(path_data, pattern = pattern, full.names = FALSE)
  
  # 파일 확장자 제거하고 이름만 추출
  file_names <- tools::file_path_sans_ext(names_fold_data)
  
  # 결과 반환
  return(file_names)
}


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

# 🟩 전체 저장 함수 ========================================================================================
split_and_save_data <- function(subjects, 
                                diagnosis_column = "DIAGNOSIS_FINAL", 
                                selected_groups = c("Dementia", "MCI", "CN"),
                                train_ratio = 0.7, 
                                k_folds = 5, 
                                seed = 4649, 
                                output_path = NULL) {
  # selected_groups = c("Dementia", "MCI")
  # 🟨 선택한 그룹 필터링 
  # 선택한 diagnosis 그룹만 포함하는 데이터 필터링
  filtered_subjects <- subjects[subjects[[diagnosis_column]] %in% selected_groups, ]
  
  
  
  # 🟨 데이터 분할
  split_result <- split_data_by_diagnosis(filtered_subjects, 
                                          diagnosis_column, 
                                          train_ratio, 
                                          seed)
  train <- split_result$train
  test <- split_result$test
  
  train_indices <- rownames(train)
  test_indices <- rownames(test)
  
  
  
  
  # 🟨 데이터 비율 확인
  print("Test set diagnosis proportion:")
  print(test[[diagnosis_column]] %>% table %>% prop.table)
  
  print("Train set diagnosis proportion:")
  print(train[[diagnosis_column]] %>% table %>% prop.table)
  
  print(paste("Test set dimensions:", dim(test)))
  print(paste("Train set dimensions:", dim(train)))
  
  
  
  # 🟨 5-Fold 교차 검증
  folds <- create_stratified_folds(train, group_column = diagnosis_column, k = k_folds, seed = seed)
  folded_data <- split_train_by_folds(train, folds)
  
  print("Fold 1 Train set dimensions:")
  print(dim(folded_data$Fold_1_Train))
  
  print("Fold 1 Validation set dimensions:")
  print(dim(folded_data$Fold_1_Validation))
  
  print("Fold 1 Train set diagnosis proportion:")
  print(folded_data$Fold_1_Train[[diagnosis_column]] %>% table %>% prop.table)
  
  
  
  
  # 🟨 데이터 저장 (output_path가 NULL이 아닐 때만 저장)
  if (!is.null(output_path)) {
    # 파일명 생성
    train_file <- create_filename(output_path, "train", seed)
    test_file <- create_filename(output_path, "test", seed)
    all_train_file <- create_filename(output_path, "all_train_data", seed)
    
    # 데이터 저장
    saveRDS(folded_data, train_file)   # Fold 데이터 저장
    saveRDS(test, test_file)            # Test 데이터 저장
    saveRDS(train, all_train_file)      # 전체 train 데이터 저장
    
    print("Data saved to files.")
  } else {
    print("Data export skipped as output_path is NULL.")
  }
  
  return(list(folded_data = folded_data, test = test, train = train))
}





# 🟥 Define Functions ##########################################################################
## 🟨 파일명 생성 함수: 경로, 파일 유형(train/test), seed 정보 포함 ===========================================
create_filename <- function(base_path, data_type, seed) {
  file_name <- sprintf("%s/%s_seed_%d.rds", base_path, data_type, seed)
  return(file_name)
}


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

