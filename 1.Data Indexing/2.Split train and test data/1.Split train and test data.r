# 🟥 Load the Subjects list ==========================================================================================
path_subjects = "/Users/Ido/Documents/✴️DataAnalysis/ADNI/RS.fMRI/0.Selected Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"
subjects = read.csv(path_subjects)



# 🟥 Select only SB ==========================================================================================
subjects = subjects %>% dplyr::filter(EPI___BAND.TYPE == "SB")
dim(subjects)


# 🟥 Extract test & train indices ==========================================================================================
## 🟨 split data ==============================================================================
seed = 4649
split_result = split_data_by_diagnosis(subjects, "DIAGNOSIS_FINAL", train_ratio = 0.7, seed = seed)

train = split_result$train
test = split_result$test

train_indices <- rownames(train)
test_indices <- rownames(test)





## 🟨 check the splitted data ==============================================================================
# check the proportion
test$DIAGNOSIS_FINAL %>% table
test$DIAGNOSIS_FINAL %>% table %>% prop.table
train$DIAGNOSIS_FINAL %>% table
train$DIAGNOSIS_FINAL %>% table %>% prop.table

# Check the sample size
dim(test)
dim(train)





## 🟨 Split fold ==============================================================================
# Train 데이터에 대해 5-fold 교차 검증 인덱스 생성
folds <- create_stratified_folds(train, group_column = "DIAGNOSIS_FINAL", k = 5, seed = seed)

# Fold별 train과 validation 데이터 나누기
folded_data <- split_train_by_folds(train, folds)
folded_data$Fold_1_Train %>% dim
folded_data$Fold_1_Validation %>% dim
folded_data$Fold_1_Train$DIAGNOSIS_FINAL %>% table %>% prop.table






# 🟨 Export data ==============================================================================
output_path = "/Users/Ido/Documents/✴️DataAnalysis/FunCurv/1.Data Indexing/2.Split train and test data"

# 파일명 생성 함수: 경로, 파일 유형(train/test), seed 정보 포함
create_filename <- function(base_path, data_type, seed) {
  file_name <- sprintf("%s/%s_seed_%d.rds", base_path, data_type, seed)
  return(file_name)
}

# 파일명 생성
train_file <- create_filename(output_path, "train", seed)
test_file <- create_filename(output_path, "test", seed)

# Fold로 나누지 않은 전체 train 데이터를 위한 파일명 생성
all_train_file <- create_filename(output_path, "all_train_data", seed)

# 데이터 저장
saveRDS(folded_data, train_file)   # Fold 데이터 저장
saveRDS(test, test_file)            # Test 데이터 저장
saveRDS(train, all_train_file)      # 전체 train 데이터 저장







