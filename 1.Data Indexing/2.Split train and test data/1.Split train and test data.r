# 🟥 Load the Subjects list ==========================================================================================
path_subjects = "/Users/Ido/Documents/✴️DataAnalysis/ADNI/RS.fMRI/0.Selected Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"
subjects = read.csv(path_subjects)



# 🟥 Select only SB ==========================================================================================
subjects = subjects %>% dplyr::filter(EPI___BAND.TYPE == "SB")
dim(subjects)


# 🟩 AD, MCI, CN ==========================================================================================
output_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data/AD, MCI, CN"
results = split_and_save_data(subjects,
                    diagnosis_column = "DIAGNOSIS_FINAL",
                    selected_groups = c("Dementia", "MCI", "CN"),
                    train_ratio = 0.7,
                    k_folds = 5,
                    seed = 4649,
                    output_path)


# 🟩 AD, MCI ==========================================================================================
output_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data/AD, MCI"
results = split_and_save_data(subjects,
                              diagnosis_column = "DIAGNOSIS_FINAL",
                              selected_groups = c("Dementia", "MCI"),
                              train_ratio = 0.7,
                              k_folds = 5,
                              seed = 4649,
                              output_path)



# 🟩 AD, CN ==========================================================================================
output_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data/AD, CN"
results = split_and_save_data(subjects,
                              diagnosis_column = "DIAGNOSIS_FINAL",
                              selected_groups = c("Dementia", "CN"),
                              train_ratio = 0.7,
                              k_folds = 5,
                              seed = 4649,
                              output_path)


# 🟩 MCI, CN ==========================================================================================
output_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data/MCI, CN"
results = split_and_save_data(subjects,
                              diagnosis_column = "DIAGNOSIS_FINAL",
                              selected_groups = c("MCI", "CN"),
                              train_ratio = 0.7,
                              k_folds = 5,
                              seed = 4649,
                              output_path)



