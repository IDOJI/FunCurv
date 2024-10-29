# demographics
path_demo = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"
demo = read.csv(path_demo)


# Smoothed results
path_smoothed_results = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_Fisher Z FC"
smoothed_data = path_smoothed_results %>% 
  list.files(pattern = "\\.rds$", full.names = T, recursive = T) %>% 
  readRDS()


# 🟩 FPCA ===========================================================================================
targe_diagnosis = list(c("Dementia", "MCI"), c("Dementia", "CN"), c("MCI", "CN"))
# Dementia, MCI

save_path
conduct_fpca_on_smoothed_results(demographics = demo,
                                 target_diagnosis = c("Dementia", "MCI"),
                                 smoothed_data = smoothed_data,
                                 save_path = save_path)
# SEX ? Age
demographics$DIAGNOSIS_FINAL
demographics %>% names
demographics$SEARCH___SEX
cbind(demographics$SEARCH___AGE)

dim(demographics)

demographics



