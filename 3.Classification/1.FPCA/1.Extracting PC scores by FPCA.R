# 🟩 common data ===========================================================================================
# demographics
path_demo = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"
demo = read.csv(path_demo %>% set_output_path)

# Smoothed results
path_smoothed_data_common = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion"

# save path
save_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA" %>% set_output_path



# 🟩 FPCA ===========================================================================================
path_smoothed_data_all = list.files(path_smoothed_data_common %>% set_output_path, full.names = T)
target_diagnosis_list = list(c("Dementia", "MCI"), c("Dementia", "CN"), c("MCI", "CN"))

for(n in seq_along(path_smoothed_data_all)){
  n = 1
  for(t in seq_along(target_diagnosis_list)){
    
    conduct_fpca_on_smoothed_results(path_smoothed_data = path_smoothed_data_all[n],
                                     demographics = demo,
                                     target_diagnosis = target_diagnosis_list[[t]],
                                     save_path = save_path)
  }
  
}



