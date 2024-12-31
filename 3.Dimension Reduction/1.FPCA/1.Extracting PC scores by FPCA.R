# 🟩 common data ===========================================================================================
## 🟨 demographics =====================================================================
path_demo = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"
path_demo = "/Users/Ido/Documents/✴️DataAnalysis/ADNI/RS.fMRI/0.Selected Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"
demo = read.csv(path_demo %>% set_output_path)

# check
demo_11 = demo %>% filter(!is.na(ADNIMERGE___ADAS11)) # ADAS11
demo_13 = demo %>% filter(!is.na(ADNIMERGE___ADAS13)) # ADAS13

# demo_11$RID %in% demo_13$RID

test = readRDS("/Users/Ido/combined_smoothing_results_AAL3.rds")
test = readRDS("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/⭐️FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF_Fisher Z FC/AAL3.rds")
test = readRDS("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/⭐️FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_Fisher Z FC/AAL3/combined_smoothing_results_AAL3.rds")
test = readRDS("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/⭐️FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARglobalCWSF_zfALFFMap/AAL3/combined_smoothing_results_AAL3.rds")

update.packages(ask = FALSE)


## 🟨 Smoothed results =======================================================================
path_smoothed_data_common = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion"
path_smoothed_data_common = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/⭐️FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion"



test = readRDS("/Users/Ido/Downloads/untitled folder 2/combined_smoothing_results_AAL3.rds")



## 🟨 save path ============================================================================
save_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA" %>% set_output_path
save_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/⭐️FunCurv/3.Dimension Reduction/1.FPCA/FPAC_ADAS13" %>% set_output_path




# 🟩 AAL3 FPCA on All subjects ===========================================================================================
path_smoothed_data_all = list.files(path_smoothed_data_common %>% set_output_path, full.names = T)
target_diagnosis_list = list(c("Dementia", "MCI"), c("Dementia", "CN"), c("MCI", "CN"), c("Dementia", "MCI", "CN"))

for(n in seq_along(path_smoothed_data_all)){
  n = 1
  for(t in seq_along(target_diagnosis_list)){
    # t = 4
    conduct_fpca_on_smoothed_results(path_smoothed_data = path_smoothed_data_all[n],
                                     demographics = demo,
                                     target_diagnosis = target_diagnosis_list[[t]],
                                     save_path = save_path)
  }
  
}





# 🟩 AAL3 FPCA on ADAS13 ===========================================================================================
path_smoothed_data_all = list.files(path_smoothed_data_common %>% set_output_path, full.names = T)
target_diagnosis_list = list(c("Dementia", "MCI"), c("Dementia", "CN"), c("MCI", "CN"), c("Dementia", "MCI", "CN"))

for(n in seq_along(path_smoothed_data_all)){
  n=1
  for(t in seq_along(target_diagnosis_list)){
    # t = 1
    conduct_fpca_on_smoothed_results(path_smoothed_data = path_smoothed_data_all[n],
                                     demographics = demo_13,
                                     target_diagnosis = target_diagnosis_list[[t]],
                                     save_path = save_path,
                                     n_fold = 3)
  }
  
}






