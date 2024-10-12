# 🟥 Load the sorted dist data ==========================================================================================
path_sorted_dist = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/2.Arrange the distances for each ROI by the size/Sorted ROI by dist.rds"
path_sorted_dist = path_sorted_dist %>% adjust_path()
sorted_dist = readRDS(path_sorted_dist)



# 🟥 Sort each dataset ===========================================================================================
## 🟩 FC ===============================================
### 🟨 FunImgARCWSF ===============================================================
# FC files list
path_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✅✴️⭐️2.Functional Connectivity/FunImgARCWSF/Fisher Z Transformation"
path_folder = path_folder %>% adjust_path
# save path
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC"
path_save = path_save %>% adjust_path

# process and save 
process_and_save_fc_data(path_folder, path_save, sorted_dist)




### 🟨 FunImgARglobalCWSF ===============================================================
# FC files list
path_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✅✴️⭐️2.Functional Connectivity/global/Fisher Z Transformation"
path_folder = path_folder %>% adjust_path
# save path
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARglobalCWSF/Fisher Z FC"
path_save = path_save %>% adjust_path

# process and save 
process_and_save_fc_data(path_folder, path_save, sorted_dist)







## 🟩 @zALFF ====================================================================================
### 🟨 FunImgARCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARCWSF/✴️zALFFMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zALFF/non"
process_roi_data(path_data_folder, sorted_dist, path_save)


### 🟨 FunImgARglobalCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARglobalCWSF/✴️zALFFMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zALFF/global"
process_roi_data(path_data_folder, sorted_dist, path_save)





## 🟩 @zReHo ====================================================================================
### 🟨 FunImgARCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARCWSF/✴️zReHoMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zReHo/non"
process_roi_data(path_data_folder, sorted_dist, path_save)


### 🟨 FunImgARglobalCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARglobalCWSF/✴️zReHoMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zReHo/global"
process_roi_data(path_data_folder, sorted_dist, path_save)




## 🟩 zDC ====================================================================================
### 🟨 FunImgARCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARCWSF/✴️zDegreeCentrality_PositiveBinarizedSumBrainMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zDegreeCentrality_PositiveBinarizedSumBrainMap/non"
process_roi_data(path_data_folder, sorted_dist, path_save)

path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARCWSF/✴️zDegreeCentrality_PositiveWeightedSumBrainMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zDegreeCentrality_PositiveWeightedSumBrainMap/non"
process_roi_data(path_data_folder, sorted_dist, path_save)




### 🟨 FunImgARglobalCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARglobalCWSF/✴️zDegreeCentrality_PositiveBinarizedSumBrainMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zDegreeCentrality_PositiveBinarizedSumBrainMap/global"
process_roi_data(path_data_folder, sorted_dist, path_save)


path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARglobalCWSF/✴️zDegreeCentrality_PositiveWeightedSumBrainMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zDegreeCentrality_PositiveWeightedSumBrainMap/global"
process_roi_data(path_data_folder, sorted_dist, path_save)



## 🟩 zfALFF ====================================================================================
### 🟨 FunImgARCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARCWSF/✴️zfALFFMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zfALFFMap/non"
process_roi_data(path_data_folder, sorted_dist, path_save)


### 🟨 FunImgARglobalCWSF ===============================================================
path_data_folder = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/ADNI_SB/✴️⭐️3.ROI-defined results/✴️⭐️3.Seed-based ALFF, ReHo, DC/✴️FunImgARglobalCWSF/✴️zfALFFMap"
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/zfALFFMap/global"
process_roi_data(path_data_folder, sorted_dist, path_save)





