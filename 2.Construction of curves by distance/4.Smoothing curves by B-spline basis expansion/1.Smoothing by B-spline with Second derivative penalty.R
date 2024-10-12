# 🟥 FC ==============================================================================================================================================
## 🟨 FunImgARCWSF ====================================================================================================================
path_FC = "E:/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC"
path_FC = path_FC %>% adjust_path
path_export = "E:/Smoothing_FunImgARCWSF_FC"


### 🟧 AAL3  ====================================================================================================================
path_atlas = list.files(path_FC, "AAL3.rds", full.names = T)
results = smoothing_multiple_ROIs(path_atlas, 
                                  n_order = 4,
                                  n_breaks = 300,
                                  lambdas = exp(seq(-5, -4, 0.1)),  # 후보 smoothing 파라미터
                                  path_export,
                                  save_each_ROI = F,
                                  overwrite = F)



### 🟧 Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_ =====================================================================
path_atlas = list.files(path_FC, "Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_.rds", full.names = T)
results = smoothing_multiple_ROIs(path_atlas, 
                                  n_order = 4,
                                  n_breaks = NULL,
                                  lambdas = exp(seq(-3, -2, 0.1)),  # 후보 smoothing 파라미터
                                  path_export,
                                  save_each_ROI = T)




# 🟥 FunImgARglobalCWSF ==============================================================================================================================================
## 🟨 FC  ====================================================================================================================
path_FC = "E:/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARglobalCWSF/Fisher Z FC"
path_FC = path_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/Smoothing_FunImgARglobalCWSF_FC"
path_export = path_export %>% adjust_path



### 🟧 AAL3  ====================================================================================================================
path_atlas = list.files(path_FC, "AAL3.rds", full.names = T)
results = smoothing_multiple_ROIs(path_atlas, 
                                  n_order = 4,
                                  n_breaks = 300,
                                  lambdas = exp(seq(-5, -4, 0.1)),  # 후보 smoothing 파라미터
                                  path_export,
                                  save_each_ROI = F,
                                  overwrite = F)


### 🟧 Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_ =====================================================================
path_atlas = list.files(path_FC, "Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds", full.names = T)
results = smoothing_multiple_ROIs(path_atlas, 
                                  n_order = 4,
                                  n_breaks = NULL,
                                  lambdas = exp(seq(-3, -2, 0.1)),  # 후보 smoothing 파라미터
                                  path_export,
                                  save_each_ROI = T)










































