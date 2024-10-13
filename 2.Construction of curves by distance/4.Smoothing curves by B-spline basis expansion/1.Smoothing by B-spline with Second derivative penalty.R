# 🟥 Trian data ==============================================================================================================================================
path_train = "/Users/Ido/Documents/✴️DataAnalysis/FunCurv/1.Data Indexing/2.Split train and test data/all_train_data_seed_4649.rds"
# path_train = "C:/Users/clair/OneDrive/바탕 화면/FunCurv/all_train_data_seed_4649.rds"
train = readRDS(path_train)
train_RID = sprintf("RID_%04d", train$RID)




# 🟥 FC ==============================================================================================================================================
## 🟨 옵션 테스트 ==============================================================================
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_FC"
# 900
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_900Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 800
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_800Parcels_7Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 700
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_700Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 600
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_600Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 500
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_500Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 400
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_400Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 300
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_300Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 200
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_200Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 100
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_100Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
results = smoothing_multiple_ROIs(path_FC_atlas, 
                                  train_RID,
                                  n_order = 4,
                                  n_breaks = NULL,
                                  lambdas = exp(seq(-4, -3, 0.5)),  # 후보 smoothing 파라미터
                                  path_export,
                                  save_each_ROI = F,
                                  overwrite = F)



## 🟨 각 atlas에 대한 옵션 정의 ==============================================================================
options_for_each_atlas_list <- list(
  AAL3 = list(n_order = 4, 
              n_breaks = 300, 
              lambdas = exp(seq(-5, -4, 0.5))),
  "1000Parcels" = list(n_order = 4,
                       n_breaks = NULL, 
                       lambdas = exp(seq(-3, -2, 0.5))),
  "900Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "800Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "700Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "600Parcels" = list(n_order = 4,
                      n_breaks = 200, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "500Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "400Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "300Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "200Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "100Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-4, -3, 0.5)))
)


## 🟨 FunImgARCWSF ====================================================================================================================
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC"
path_FC = path_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_FC"
path_export = path_export %>% adjust_path
apply_smoothing_to_atlas_files(base_path = path_FC, 
                               options_for_each_atlas_list = options_for_each_atlas_list, 
                               common_options = list(path_export = path_export, 
                                                     save_each_ROI = FALSE, 
                                                     overwrite = FALSE,
                                                     train_RID = train_RID)
                               )


## 🟨 FunImgARglobalCWSF ====================================================================================================================
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARglobalCWSF/Fisher Z FC"
path_FC = path_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARglobalCWSF_FC"
path_export = path_export %>% adjust_path
apply_smoothing_to_atlas_files(base_path = path_FC, 
                               options_for_each_atlas_list = options_for_each_atlas_list, 
                               common_options = list(path_export = path_export, 
                                                     save_each_ROI = FALSE, 
                                                     overwrite = FALSE,
                                                     train_RID = train_RID)
))




# 🟥 ReHo ==============================================================================================================================================
## 🟨 옵션 테스트 ==============================================================================
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_ReHo"
# 1000
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 900
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_900Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 800
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_800Parcels_7Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 700
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_700Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 600
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_600Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 500
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_500Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 400
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_400Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 300
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_300Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 200
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_200Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# 100
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_100Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# AAL3
path_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/AAL3.rds"
results = smoothing_multiple_ROIs(path_FC_atlas, 
                                  train_RID,
                                  n_order = 4,
                                  n_breaks = NULL,
                                  lambdas = exp(seq(-3, -2, 0.5)),  # 후보 smoothing 파라미터
                                  path_export,
                                  save_each_ROI = F,
                                  overwrite = F)



## 🟨 각 atlas에 대한 옵션 정의 ==============================================================================
options_for_each_atlas_list <- list(
  AAL3 = list(n_order = 4, 
              n_breaks = 300, 
              lambdas = exp(seq(-5, -4, 0.5))),
  "1000Parcels" = list(n_order = 4,
                       n_breaks = NULL, 
                       lambdas = exp(seq(-3, -2, 0.5))),
  "900Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "800Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "700Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "600Parcels" = list(n_order = 4,
                      n_breaks = 200, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "500Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "400Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "300Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "200Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "100Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5)))
)


## 🟨 FunImgARCWSF ====================================================================================================================
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC"
path_FC = path_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_FC"
path_export = path_export %>% adjust_path
apply_smoothing_to_atlas_files(base_path = path_FC, 
                               options_for_each_atlas_list = options_for_each_atlas_list, 
                               common_options = list(path_export = path_export, 
                                                     save_each_ROI = FALSE, 
                                                     overwrite = FALSE,
                                                     train_RID = train_RID)
)


## 🟨 FunImgARglobalCWSF ====================================================================================================================
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARglobalCWSF/Fisher Z FC"
path_FC = path_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARglobalCWSF_FC"
path_export = path_export %>% adjust_path
apply_smoothing_to_atlas_files(base_path = path_FC, 
                               options_for_each_atlas_list = options_for_each_atlas_list, 
                               common_options = list(path_export = path_export, 
                                                     save_each_ROI = FALSE, 
                                                     overwrite = FALSE,
                                                     train_RID = train_RID)
))







