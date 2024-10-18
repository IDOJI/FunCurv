# 🟥 Trian data ==============================================================================================================================================
path_train = "/Users/Ido/Documents/✴️DataAnalysis/FunCurv/1.Data Indexing/2.Split train and test data/train_seed_4649.rds"
path_train = "E:/FunCurv/1.Data Indexing/2.Split train and test data/train_seed_4649.rds"
train_folded = readRDS(path_train)
path_test = "/Users/Ido/Documents/✴️DataAnalysis/FunCurv/1.Data Indexing/2.Split train and test data/test_seed_4649.rds"
path_test = "E:/FunCurv/1.Data Indexing/2.Split train and test data/test_seed_4649.rds"
test = readRDS(path_test)





# 🟥 FC ==============================================================================================================================================
## 🟨 옵션 테스트 ==============================================================================
# path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_FC"
# # 900
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_900Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 800
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_800Parcels_7Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 700
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_700Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 600
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_600Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 500
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_500Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 400
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_400Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 300
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_300Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 200
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_200Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 100
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_100Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# results = smoothing_multiple_ROIs(path_all_FC_atlas, 
#                                   train_RID,
#                                   n_order = 4,
#                                   n_breaks = NULL,
#                                   lambdas = exp(seq(-4, -3, 0.5)),  # 후보 smoothing 파라미터
#                                   path_export,
#                                   save_each_ROI = F,
#                                   overwrite = F)





## 🟨✅ 각 atlas에 대한 옵션 정의 ==============================================================================
options_for_each_atlas_list <- list(
  AAL3 = list(n_order = 4, 
              n_breaks = 300, 
              lambdas = exp(seq(-6, -5, 0.5))),
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
                      n_breaks = NULL, 
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


## 🟨✅ 공통 옵션 정의 ==============================================================================
common_options <- list(path_export = "/path/to/export", 
                       overwrite = FALSE, 
                       max_plots = 5  # 최대 2개의 ROI에 대해 플롯 생성
                       )



## 🟨 FunImgARCWSF ====================================================================================================================
path_all_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC"
path_all_FC = path_all_FC %>% adjust_path
# path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_FC"
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/Smoothing_FunImgARCWSF_FC"
common_options$path_export = path_export %>% adjust_path
apply_smoothing_to_all_atlas_files(
  path_all_FC = path_all_FC, 
  train_folded = train_folded,
  test = test,
  options_for_each_atlas_list = options_for_each_atlas_list, 
  common_options = common_options
)






## 🟨 FunImgARglobalCWSF ====================================================================================================================
path_all_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARglobalCWSF/Fisher Z FC"
path_all_FC = path_all_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/Smoothing_FunImgARglobalCWSF_FC"
# path_export = "/Users/Ido/Downloads/test"
common_options$path_export = path_export %>% adjust_path
apply_smoothing_to_all_atlas_files(
  path_all_FC = path_all_FC, 
  train_folded = train_folded,
  test = test,
  options_for_each_atlas_list = options_for_each_atlas_list, 
  common_options = common_options
)





# 🟥 ReHo ==============================================================================================================================================
## 🟨 옵션 테스트 ==============================================================================
# path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_ReHo"
# # 1000
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 900
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_900Parcels_7Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 800
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_800Parcels_7Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 700
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_700Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 600
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_600Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 500
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_500Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 400
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_400Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 300
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_300Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 200
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_200Parcels_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # 100
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/Schaefer2018_100Parcels_Kong2022_17Networks_order_FSLMNI152__resampled.nii.gz.rds"
# # AAL3
# path_all_FC_atlas = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo/AAL3.rds"
# results = smoothing_multiple_ROIs(path_all_FC_atlas, 
#                                   train_RID,
#                                   n_order = 4,
#                                   n_breaks = NULL,
#                                   lambdas = exp(seq(-4, -3, 0.5)),  # 후보 smoothing 파라미터
#                                   path_export,
#                                   save_each_ROI = F,
#                                   overwrite = F)



## 🟨✅  atlas에 대한 옵션 정의 ==============================================================================
options_for_each_atlas_list <- list(
  AAL3 = list(n_order = 4, 
              n_breaks = 300, 
              lambdas = exp(seq(-5, -4, 0.5))),
  "1000Parcels" = list(n_order = 4,
                       n_breaks = NULL, 
                       lambdas = exp(seq(-4, -3, 0.5))),
  
  "900Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  "800Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  
  "700Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-3, -2, 0.5))),
  
  
  "600Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "500Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  
  "400Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "300Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "200Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5))),
  "100Parcels" = list(n_order = 4,
                      n_breaks = NULL, 
                      lambdas = exp(seq(-2, -1, 0.5)))
)


## 🟨✅ 공통 옵션 정의 ==============================================================================
common_options <- list(path_export = "/path/to/export", 
                       overwrite = FALSE, 
                       max_plots = 6  # 최대 2개의 ROI에 대해 플롯 생성
)



## 🟨 FunImgARCWSF ====================================================================================================================
path_all_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/zReHo"
path_all_FC = path_all_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_zReHo"
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/Smoothing_FunImgARCWSF_zReHo"
# path_export = "/Users/Ido/Downloads/test"
common_options$path_export = path_export %>% adjust_path
apply_smoothing_to_all_atlas_files(
  path_all_FC = path_all_FC, 
  train_folded = train_folded,
  test = test,
  options_for_each_atlas_list = options_for_each_atlas_list, 
  common_options = common_options
)






## 🟨 FunImgARglobalCWSF ====================================================================================================================
path_all_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARglobalCWSF/zReHo"
path_all_FC = path_all_FC %>% adjust_path
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARglobalCWSF_zReHo"
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/Smoothing_FunImgARglobalCWSF_zReHo"
# path_export = "/Users/Ido/Downloads/test"
common_options$path_export = path_export %>% adjust_path
apply_smoothing_to_all_atlas_files(
  path_all_FC = path_all_FC,
  train_folded = train_folded,
  test = test,
  options_for_each_atlas_list = options_for_each_atlas_list, 
  common_options = common_options
)







