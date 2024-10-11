# 🟥 Data Load ==============================================================================================================================================
path_FC_AAL3 = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/AAL3_.rds"
FC_AAL3 = readRDS(path_FC_AAL3)

# kth ROI
kth_ROI = FC_AAL3[[3]]
domain = kth_ROI$Euclid_dist
roi = kth_ROI$ROI
kth_ROI$ROI = kth_ROI$Euclid_dist = NULL



# 🟥 FC ==============================================================================================================================================
## 🟨 Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_ =====================================================================
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_.rds"
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_.rds"
path_export = "/Users/Ido/Downloads/untitled folder"  # 결과 저장 경로
results = smoothing_multiple_ROIs(path_FC, 
                                  n_order = 4, 
                                  lambdas = exp(seq(-1, 0, 0.01)),  # 후보 smoothing 파라미터
                                  path_export)


## 🟨 AAL3  ====================================================================================================================
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/AAL3_.rds"
path_FC = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC/AAL3_.rds"
path_export = "/Users/Ido/Downloads/untitled folder"  # 결과 저장 경로
results = smoothing_multiple_ROIs(path_FC, 
                                  n_order = 4, 
                                  lambdas = exp(seq(-5, -4, 0.1)),  # 후보 smoothing 파라미터
                                  path_export)






















