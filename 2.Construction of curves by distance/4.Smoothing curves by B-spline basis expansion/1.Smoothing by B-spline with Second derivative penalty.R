# 🟥 FC ==============================================================================================================================================
## 🟨 FunImgARCWSF  ====================================================================================================================
path_FC = "E:/FunCurv/2.Construction of curves by distance/3.Curves by Distance/FunImgARCWSF/Fisher Z FC"
path_export = "E:/smoothing_FC"

### 🟨 Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_ =====================================================================
path_atlas = list.files(path_FC, "Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz_.rds", full.names = T)
results = smoothing_multiple_ROIs(path_atlas, 
                                  n_order = 4, 
                                  lambdas = exp(seq(-3, -2, 0.1)),  # 후보 smoothing 파라미터
                                  path_export)


### 🟨 AAL3  ====================================================================================================================
path_atlas = list.files(path_FC, "AAL3_.rds", full.names = T)
results = smoothing_multiple_ROIs(path_atlas, 
                                  n_order = 4, 
                                  lambdas = exp(seq(-4, -1, 0.1)),  # 후보 smoothing 파라미터
                                  path_export)






















