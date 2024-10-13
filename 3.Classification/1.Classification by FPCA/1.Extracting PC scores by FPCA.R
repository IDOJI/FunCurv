# 🟥 Load Train =============================================================================================
path_train = "/Users/Ido/Documents/✴️DataAnalysis/FunCurv/1.Data Indexing/2.Split train and test data/train_seed_4649.rds"
train_fold = readRDS(path_train)



# 🟥 FC curves =============================================================================================
## 🟨 FunImgARCWSF ===================================================================================================
input_paths <- "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_ReHo"
output_path <- "/Users/Ido/Downloads/untitled folder 2"
perform_fpca_for_multiple_atlases(train_fold, input_paths, output_path)



## 🟨 FunImgARglobalCWSF ===================================================================================================
input_paths <- "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion/FunImgARCWSF_ReHo"
output_path <- "/Users/Ido/Downloads/untitled folder 2"
perform_fpca_for_multiple_atlases(input_paths, output_path)










