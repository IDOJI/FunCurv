# 🟥 FC curves =============================================================================================
## 🟨 FunImgARCWSF ===================================================================================================
input_paths = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/Smoothing_FunImgARCWSF_FC"
output_path <- "E:/FPCA_FunImgARCWSF_FC"
output_path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FPCA_FunImgARCWSF_FC"
perform_fpca_for_multiple_atlases(input_paths %>% convert_path, output_path)



## 🟨 FunImgARglobalCWSF ===================================================================================================
input_paths <- "/Users/Ido/Documents/✴️DataAnalysis/FunCurv/2.Construction of curves by distance/4.Smoothing Curves by B-spline basis expansion/FunImgARCWSF_FC"
input_paths = "E:/Smoothing_FunImgARglobalCWSF_FC"
output_path <- "E:/FPCA_FunImgARglobalCWSF_FC"
perform_fpca_for_multiple_atlases(input_paths %>% convert_path, output_path)




# 🟥 zReHo =============================================================================================
## 🟨 FunImgARCWSF ===================================================================================================
input_paths <- "E:/Smoothing_FunImgARCWSF_zReHo"
output_path <- "E:/FPCA_FunImgARCWSF_FC"
perform_fpca_for_multiple_atlases(input_paths %>% convert_path, output_path)



## 🟨 FunImgARglobalCWSF ===================================================================================================
input_paths <- "E:/Smoothing_FunImgARglobalCWSF_zReHo"
output_path <- "E:/FPCA_FunImgARglobalCWSF_FC"
perform_fpca_for_multiple_atlases(input_paths %>% convert_path, output_path)






