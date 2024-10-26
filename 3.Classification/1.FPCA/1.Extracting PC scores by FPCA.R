# Remove old files
# pattern = "FPCA_Train_AAL3"
# path = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA"
# path_files_to_remove =  list.files(path, pattern  = pattern, full.names = T, recursive = T)
# # .rds 파일 삭제
# rds_files <- grep("\\.rds$", path_files_to_remove, value = TRUE)
# file.remove(rds_files)


# 🟩 FPCA ===========================================================================================
path_all_splitted_subjects = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data"
path_all_smoothed_results = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion"
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA"
apply_FPCA_to_all_measures(path_all_splitted_subjects,
                           path_all_smoothed_results,
                           path_export,
                           atlas_name = "AAL3")

