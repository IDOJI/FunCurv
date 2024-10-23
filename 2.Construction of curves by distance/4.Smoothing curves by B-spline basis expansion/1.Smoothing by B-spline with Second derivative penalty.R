# 🟥 smoothing all subjects ==============================================================================================================================================
path_test_train_subjects_list = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data"
path_curves_by_distance = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance"
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion"
atlas_name = "AAL3"

path_all_subjects_list = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv"

results = apply_by_target_group(path_all_subjects_list,
                                path_test_train_subjects_list,
                                path_curves_by_distance,
                                path_export,
                                atlas_name)






