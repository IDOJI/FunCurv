# 🟥 옵션 정의 ==============================================================================================================================================
target_group_list = list()
target_group_list[[1]] = "AD, MCI"
target_group_list[[2]] = "AD, CN"
target_group_list[[3]] = "MCI, CN"
target_group_list[[4]] = "AD, MCI, CN"

path_test_train_subjects_list = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data"
path_curves_by_distance = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/3.Curves by Distance"
path_export = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/2.Construction of curves by distance/4.Smoothing curves by B-spline basis expansion"
path_export = "E:/4.Smoothing curves by B-spline basis expansion"
atlas_name = "AAL3"

results = lapply(target_group_list, function(target_group){
  # target_group = target_group_list[[1]]
  apply_by_target_group(target_group,
                        path_test_train_subjects_list,
                        path_curves_by_distance,
                        path_export,
                        atlas_name)
})






