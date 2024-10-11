# 🟥 Load the dist matrix ==========================================================================================
path_dist_mat = "/Users/Ido/Documents/✴️DataAnalysis/ADNI/RS.fMRI/@@@2.Brain Atlas/3.Compute Distance among ROIs/Distance among ROIs___Mean Euclid Distance Matrices.rds"
dist_mat = readRDS(path_dist_mat)

# dist_mat$Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz %>% class




# 🟩 Euclid Distance ==========================================================================================
# 모든 atlas에 대해 ROI 간 거리 정렬
sorted_distance_data <- process_all_atlases(dist_mat)

# Save
path_save = "/Users/Ido/Documents/✴️DataAnalysis/ADNI/RS.fMRI/@@@2.Brain Atlas/4.Arrange the distances for each ROI by the size"
file_name = "Sorted ROI by dist.rds"
saveRDS(sorted_distance_data, file.path(path_save, file_name))
























