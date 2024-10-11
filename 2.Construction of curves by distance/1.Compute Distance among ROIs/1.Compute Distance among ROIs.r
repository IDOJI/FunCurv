# 🟥 Setting ==========================================================================================
path_save = "/Users/Ido/Documents/✴️Data/ADNI/RS.fMRI/2.Brain Atlas/3.Compute Distance among ROIs"




# 🟥 Load the atlas list ==========================================================================================
path_atlas_list = "/Users/Ido/Documents/✴️Data/ADNI/RS.fMRI/2.Brain Atlas/2.Extract Coordinates of ROIs for each Atals/extracted coordinates for each ROI.rds"
atlas = readRDS(path_atlas_list)
atlas$Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz
names(atlas)




# 🟩 Euclid Distance ==========================================================================================
# 하나의 ROI에 대해 중심 좌표를 계산하는 함수
get_centroid <- function(roi_data) {
  apply(roi_data, 2, mean) # 평균으로 중심 좌표를 계산
}


# 하나의 atlas에 대해 각 ROI의 중심 좌표와 거리 행렬을 계산하는 함수
calculate_distances_for_atlas <- function(atlas) {
  centroids <- sapply(names(atlas), function(roi) {
    get_centroid(atlas[[roi]])
  })
  
  # 중심 좌표를 행렬로 변환 (각 열이 x, y, z 좌표를 나타냄)
  centroids <- t(centroids)
  
  # 뇌 영역 간의 유클리드 거리 계산
  distances <- dist(centroids)
  
  # 거리 행렬을 데이터프레임으로 변환
  distance_matrix <- as.matrix(distances)
  
  return(distance_matrix)
}


# 모든 atlas에 대해 ROI 중심 좌표와 거리 행렬을 계산하는 함수
calculate_distances_for_all_atlases <- function(atlas_list) {
  distance_results <- list()
  
  for (atlas_name in names(atlas_list)) {
    # atlas 객체가 AAL3처럼 좌표 정보가 있는지 확인
    if (is.list(atlas_list[[atlas_name]]) && length(atlas_list[[atlas_name]]) > 0) {
      message(paste("Calculating distances for", atlas_name))
      distance_results[[atlas_name]] <- calculate_distances_for_atlas(atlas_list[[atlas_name]])
    } else {
      message(paste("Skipping", atlas_name, "- no ROI data available."))
    }
  }
  
  return(distance_results)
}



# 모든 atlas에 대해 거리 계산
distance_matrices = calculate_distances_for_all_atlases(atlas)
distance_matrices$Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz %>% View


# Save
file_name = "Distance among ROIs___Mean Euclid Distance Matrices.rds"
saveRDS(distance_matrices, file.path(path_save, file_name))







