# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE, quietly = T)
    }
  }
}

List.list = list()
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer")
List.list[[2]] = stat = c("fda", "MASS")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr", "readr", "caret", "readxl")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)

## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select








# 🟥 Define Functions ##########################################################################
fit_length <- function(number, digits) {
  # Convert the number to a string and pad with leading zeros
  formatted_number <- sprintf(paste0("%0", digits, "d"), number)
  return(formatted_number)
}

## 🟧 Extract coordinates ===========================================================================
# 각 ROI별로 거리를 정렬하는 함수 정의
# distance_matrix = dist_mat$Schaefer2018_1000Parcels_17Networks_order_FSLMNI152__resampled.nii.gz
# roi_name = "ROI_0965"
sort_distances_for_roi <- function(distance_matrix, roi_name) {
  # 해당 ROI와 다른 모든 ROI 간의 거리 추출
  distances <- distance_matrix[roi_name, ]
  
  # 자기 자신을 제외한 거리를 추출 (거리 값이 0인 경우)
  distances <- distances[distances != 0]
  
  # 거리를 기준으로 정렬
  sorted_distances <- sort(distances)
  
  # ROI 이름과 거리를 리스트로 반환
  return(sorted_distances)
}

# 특정 atlas에 대해 각 ROI별로 거리를 정렬한 리스트 생성
process_atlas <- function(distance_matrix) {
  # distance_matrix의 행 이름 (ROI 이름들)
  roi_names <- rownames(distance_matrix)
  
  # 결과를 저장할 리스트 초기화
  roi_distance_list <- list()
  
  # 각 ROI에 대해 거리 계산 및 정렬
  for (roi_name in roi_names) {
    # roi_name = roi_names[1]
    sorted_distances <- sort_distances_for_roi(distance_matrix, roi_name)
    roi_distance_list[[roi_name]] <- sorted_distances
  }
  
  return(roi_distance_list)
}

# 모든 atlas에 대해 ROI 간 거리를 정렬하는 함수
process_all_atlases <- function(all_distance_matrices) {
  # all_distance_matrices = dist_mat
  # 결과를 저장할 리스트 초기화
  all_atlas_sorted_distances <- list()
  
  # 각 atlas에 대해 반복
  for (atlas_name in names(all_distance_matrices)) {
    # atlas_name = names(all_distance_matrices)[1]
    message(paste("Processing atlas:", atlas_name))
    # 각 atlas에 대해 거리 정렬
    sorted_distances <- process_atlas(distance_matrix = all_distance_matrices[[atlas_name]])
    all_atlas_sorted_distances[[atlas_name]] <- sorted_distances
  }
  
  return(all_atlas_sorted_distances)
}





