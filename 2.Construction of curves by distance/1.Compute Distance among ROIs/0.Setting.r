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
extract_xyz_coordinate = function(ith_atlas){
  # Get unique ROI numbers in the atlas, excluding background (0)
  roi_numbers <- unique(ith_atlas[ith_atlas > 0]) %>% sort
  
  # Store the data frame in the list
  tictoc::tic()
  roi_coordinates_list = lapply(roi_numbers, function(roi){
    # Find voxel positions for the current ROI
    voxel_coords <- which(ith_atlas == roi, arr.ind = TRUE)
    
    # Convert to a data frame
    roi_df <- as.data.frame(voxel_coords)
    
  }) %>% setNames(fit_length(roi_numbers, roi_numbers %>% max %>% nchar) %>% paste0("ROI_", .))
  tictoc::toc()
  
  return(roi_coordinates_list)
}





