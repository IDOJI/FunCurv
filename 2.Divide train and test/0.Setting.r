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
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2")
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
##  🟩 plot ===============================================================================
ggplot___lines = function(df,
                          col_names = NULL,
                          x_col = "Year",
                          x_axis_vals = NULL,
                          # options
                          point = T,
                          show.legend = T,
                          # labels
                          title = "Timeseries",
                          xlab = "Year",
                          ylab = "Value",
                          color_legend_title = "Category",
                          # export
                          path_Export = NULL,
                          file.name = NULL,
                          width = 20,
                          height = 5){
  # 🟥 Colnames ====================================================================
  ## 🟨 Check input ===============================================================
  if(is.null(col_names)){
    col_names = colnames(df)
    col_names = col_names[col_names != x_col]
  }
  
  
  ## 🟨 Subset ===============================================================
  df_selected = df[, c(x_col, col_names), drop = FALSE]
  
  
  
  
  
  
  
  # 🟥 x-axis ====================================================================
  if(is.null(x_axis_vals)){
    x_axis_vals = 1:nrow(df_selected)
  }
  if(nrow(df_selected)!=length(x_axis_vals)){
    stop("Compare the length of x_axis_vals and the rows of df")
  }
  x_axis_labs = df_selected[, x_col]
  
  
  
  
  
  
  # 🟥 Transform data into long format ============================================================
  df_selected$Year = df_selected$Year %>% as.numeric
  long_df = df_selected %>% pivot_longer(cols = -!!x_col,
                                         names_to = "Category",
                                         values_to = "Value") %>% dplyr::arrange(Category, !!x_col)
  
  
  
  
  # 🟥 Add x-axis vals ============================================================
  x.axis_df = cbind(x_axis_vals = x_axis_vals, long_df)
  x.axis_df$Value = as.numeric(x.axis_df$Value)
  x.axis_df$x_axis_vals = as.numeric(x.axis_df$x_axis_vals)
  
  
  
  # 🟥 plotting ====================================================================
  ## 🟨 Line ============================================================================
  p <- ggplot() +
    geom_line(data = x.axis_df, aes(x = x_axis_vals, y = Value, group = Category, color = Category),
              show.legend = show.legend)
  
  
  
  
  ## 🟨 Point ============================================================================
  if(point){
    
    p = p + geom_point(data = x.axis_df,
                       aes(x = x_axis_vals, y = Value, group = Category, color = Category),
                       show.legend = FALSE) # 선 위에 점 추가, 범례는 이미 geom_line에서 표시했으므로 여기서는 표시하지 않음
    
  }
  
  
  
  ## 🟨 Lables ============================================================================
  p = p + scale_x_discrete(limits = x_axis_labs) + # x축 라벨 지정
    theme_minimal() +
    labs(title = title, x = xlab, y = ylab, color = color_legend_title)
  
  
  
  
  ## 🟨 Theme ============================================================================
  p = p + theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold"), # 타이틀 가운데 정렬
                axis.title.x = element_text(size = 20, face = "bold"), # x 축 라벨 크기 및 굵기 조정
                axis.title.y = element_text(size = 20, face = "bold"), # y 축 라벨 크기 및 굵기 조정
                legend.title = element_text(size = 17, face = "bold") # 범례 제목 크기 및 굵기 조정
  )
  
  
  
  # 🟥 Exporting =================================================================================
  if(!is.null(path_Export)){
    ggsave(paste0(path_Export, "/", file.name, ".png"), p, bg = "white", width = width, height = height)
  }
  
  return(p)
}


## 🟩 FC ==================================================================================================
extract_unique_roi <- function(data_list) {
  # ROI 열 추출
  roi_list <- lapply(data_list, function(kth_rid) {
    kth_rid[,"ROI"]
  })
  
  # 모든 ROI 열이 동일한지 확인
  if (all(sapply(roi_list, function(x) identical(x, roi_list[[1]])))) {
    # 모두 같으면 첫 번째 ROI만 남김
    unique_roi <- roi_list[[1]]
  } else {
    # 동일하지 않으면 에러 메시지 출력
    stop("Error: Not all ROI columns are identical.")
  }
  
  return(unique_roi)
}



extract_unique_dist <- function(data_list) {
  # ROI 열 추출
  roi_list <- lapply(data_list, function(kth_rid) {
    kth_rid[,"Euclid_Distance"]
  })
  
  # 모든 ROI 열이 동일한지 확인
  if (all(sapply(roi_list, function(x) identical(x, roi_list[[1]])))) {
    # 모두 같으면 첫 번째 ROI만 남김
    unique_roi <- roi_list[[1]]
  } else {
    # 동일하지 않으면 에러 메시지 출력
    stop("Error: Not all Dist columns are identical.")
  }
  
  return(unique_roi)
}




# 함수 정의
extract_fc_data <- function(ith_fc, ith_sorted_dist) {
  each_roi_sorted_fc_data <- lapply(names(ith_fc), function(rid) {
    roi <- names(ith_sorted_dist)[1]
    dist_each_roi <- ith_sorted_dist[[roi]]
    
    # FC 값을 필터링
    fc <- ith_fc[[rid]][, roi] %>%
      keep(names(.) %in% names(dist_each_roi)) %>%
      .[names(dist_each_roi)]
    
    # 데이터프레임 생성
    df <- data.frame(
      ROI_1 = names(dist_each_roi),
      ROI_2 = names(fc),
      Euclid_Distance = dist_each_roi,
      FC = fc,
      stringsAsFactors = FALSE
    )
    row.names(df) <- NULL
    
    # ROI_1과 ROI_2가 동일한 경우 처리
    if (all(df$ROI_1 == df$ROI_2)) {
      df$ROI_1 <- NULL
      df <- df %>% rename(ROI = ROI_2)
    }
    
    return(df)  
  }) %>% setNames(names(ith_fc))
  
  return(each_roi_sorted_fc_data)
}



# 함수 정의
extract_fc_columns <- function(data_list) {
  # 각 데이터프레임에서 FC 열을 추출하고 cbind 형태로 결합
  fc_data <- do.call(cbind, lapply(data_list, function(df) df$FC)) %>% 
    as.data.frame
  
  # 열 이름을 원래 리스트의 원소 이름으로 변경
  colnames(fc_data) <- names(data_list)
  
  return(fc_data)
}


library(dplyr)
library(crayon)
library(tools)

library(dplyr)
library(crayon)
library(tools)

# FC 데이터를 처리하고 저장하는 함수 정의
process_and_save_fc_data <- function(path_folder, path_save, sorted_dist) {
  # FC 데이터 파일 목록 가져오기
  fc_data_list <- list.files(path_folder, full.names = TRUE)
  
  # 각 파일에 대해 반복
  for (ith_fc_path in fc_data_list) {
    # 아틀라스 이름 추출
    ith_atlas <- basename(ith_fc_path) %>%
      file_path_sans_ext() %>%
      sub("_combined_Fisher_Z_fc$", "", .)
    
    # 저장할 파일 경로 설정
    save_file_path <- file.path(path_save, paste0(ith_atlas, "_.rds"))
    
    # 파일이 이미 존재하는지 확인하고, 존재하면 건너뜀
    if (file.exists(save_file_path)) {
      cat(crayon::red(paste("File already exists for atlas:", ith_atlas, ". Skipping processing.\n")))
      next
    }
    
    # 시작 시간 기록
    total_start_time <- Sys.time()
    
    # FC 데이터 읽기
    ith_fc <- readRDS(ith_fc_path)
    
    # 거리 정보 정렬
    ith_sorted_dist <- sorted_dist[[ith_atlas]]
    
    # 초기화
    ith_sorted_FC_data <- list()
    
    # 각 ROI에 대해 반복
    for (roi in names(ith_sorted_dist)) {
      # 시작 시간 기록
      start_time <- Sys.time()
      
      # 각 ROI에 대한 데이터 처리
      each_roi_sorted_fc_data <- extract_fc_data(ith_fc, ith_sorted_dist)
      
      # 데이터프레임 합치기
      unique_roi_result <- extract_unique_roi(each_roi_sorted_fc_data)
      unique_dist_result <- extract_unique_dist(each_roi_sorted_fc_data)
      combined_fc_data <- extract_fc_columns(each_roi_sorted_fc_data) %>%
        cbind(ROI = unique_roi_result, Euclid_dist = unique_dist_result, .)
      
      # 결과 저장
      ith_sorted_FC_data[[roi]] <- combined_fc_data
      
      # 종료 시간 기록 및 소요 시간 계산
      end_time <- Sys.time()
      elapsed_time <- end_time - start_time
      
      # 결과 출력: ROI와 소요 시간을 각각 다른 색상으로 출력
      cat(crayon::blue(paste("Finished processing ROI:", roi, "\n")))
      cat(crayon::green(paste("Time taken for ROI", roi, ":", round(elapsed_time, 2), "seconds\n")))
    }
    
    # 결과를 파일로 저장
    saveRDS(ith_sorted_FC_data, save_file_path)
    
    # 전체 소요 시간 출력
    total_end_time <- Sys.time()
    total_elapsed_time <- total_end_time - total_start_time
    cat(crayon::yellow(paste0("Finished processing atlas: ", ith_atlas, " | Total time taken: ", round(total_elapsed_time, 2), " seconds\n")))
  }
}


## 🟩 ReHo, DC, ALFF ==================================================================================================
process_roi_data <- function(path_data_folder, sorted_dist, path_save) {
  # 저장할 경로가 존재하지 않으면 생성
  if (!dir.exists(path_save)) {
    dir.create(path_save, recursive = TRUE)
  }
  
  # `Mean__`으로 시작하는 모든 rds 파일의 경로 가져오기
  path_target_data <- list.files(path_data_folder, pattern = "^Mean__.*\\.rds$", full.names = TRUE)
  
  # atlas 이름 추출
  target_file_names <- list.files(path_data_folder, pattern = "\\.rds$", full.names = FALSE) %>%
    sub("\\.rds$", "", .)
  target_file_names = target_file_names[!grepl("^Mean__", target_file_names)]
  
  
  # 정렬된 dist 목록의 이름이 파일 이름에 모두 포함되는지 확인
  if (all(names(sorted_dist) %in% target_file_names)) {
    for (target_atlas_name in target_file_names) {
      # target_atlas_name = target_file_names[2]
      # 이미 저장된 파일이 존재하는지 확인
      save_file_path <- file.path(path_save, paste0(target_atlas_name, ".rds"))
      if (file.exists(save_file_path)) {
        cat(yellow(paste0("Skipping: ", target_atlas_name, " (File already exists)\n")))
        next
      }
      
      # 처리 시작 시간 기록
      start_time <- Sys.time()
      
      # 해당 atlas에 대한 dist 데이터와 target data 로드
      target_atlas <- sorted_dist[[target_atlas_name]]
      target_data <- readRDS(path_target_data[which(target_file_names == target_atlas_name)])
      # target_data %>% View
      
      # RID 추출 및 target_data의 RID 열 제거
      RID <- regmatches(target_data$RID, regexpr("RID_\\d+", target_data$RID))
      target_data$RID <- NULL
      
      # 각 ROI에 대해 데이터 재구성 및 시간 소요 출력
      rearranged_data <- lapply(names(target_atlas), function(roi) {
        # ROI 처리 시작 시간 기록
        roi_start_time <- Sys.time()
        
        # ROI 데이터와 거리 정보 추출
        roi_dist <- target_atlas[[roi]]
        roi_data <- target_data[, names(roi_dist)] %>% t()
        
        # ROI 데이터의 행 이름과 ROI 거리의 이름이 일치하는지 확인
        if (all(rownames(roi_data) == names(roi_dist))) {
          rownames(roi_data) <- NULL
          colnames(roi_data) <- RID
          roi_data_2 <- cbind(ROI = names(roi_dist), Euclid_Dist = roi_dist, roi_data) %>%
            as_tibble()
        } else {
          stop(paste("Error: Mismatch in rownames for ROI:", roi))
        }
        
        # ROI 처리 완료 시간 기록 및 소요 시간 계산
        roi_end_time <- Sys.time()
        roi_elapsed_time <- round(difftime(roi_end_time, roi_start_time, units = "secs"), 2)
        
        # 각 ROI에 대해 소요된 시간 출력
        cat(cyan(paste0("Processed ROI: ", roi, " (Time: ", roi_elapsed_time, " seconds)\n")))
        
        return(roi_data_2)
      }) %>% setNames(names(target_atlas)) # roi
      
      # 데이터 저장
      saveRDS(rearranged_data, save_file_path)
      
      # 처리 완료 시간 기록 및 소요 시간 계산
      end_time <- Sys.time()
      elapsed_time <- round(difftime(end_time, start_time, units = "secs"), 2)
      
      # 시간 소요 출력
      cat(green(paste0("Completed: ", target_atlas_name, " (Time: ", elapsed_time, " seconds)\n")))
    }
  } else {
    cat(red("Error: Some atlas names are not in the target file names.\n"))
  }
}



