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
##  🟩 path ===============================================================================
# 경로 자동 변환 함수 정의
adjust_path <- function(path) {
  # 운영체제에 따라 기본 경로 앞부분 설정
  if (.Platform$OS.type == "windows") {
    # macOS 경로를 Windows 경로로 변환
    path <- sub("^/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/", "E:/", path)
  } else if (.Platform$OS.type == "unix" && grepl("darwin", R.version$os)) {
    # 이미 macOS 경로이므로 그대로 유지
    path <- path
  } else {
    stop("지원되지 않는 운영체제입니다.")
  }
  
  # 최종 경로 반환
  return(path)
}


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
combine_fisher_z_fc <- function(data_list) {
  # Dist와 ROI가 동일한지 확인
  dist_roi_check <- lapply(data_list, function(df) df[, c("Dist", "ROI")])
  # x = dist_roi_check[[2]]
  if (!all(sapply(dist_roi_check, function(x) identical(x, dist_roi_check[[1]])))) {
    stop("Error: Not all 'Dist' and 'ROI' columns are identical across data frames.")
  }
  
  # 동일한 Dist와 ROI를 기준으로 결합
  dist_and_roi <- dist_roi_check[[1]]  # Dist와 ROI를 포함한 첫 번째 데이터프레임
  
  # 각 데이터프레임의 Fisher_Z_FC 열 이름을 리스트 원소 이름으로 변경하여 cbind
  combined_data <- lapply(names(data_list), function(name) {
    # name = names(data_list)[1]
    df <- data_list[[name]]
    df[["Fisher_Z_FC"]] %>% as_tibble %>% setNames(name)  # 열 이름을 리스트 원소 이름으로 변경
  }) %>% do.call(cbind, .) %>% cbind(dist_and_roi, .) %>% as_tibble
  
  return(combined_data)
}

# FC 데이터를 처리하고 저장하는 함수 정의
process_and_save_fc_data <- function(path_folder, path_save, sorted_dist) {
  require(tools)  # 'tools' 패키지 로드
  fc_data_list <- list.files(path_folder, full.names = TRUE)  # FC 데이터 파일 목록 가져오기
  
  for (ith_fc_path in fc_data_list) {
    # 아틀라스 이름 추출 및 저장 경로 생성
    ith_atlas <- basename(ith_fc_path) %>%
      file_path_sans_ext() %>%
      sub("_combined_Fisher_Z_fc$", "", .)
    
    save_file_path <- file.path(path_save, paste0(ith_atlas, ".rds"))
    
    # 파일이 이미 존재하면 건너뛰기
    if (file.exists(save_file_path)) {
      cat(crayon::red(paste("File already exists for atlas:", ith_atlas, ". Skipping processing.\n")))
      next  # 다음 파일로 넘어가기
    }
    
    total_start_time <- Sys.time()  # 전체 처리 시간 시작
    
    # 파일 읽기, 에러 처리
    tryCatch({
      ith_fc <- readRDS(ith_fc_path)
    }, error = function(e) {
      cat(crayon::red(paste("Error reading file:", ith_fc_path, "\n")))
      next  # 파일 읽기에 실패하면 건너뛰기
    })
    
    # 거리 정보 정렬
    ith_sorted_dist <- sorted_dist[[ith_atlas]]
    ith_sorted_FC_data <- list()
    
    # 정렬 수행 및 시간 측정
    tictoc::tic("Sorting")
    each_rid_sorted_FC_list <- lapply(names(ith_fc), function(rid) {
      rid_fc <- ith_fc[[rid]]
      
      rid_sorted_FC_list <- lapply(names(ith_sorted_dist), function(roi) {
        roi_rid_fc <- rid_fc[, roi]
        roi_dist <- ith_sorted_dist[[roi]]
        selected_roi_rid_fc <- roi_rid_fc[names(roi_rid_fc) %in% names(roi_dist)]
        sorted_selected_roi_rid_fc <- selected_roi_rid_fc[names(roi_dist)]
        data.frame(
          Dist = roi_dist,
          ROI = names(sorted_selected_roi_rid_fc),
          Fisher_Z_FC = sorted_selected_roi_rid_fc
        ) %>% as_tibble()
      }) %>% setNames(names(ith_sorted_dist))
      
      return(rid_sorted_FC_list)
    }) %>% setNames(names(ith_fc))
    tictoc::toc()  # 정렬 종료
    
    # ROI 별로 데이터 처리 및 시간 측정
    final_combined_data_list <- lapply(names(ith_sorted_dist), function(roi) {
      start_time <- Sys.time()  # ROI 처리 시작 시간
      
      combined_roi_fc <- lapply(names(each_rid_sorted_FC_list), function(rid) {
        each_rid_sorted_FC_list[[rid]][[roi]]
      }) %>%
        setNames(names(each_rid_sorted_FC_list)) %>%
        combine_fisher_z_fc  # Fisher_Z_FC 결합
      
      end_time <- Sys.time()  # ROI 처리 종료 시간
      elapsed_time <- end_time - start_time  # 소요 시간 계산
      
      # 처리 완료 메시지 출력
      cat(crayon::blue(paste0("Finished processing atlas: ", ith_atlas, 
                              " | ROI: ", roi, "\n")))
      cat(crayon::green(paste0("Time taken for ROI ", roi, ": ", 
                               round(elapsed_time, 2), " seconds\n")))
      
      return(combined_roi_fc)  # ROI 데이터 반환
    }) %>% setNames(names(ith_sorted_dist))
    
    # 최종 결과 저장
    saveRDS(final_combined_data_list, save_file_path)
    
    # 전체 처리 시간 계산 및 출력
    total_end_time <- Sys.time()
    total_elapsed_time <- total_end_time - total_start_time
    cat(crayon::yellow(paste0("Finished processing atlas: ", ith_atlas, 
                              " | Total time taken: ", 
                              round(total_elapsed_time, 2), " seconds\n")))
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



