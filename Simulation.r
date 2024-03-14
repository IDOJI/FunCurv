# rm(list=ls())
# 🟥 Load Functions & Packages ##########################################################################
## 🟧Loading my functions ======================================================
# Check my OS
os <- Sys.info()["sysname"]
if(os ==  "Darwin"){
  
  path_OS = "/Users/Ido" # mac
  
}else if(os ==  "Window"){
  
  path_OS = "C:/Users/lleii"  
  
}
path_Dropbox = paste0(path_OS, "/Dropbox")
path_GitHub = list.files(path_Dropbox, pattern = "GitHub", full.names = T)
path_GitHub_Code = paste0(path_GitHub, "/GitHub___Code")
Rpkgs = c("ADNIprep", "StatsR", "refineR", "dimR")
Load = sapply(Rpkgs, function(y){
  list.files(path = path_GitHub_Code, pattern = y, full.names = T) %>% 
    paste0(., "/", y,"/R") %>% 
    list.files(., full.names = T) %>% 
    purrr::walk(source)
})



## 🟧Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE)
    }
  }
}

List.list = list()
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer")
List.list[[2]] = stat = c("fda", "MASS")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)






# 🟥 Define functions #########################################################################
## 🟧 Data Random sampling ====================================================================
### 🟨 Sampling function  --------------------------------------------------------------------------------------
Data_Sampling_for_One_Combination = function(Demo, 
                                             FC_Curves,
                                             FC_Matrix,
                                             Pipeline = c("FunImgARCWSF", "FunImgARglobalCWSF"),
                                             BandType = c("MB", "SB"),
                                             Diagnosis = c("AD", "CN"),
                                             proportion = c(0.1, 0.9),
                                             sample_size = c(100, 500, 1000),
                                             seed,
                                             path_save){
  # ✅Arguments =================================================================== 
  # FC_Curves: List of 164 region
  # Demo: subjects list
  # FC_Curves = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/✴️DataAnalysis___FDA on RS-fMRI FC Euclidean/1.Sorting FC by Euclidean distance/Sorted.FC.by.Dist___FunImgARglobalCWSF.rds")
  # Demo = read.csv("/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/㊙️CommonData___ADNI___RS-fMRI___BOLD/RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list_(Selected_Variables).csv")
  
  
  
  # ✅Random Sampling =================================================================== 
  ## ✅✅Filtering -------------------------------------------------------------------------
  Demo_New = Demo %>% 
    dplyr::filter(EPI___SLICE.BAND.TYPE == BandType) %>%  # BandType
    dplyr::filter(DIAGNOSIS_NEW %in% Diagnosis)
  
  
  
  ## ✅✅Demographics---------------------------------------------------------------------
  Sampled_Demo = Sampling___Group(data = Demo_New, 
                                  group_var = "DIAGNOSIS_NEW", 
                                  group = Diagnosis, 
                                  proportion = proportion, 
                                  sample_size = sample_size, 
                                  replace = TRUE, 
                                  as.combined = TRUE,
                                  seed = seed)
  
  
  
  
  ## ✅✅FC curves-----------------------------------------------------------------------
  RID = paste0("RID_", Sampled_Demo$RID %>% fit_length(4))
  Sampled_FC_Curves = lapply(FC_Curves, function(kth_FC_Curves){
    
    ith_Index = match(RID, colnames(kth_FC_Curves))
    
    kth_FC_Curves_New = kth_FC_Curves[, c(1,ith_Index), drop = FALSE]
    
    # Remove NA row
    kth_FC_Curves_New[complete.cases(kth_FC_Curves_New), ]
    
  }) %>% setNames(names(FC_Curves)) # FC curves for ith Group
  
  
  
  
  
  
  ## ✅✅FC  matrix----------------------------------------------------------------
  Sampled_FC_Matrix.list = NULL
  # Sampled_FC_Matrix.list = lapply(Sampled_Demo.list, function(ith_Group_Demo){
  #   # Extract ith-RID
  #   ith_RID = paste0("RID_", ith_Group_Demo$RID %>% fit_length(4))
  #   
  #   # Random sampling on FC matrices for ith group
  #   ith_Sampled_FC_Matrix = lapply(FC_Matrix, function(kth_FC_Matrix){
  #     
  #     ith_Index = match(ith_RID, colnames(kth_FC_Matrix))
  #     kth_FC_Matrix[, c(1,ith_Index), drop = FALSE]
  #     
  #   }) %>% setNames(names(FC_Matrix)) # FC curves for ith Group
  #   
  #   return(ith_Sampled_FC_Matrix) 
  #   
  # }) %>% setNames(Diagnosis)   # Diagnosis
  
  
  
  # ✅Combine Data =================================================================== 
  Combined_Data = list(Demographics = Sampled_Demo, 
                       FC_Curves = Sampled_FC_Curves,
                       FC_Matrix = Sampled_FC_Matrix.list)
  
  

  
  # ✅Save Those Sampled Data =================================================================== 
  Combined_Data$save_folder_name = Sampling_Combination = paste(Diagnosis, proportion, sep = "_", collapse = "___") %>% 
    paste(paste0(Pipeline, "___", BandType, "___", "Sample-Size_",sample_size), ., "Seed", seed, sep = "___")
  
  # path_save_folder = paste0(path_save, "/", Sampling_Combination)
  # 
  # dir.create(path_save_folder, F, T)
  # 
  # if(list.files(path_save_folder, pattern = "Sampled Data.rds") %>% length == 0){
  # 
  #     saveRDS(object = Combined_Data, file = paste0(path_save_folder, "/Sampled Data.rds"))
  #   
  # }
  
  
  cat("\n", crayon::green("Random sampling with relplacement is done:"), crayon::red(Sampling_Combination),"\n")  
  
  return(Combined_Data)
}



### 🟨 Sampling function for each combination --------------------------------------------------------------------------------------
# Data_Sampling_Combination = function(Demo,
#                                      FC_Curves,
#                                      FC_Matrix,
#                                      Pipeline,
#                                      BandType,
#                                      Diagnosis,
#                                      proportion,
#                                      sample_size,
#                                      seed,
#                                      path_save){
#   for(prop in proportion){
#     for(n in sample_size){
#       Data_Sampling(Demo = Demo,
#                     FC_Curves = FC_Curves,
#                     FC_Matrix = FC_Matrix,
#                     Pipeline = Pipeline,
#                     BandType = BandType,
#                     Diagnosis = Diagnosis,
#                     proportion = prop,
#                     sample_size = n,
#                     seed = seed,
#                     path_save = path_save)
#     }
#   }  
# }









## 🟧 Smoothing using Bspline ====================================================================
Smoothing_by_Bspline = function(Sampled_Data, path_save){
  # ✅ Load path of the sampled data list --------------------------------------------------------
  # Folders = list.files(path_save, full.names = T)
  # path_Sampled_Data_List = sapply(Folders, function(y){
  #   list.files(y, full.names=T, pattern = "Sampled Data.rds")
  # }) %>% unname

  
  
  
  # ✅ Smoothing --------------------------------------------------------
  tictoc::tic()
  # for(i in seq_along(path_Sampled_Data_List)){
  #   # ith save_path 
  #   ith_path_save = Folders[i]
  #   
  #   # ith path
  #   ith_path_Sampled_Data = path_Sampled_Data_List[i]
  #   
  #   # ith Sampled Data
  #   ith_Sampled_Data = readRDS(ith_path_Sampled_Data)
  #   
  #   # Demographics
  #   ith_Demo = ith_Sampled_Data$Demographics
  #   
  #   # FC curves
  #   ith_FC_Curves = ith_Sampled_Data$FC_Curves
  #   
  #   
  #   # Smoothing
  #   ith_Smoothing_Results = lapply(seq_along(ith_FC_Curves), function(k){
  #     
  #     kth_Region = ith_FC_Curves[[k]]
  #     
  #     kth_x = kth_Region[,1]
  #     
  #     FDA___Smoothing(Bspline = list(y = kth_Region[,-1],
  #                                    x = kth_x,
  #                                    range_vals = c(min(kth_x), max(kth_x)),
  #                                    nbasis = NULL,
  #                                    norder = 4,
  #                                    breaks = kth_x,
  #                                    labmdas =  exp(seq(-5, -4, 0.1)),
  #                                    m_int2Lfd = 2,
  #                                    argvals = kth_x), 
  #                     best.criterion = "gcv",
  #                     path_Export = paste0(ith_path_save, "/Smoothed FC Curves using Bspline"), 
  #                     file.name = paste0(fit_length(k, 3), "_", names(ith_FC_Curves)[k]))  
  #     
  #     
  #   }) %>% setNames(names(ith_FC_Curves))
  #   
  #   # Save Smoothed Data
  #   saveRDS(ith_Smoothing_Results, paste0(ith_path_save, "/Smoothed FC Curves using Bspline.rds"))
  # }
  
  FC_Curves = Sampled_Data$FC_Curves
  
  path_Export = paste0(path_save, "/", Sampled_Data$save_folder_name)
  
  Smoothed_Data.list = lapply(seq_along(Sampled_Data$FC_Curves), function(k){
    kth_Region = FC_Curves[[k]]
    
    kth_x = kth_Region[,1]
    
    FDA___Smoothing(Bspline = list(y = kth_Region[,-1],
                                   x = kth_x,
                                   range_vals = c(min(kth_x), max(kth_x)),
                                   nbasis = NULL,
                                   norder = 4,
                                   breaks = kth_x,
                                   labmdas =  exp(seq(-5, -4, 0.1)),
                                   m_int2Lfd = 2,
                                   argvals = kth_x), 
                    best.criterion = "gcv",
                    path_Export = paste0(path_Export , "/Smoothed FC Curves using Bspline"), 
                    file.name = paste0(fit_length(k, 3), "_", names(ith_FC_Curves)[k]),
                    save_rds = F,
                    save_plot = F)    
  }) %>% setNames(names(Sampled_Data$FC_Curves))
  
  tictoc::toc()
  
  cat("\n", crayon::green("Exporting"), crayon::bgRed("Smoothed Data"), crayon::green("is done!"),"\n")
  return(Smoothed_Data.list)
}











## 🟧 Generate True Coef functions =================================================================
Generate_True_Coef_Function = function(Smoothed_Results, num_p_nonzero, path_save){
  #### ✅ Generate functions by Total Number of Functions ============================================================================
  # How many functions to generate
  Total_Num_Functions = tail(num_p_nonzero, 1)
  
  Braion_Regions = names(Smoothed_Results)
  
  Generated_Coef_Functions = list()
  Plot_Titles = list()
  
  for(k in 1:Total_Num_Functions){
    # smoothed results of kth region
    kth_Region_Smoothing = Smoothed_Results[[k]]$smoothing
    
    # Domain
    Domain = kth_Region_Smoothing$argvals %>% as.vector
    
    # Range
    Range = kth_Region_Smoothing$fd$basis$rangeval
    
    # Basis Expansion
    nbasis = k + 3  # nbasis 설정
    if(k %% 2 == 0){
      what_basis = "fourier"
      Basis = create.fourier.basis(rangeval = Range, nbasis = nbasis, period = (2 + k) * pi)
    } else {
      what_basis = "bspline"
      Basis = fda::create.bspline.basis(rangeval = Range, nbasis = nbasis)
    }
    
    
    # Functional Coef : length(coef) = nbassi of Basis
    set.seed(k)
    Generated_Coef_Functions[[k]] = fd_obj = fd(coef = matrix(runif(nbasis), nrow = nbasis), basisobj = Basis)
    
    # Saving plots
    path_Export = paste0(path_save, "/", "Regression Coefficient Functions")
    Plot_Titles[[k]] = plot_title = paste0("Region___",Braion_Regions[k], "___k=", k , "___nbasis=", nbasis, "___Basis=", what_basis)
    
    dir.create(path_Export, F)
    png(filename = paste0(path_Export, "/", sprintf("%02d", k), "th_Coefficient.png"), width = 800, height = 500, bg = "white")
    plot(fd_obj, main = plot_title)
    dev.off()
    cat("\n", crayon::green("Saving"),crayon::bgMagenta("regression coefficient function plots"),crayon::green("is done!"), "\n")
  }
  
  
  # path_Folders = list.files(path_save, full.names=T)
  # 
  # path_Smoothed = sapply(path_Folders, function(y){
  #   list.files(y, pattern = "Smoothed FC Curves using Bspline.rds", full.names = T)
  # }) %>% unname
  # 
  # ith_Smoothed = readRDS(path_Smoothed[1])
  # 
  
  #### ✅ Inner product with all zero fd ===========================================================================
  # # 기저 함수 객체 가져오기
  # basisobj <- mth_Smoothed_Result$smoothing$fd$basis
  # 
  # # 계수를 0으로 하는 벡터 생성
  # coefs_zero <- matrix(0, nrow = basisobj$nbasis, ncol = 1)
  # 
  # # 모든 점에서 0의 값을 갖는 fd 객체 생성
  # fd_zero <- fd(coefs_zero, basisobj)
  # 
  # # fd_zero와 mth_Smoothed_Result$smoothing$fd 사이의 내적 계산
  # inner_product <- inprod(kth_Region_Smoothing, mth_Smoothed_Result$smoothing$fd)
  
  
  names(Generated_Coef_Functions) = unlist(Plot_Titles)
  
  return(Generated_Coef_Functions)
}



## 🟧 Generate True Responses =================================================================
Generate_True_Responses = function(Smoothed_Results, True_Coef_Functions, cutoff = 0.5){
  ## 🟨 Linear Predictor by Inner product ==========================================================================
  Linear_Predictor = list()
  for(i in seq_along(True_Coef_Functions)){
    
    Linear_Predictor[[i]] = fda::inprod(Smoothed_Results[[i]]$smoothing$fd, 
                                        True_Coef_Functions[[i]])
    
  }
  
  Linear_Predictor.df = do.call(cbind, Linear_Predictor)
  

  ## 🟨 probabilities by a sigmoid function ==========================================================================
  p = apply(Linear_Predictor.df, MARGIN=1, function(ith_record){
    E = sum(ith_record) %>% exp()
    E/(1+E)
  })
    
  
  ## 🟨 Decide category by probabilities ==========================================================================  
  Category = ifelse(phat > cutoff, 1, 0)
  
  
  
  Results = data.frame(Category = Category, p = p)
  return(Results)
  
}




  
  
  
## 🟧 FPCA =====================================================================
FPCA = function(Smoothed_Results, path_save){
  
  for(i in seq_along(Smoothed_Results)){
    
    ith_Smoothed_Results = Smoothed_Results[[i]]$smoothing
    
    
    
  }
  FPCA_Train = lapply(seq_along(Smoothing_Train.list), function(k){
    
    kth_Regions = names(kth_Smoothing)
    
    tictoc::tic()
    
    kth_FPCA = lapply(seq_along(kth_Smoothing), function(i){
      
      FDA___fPCA(fdobj = kth_Smoothing[[i]]$smoothing$fd,
                 threshold = 0.9,
                 path_Export = paste0(path_FPCA, "/", Names_Smoothing_Train[k]),
                 file.name = kth_Regions[i])
      
    }) %>% setNames(kth_Regions)
    
    saveRDS(kth_FPCA, paste0(path_FPCA, "/FPCA___", Names_Smoothing_Train[k], ".rds"))
    
    
    tictoc::toc()
    
    
    cat("\n", paste0(crayon::bgRed(basename(Names_Smoothing_Train[k])), crayon::green(" is done!")) ,"\n")
  })

}





# Load data
path_FPCA_Train = list.files(path_FPCA, full.name = T, pattern = "\\.rds$") %>% 
  grep("Scores", ., value = T, invert = T)
Names_FPCA_Train = path_FPCA_Train %>% basename_sans_ext()
FPCA_Train.list = lapply(path_FPCA_Train, readRDS) %>% setNames(Names_FPCA_Train)





## 🟨 Export FPCA socres for Train ==================================================================================
# Extract_fPCA_Scores_with_GroupNums = function(FPCA, path_Export, File.Name){
#   # Export directory
#   fs::dir_create(path_Export, recurse = T)
#   
#   # Group lasso를 위한 numbering
#   FPCA_Scores_GroupNum = c()
#   
#   # Extract FPCA scores
#   FPCA_Scores = lapply(seq_along(FPCA), function(i){
#     
#     ith_Region = FPCA[[i]]
#     
#     ith_PC_Scores = ith_Region$scores %>% as.data.frame
#     
#     names(ith_PC_Scores) = paste0(names(FPCA)[i], "___", 1:ncol(ith_PC_Scores))
#     
#     FPCA_Scores_GroupNum <<- c(FPCA_Scores_GroupNum, rep(i, times = ncol(ith_PC_Scores)))
#     
#     return(ith_PC_Scores)
#   })
#   
#   FPCA_Scores = do.call(cbind, FPCA_Scores)
#   
#   FPCA_Combined = list(fPCA_Scores = FPCA_Scores, Features_Group_Nums = FPCA_Scores_GroupNum)
#   
#   saveRDS(FPCA_Combined, file = paste0(path_Export, "/Scores___", File.Name, "___Train.rds"))
# 
# }
# 
# 
# 
# # Extracting scores from Train
# FPCA_Scores = mapply(Extract_fPCA_Scores_with_GroupNums, 
#                      FPCA = FPCA_Train.list, 
#                      File.Name = Names_Smoothed_Files, 
#                      path_Export = path_FPCA, 
#                      SIMPLIFY = FALSE)








## 🟨 Exporting fPCA scores for Test ==================================================================================
for(i in seq_along(FPCA_Train.list)){
  tictoc::tic()
  ith_FPCA_Train = FPCA_Train.list[[i]]
  ith_Smoothing_Test = Smoothing_Test.list[[i]]
  
  ith_BrainRegion = names(ith_FPCA_Train)
  
  ith_Scores_Group_Num = c()
  
  # Computer inner product for each Brain Region
  ith_Scores_Test = lapply(seq_along(ith_FPCA_Train), function(j){
    # FPCA results from Train  
    ijth_Region_FCPA_Train = ith_FPCA_Train[[j]]
    
    # centering before eigenvalue decomposition
    ijth_Region_Smoothing_FD_Centered_Test = center.fd(ith_Smoothing_Test[[j]]$smoothing$fd) 
    
    ijth_Scores_Test = fda::inprod(fdobj1 = ijth_Region_Smoothing_FD_Centered_Test, 
                                   fdobj2 = ijth_Region_FCPA_Train$harmonics)
    
    colnames(ijth_Scores_Test) = paste0(ith_BrainRegion[j], "___",  1:ncol(ijth_Scores_Test))
    rownames(ijth_Scores_Test) = colnames(ijth_Region_Smoothing_Test$smoothing$y)
    ith_Scores_Group_Num <<- c(ith_Scores_Group_Num, rep(j, times = ncol(ijth_Scores_Test)))
    
    return(ijth_Scores_Test)
  }) %>% setNames(names(ith_FPCA_Train)) %>% do.call(cbind, .) %>% as.data.frame()
  
  
  saveRDS(list(Scores = ith_Scores_Test, Features_Group_Nums = ith_Scores_Group_Num), 
          file = paste0(path_FPCA, "/Scores___", Names_Smoothed_Files[i], "___Test.rds"))
  tictoc::toc()
}


# path_Smooothing = list.files(path_Euclidean, pattern = "Smoothing", full.names=T) %>%
#   list.files(pattern = "FunImgARCWSF___Subjects_ADCN___Train.rds", full.names=T)
# 
# Smoothing_Train = path_Smooothing %>% readRDS
# ACC_Smoothing = Smoothing_Train$ACC_pre_L$smoothing
# 
# 
# 
# path_FPCA_New = list.files(path_FPCA, pattern = "FPCA___FunImgARCWSF___Subjects_ADCN___Train.rds", full.names=T)
# FPCA_new = path_FPCA_New %>% readRDS
# ACC_FPCA = FPCA_new$ACC_pre_L
# 
# 
# center.fd(functionalData)
# Scores = inprod(center.fd(ACC_Smoothing$fd), ACC_FPCA$harmonics) %>% as.data.frame
# cbind(Scores[1,] %>% unlist, ACC_FPCA$scores[1,])








# 🟥 Classification #################################################################







# 🟥 Final Simulation Function #########################################################################
## 🟧 Define the simulation function =====================================================
Simulation  = function(Demo,
                       FC_Curves,
                       FC_Matrix,
                       Pipeline,
                       BandType,
                       Diagnosis,
                       proportion,
                       sample_size,
                       seed,
                       num_p_nonzero,
                       path_save){
  # ✅ Data Sampling --------------------------------------------------------------------
  Sampled_Data = Data_Sampling(Demo,
                               FC_Curves,
                               FC_Matrix,
                               Pipeline,
                               BandType,
                               Diagnosis,
                               proportion,
                               sample_size,
                               seed,
                               cutoff= 0.5, 
                               path_save)
          
  
  
  # ✅ Smoothing --------------------------------------------------------------------
  path_Export = paste0(path_save,"/", Sampled_Data$save_folder_name)
  Smoothed_Results = Smoothing_by_Bspline(Sampled_Data, path_Export)
  
  
  
  # ✅ Generate True Coefficients --------------------------------------------------------------------
  True_Coef_Functions = Generate_True_Coef_Function(Smoothed_Results, num_p_nonzero, path_save)
  
  
  
  # ✅ Generate True response variables --------------------------------------------------------------------
  True_Responses = Generate_True_Responses(Smoothed_Results, True_Coef_Functions, cutoff)
  
  True_Responses$Category %>% table
  
  
  # ✅ Dimension Reduction on FC Matrices --------------------------------------------------------------------
  
  
  
  
  
  
  # ✅ Smoothing by Bspline --------------------------------------------------------------------
  Generate_True_Coef_Function
  
}
  

## 🟧 Setting ======================================================================
# save path
path_save = "/Volumes/Backup_SSD/Backup___Dropbox/@DataAnalysis/✴️DataAnalysis___FDA on RS-fMRI FC Euclidean/6.Simulation"

# Data
Demo = read.csv("/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/㊙️CommonData___ADNI___RS-fMRI___BOLD/RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list_(Selected_Variables).csv")
FC_Curves = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/✴️DataAnalysis___FDA on RS-fMRI FC Euclidean/1.Sorting FC by Euclidean distance/Sorted.FC.by.Dist___FunImgARglobalCWSF.rds")
FC_Matrix = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/㊙️CommonData___ADNI___RS-fMRI___BOLD/Functional Connectivity/FunImgARglobalCWSF/ROI___AAL3___FunImgARglobalCWSF___Static___Pearson___FisherZ___Combined.by.Each.Region/FC_Combined_by_Regions.rds")

# Seed : 100 seed for 100 times repetition for each scenario
Seed_Seq = seq(1,5000, by = 50)[1:100]

# Arguments
Total_proportion = list(c(0.1, 0.9), c(0.2, 0.8), c(0.3, 0.7), c(0.4, 0.6), c(0.5, 0.5))
Total_sample_size = c(100, 500, 1000)
Total_num_p_nonzero = c(5,10,20)

Pipeline = "FunImgARglobalCWSF"
BandType = "SB"
Diagnosis = c("AD", "CN")

proportion = Total_proportion[[1]]
sample_size = Total_sample_size[1]
num_p_nonzero = Total_num_p_nonzero[1]



## 🟧 Repeat Simulation =====================================================
for(prop in proportion){
  for(n in sample_size){
    Data_Sampling(Demo = Demo,
                  FC_Curves = FC_Curves,
                  FC_Matrix = FC_Matrix,
                  Pipeline = Pipeline,
                  BandType = BandType,
                  Diagnosis = Diagnosis,
                  proportion = prop,
                  sample_size = n,
                  seed = seed,
                  path_save = path_save)
  }
}  
Results = sapply(Seed_Seq, function(lth_seed){

  test = Simulation(Demo,
                   FC_Curves,
                   FC_Matrix,
                   Pipeline,
                   BandType, 
                   Diagnosis,
                   proportion,
                   sample_size,
                   seed = lth_seed,
                   num_p_nonzero,
                   path_save)  
  
})





