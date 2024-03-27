# save.image(file = "TMP.RData")
# load("/Users/Ido/Library/CloudStorage/Dropbox/@GitHub/Github___Obsidian/Obsidian/☔️Papers_Writing/㊙️MS Thesis_FC Curves using FDA/♏️⭐️분석 코드/attachments/TMP.RData")
# rm(list=ls())
# 🟥 Load Functions & Packages ##########################################################################
## 🟧Loading my functions ======================================================
# Check my OS/
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
Data_Sampling = function(Demo, 
                         FC_Curves,
                         Pipeline = "FunImgARglobalCWSF",
                         BandType = "SB",
                         Diagnosis = c("AD", "CN"),
                         proportion = 0.1,
                         sample_size = 100,
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










## 🟧 Setting & Loading data =====================================================
path_Data = path_Paper_Data %>% list.files(., pattern = "\\.rds$", full.names=T)
Names_Data = basename_sans_ext(path_Data)

# Loading Data
# If there is no "NA" at the end of each file name, they include NA for Demo variables
Data.list = lapply(path_Data, readRDS) %>% setNames(Names_Data)

# Logistic setting
path_Export = ith_path_Export = paste0(path_Paper_Results, "/", Names_Data[2])






#===============================================================================
# Binomial - NA GroupVar  : Only FPCA
#===============================================================================
Which_NA_RM_Data = intersect(grep("_NA", Names_Data), grep("_Full", Names_Data, invert = TRUE))
path_Export = paste0(path_Paper_Results, "/RmNA___GroupPenalty/FPCA")
Group_Penalty = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP")


Classification___Logistic

for(gth_Penalty in Group_Penalty){
  
  try({
    Resulst =  Classification(Logistic = list(#----------------------------------------
                                              # Data Setting
                                              #----------------------------------------
                                              Train_X = kth_Data$Train_X,
                                              Train_y = kth_Data$Train_y, # factor with levels
                                              Test_X = kth_Data$Test_X,
                                              Test_y = kth_Data$Test_y, # factor with levels
                                              Train_Folds_Index.vec = kth_Data$Folds.vec,
                                              Train_Folds_Index.list = kth_Data$Folds.list,
                                              Standardize = TRUE,
                                              #----------------------------------------
                                              # Modeling Fitting
                                              #----------------------------------------
                                              # Method
                                              Response_Type = "Nominal",
                                              Fitting_Method = gth_Penalty,
                                              Cut_Off = 0.5,
                                              # Model
                                              Family = c("binomial"),
                                              Link = c("logit"),
                                              # Penalty
                                              penalty_alpha = seq(0, 1, 0.01),
                                              penalty_lambda = exp(seq(-2,2,0.01)),
                                              penalty.factor = rep(1, ncol(kth_Data$Train_X)), # which variables no penalty? The corresponding position of 0 is the variables with no penalty
                                              #----------------------------------------
                                              # Tuning measures
                                              #----------------------------------------
                                              Tune_Method = c("cvMisclass"),
                                              # Best_Model_Criterion = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
                                              #                          "cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic"),
                                              #----------------------------------------
                                              # Grouping variables
                                              #----------------------------------------
                                              Grouped_Vars_Index = kth_Data$Train_X_FeaturesGroupsNums, # NULL이 아니면 그룹 정보를 사용, 그룹 위치 벡터를 넣어야 함.
                                              #----------------------------------------
                                              # Plotting
                                              #----------------------------------------
                                              Plot_y_varname = NULL, # proportional logit plot은 하나의 변수만 가능하므로 한 변수 지정
                                              Plot_x_varname = NULL, # 지정하지 않으면 plot 안 그려짐
                                              AUC_in_Legend = TRUE,
                                              #----------------------------------------
                                              # Export Results
                                              #----------------------------------------
                                              path_Export = paste0(path_Export , "/", Names_Data[k], "___", gth_Penalty))
    )}, silent = TRUE  
  )
}


for(k in Which_NA_RM_Data){

  
}
Export_AUC(path_Export)















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
FDA_Simulation  = function(Demo,
                           FC_Curves,
                           FC_Matrix,
                           Pipeline,
                           BandType,
                           Diagnosis,
                           proportion,
                           sample_size,
                           seed,
                           num_p_nonzero,
                           cutoff,
                           Train_K_Folds =  3,
                           path_save){
  # ✅ Data Sampling --------------------------------------------------------------------
  Sampled_Data = Data_Sampling(Demo,
                               FC_Curves,
                               Pipeline,
                               BandType,
                               Diagnosis,
                               proportion,
                               sample_size,
                               seed,
                               path_save)
  
  
  
  
  # ✅ Generate TRUE data -------------------------------------------------------------------
  ## ✅ Setting for FDA-------------------------------------------------------------------
  # List for extracted Domains and FC curves
  Domain_x.list = list()
  FC_Curves.list = list()
  
  
  # Extraction
  for(k in seq_along(Sampled_Data$FC_Curves)){
    Domain_x.list[[k]] = Sampled_Data$FC_Curves[[k]][,1]
    FC_Curves.list[[k]] = Sampled_Data$FC_Curves[[k]][,-1]
  }

  
    
  # Brain regions names
  names(FC_Curves.list) = names(Domain_x.list) = names(Sampled_Data$FC_Curves)
  
  
  # Fold option
  Fold_Arguments = list(Data.df = Sampled_Data$Demographics,
                        Var_1 = "DIAGNOSIS_NEW",
                        Train_K_Folds = Train_K_Folds,
                        Return_Validation = TRUE,
                        seed = seed)
  
  
  # Smoothing Option
  Bspline.list = lapply(Domain_x.list, function(x){
    list(x = x,
         range_vals = c(min(x), max(x)),
         nbasis = NULL,
         norder = 4,
         breaks = x,
         lambdas = exp(seq(-5, -4, 0.1)),
         m_int2Lfd = 2,
         argvals = x,
         best_criterion = "gcv")
  }) %>% setNames(names(FC_Curves.list))
  
  
  
  ## ✅ FDA : Bspline smoothing -------------------------------------------------------------------
  FDA___Simulation___GenerateTrueData = function(Curves.list, Bspline.list){
    # 🟥 Smoothing =======================================================================
    FDA___CV()
    
    
    # 🟥 Smoothing =======================================================================
    
    
  }  

  
  
  # ✅ Smoothing results --------------------------------------------------------------------
  ## ️✴️ True Coefficients  ----------------------------------------------------------
  True_Coef_Functions.list = FDA___Simulation___GenerateTrueCoefFunctions(Smoothed_Results.list = FDA_Results$Train_Result$Smoothed_Train, num_p_nonzero)
  
  
  
  ## ✴️ Generate True response variables --------------------------------------------------------------------
  True_Responses = FDA___Simulation___GenerateTrueResponses(FDA_Results.list$Train_Result$Smoothed_Train, True_Coef_Functions.list, cutoff)
  
  
  
  
  
  
  
  # ✅ Dimension Reduction on FC Matrices --------------------------------------------------------------------
  FDA_Results.list = FDA___CV(Demographics.df = Sampled_Data$Demographics,
                              Curves.list = FC_Curves.list,
                              Fold_Arguments = Fold_Arguments,
                              Bspline.list = Bspline.list,
                              FPCA.list = list(threshold = 0.9),
                              path_save = NULL)
  # random noise
  # true covariates + noise -> obs -> smoothing
  true 시그널 관련 논문 ㅊ자기
  # scenario 1
  # true covariates + noise -> obs
  
  # scenario 2
  # raw covariates + noise -> obs -> smoothing
  
  
  
  
  # ✅ --------------------------------------------------------------------
  Generate_True_Coef_Function
  
}
  

## 🟧 Setting ======================================================================
# save path
path_save = "/Volumes/Backup_SSD/Backup___Dropbox/@DataAnalysis/✴️DataAnalysis___FDA on RS-fMRI FC Euclidean/6.Simulation"

# Data
Demo = read.csv("/Volumes/Backup_SSD/Backup___Dropbox/@DataAnalysis/㊙️CommonData___ADNI___RS-fMRI___BOLD/RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list_(Selected_Variables).csv")
FC_Curves = readRDS("/Volumes/Backup_SSD/Backup___Dropbox/@DataAnalysis/✴️DataAnalysis___FDA on RS-fMRI FC Euclidean/1.Sorting FC by Euclidean distance/Sorted.FC.by.Dist___FunImgARglobalCWSF.rds")
# FC_Matrix = readRDS("/Users/Ido/Library/CloudStorage/Dropbox/@DataAnalysis/㊙️CommonData___ADNI___RS-fMRI___BOLD/Functional Connectivity/FunImgARglobalCWSF/ROI___AAL3___FunImgARglobalCWSF___Static___Pearson___FisherZ___Combined.by.Each.Region/FC_Combined_by_Regions.rds")

# Seed : 100 seed for 100 times repetition for each scenario
Seed_Seq = seq(1,5000, by = 50)[1:100]
# seed = Seed_Seq[1]

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
cutoff = 0.5


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





