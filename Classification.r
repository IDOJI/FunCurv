# 🟥 NonGroupPenalty ################################################################










#===============================================================================
# Path
#===============================================================================
path_Data = path_Paper_Data %>% list.files(., pattern = "\\.rds$", full.names=T)
Names_Data = basename_sans_ext(path_Data)






#===============================================================================
# Loading Data
#===============================================================================
# If there is no "NA" at the end of each file name, they include NA for Demo variables
Data.list = lapply(path_Data, readRDS) %>% setNames(Names_Data)











#===============================================================================
# Define a Logistic-setting function
#===============================================================================
path_Export = ith_path_Export = paste0(path_Paper_Results, "/", Names_Data[2])
# Binomial
Logistic_Setting_1 = function(ith_Data, 
                              path_Export, 
                              Only_Demo = F,
                              Exclude_Demo = T,
                              Fitting_Method = "MLE", 
                              Family = c("binomial"), 
                              Link = "logit",
                              Grouped_Vars_Index = NULL,
                              penalty_factor = NULL,
                              penalty_lambda = exp(seq(-50,50,1))){
  
  
  ith_Data$Train_X = ith_Data$Train_X %>% dplyr::select(-RID) %>% dplyr::select(-ends_with("MMSCORE"))
  ith_Data$Test_X = ith_Data$Test_X %>% dplyr::select(-RID) %>% dplyr::select(-ends_with("MMSCORE"))
  
  if(Only_Demo){
    ith_Data$Train_X = ith_Data$Train_X %>% dplyr::select(contains("DEMO"))
    ith_Data$Test_X = ith_Data$Test_X %>% dplyr::select(contains("DEMO"))
  }
  if(Exclude_Demo){
    ith_Data$Train_X = ith_Data$Train_X %>% dplyr::select(-contains("DEMO"))
    ith_Data$Test_X = ith_Data$Test_X %>% dplyr::select(-contains("DEMO"))
  }
  
  
  
  
   Logistic = list(#----------------------------------------
                  # Data Setting
                  #----------------------------------------
                  Train_X = ith_Data$Train_X,
                  Train_y = ith_Data$Train_y,
                  Test_X = ith_Data$Test_X,
                  Test_y = ith_Data$Test_y,
                  Train_Folds_Index.vec = ith_Data$Folds.vec,
                  Train_Folds_Index.list = ith_Data$Folds.list,
                  Standardize = TRUE,
                  #----------------------------------------
                  # Modeling Fitting
                  #----------------------------------------
                  # Method
                  Response_Type = "Ordinal",
                  Fitting_Method = Fitting_Method, #
                  # Model
                  Family = Family,
                  Link = Link,
                  # Penalty
                  penalty_alpha = seq(0, 1, 0.1),
                  penalty_lambda = penalty_lambda,
                  penalty_factor = penalty_factor,
                  #----------------------------------------
                  # Tuning measures
                  #----------------------------------------
                  Tune_Method = c("cvMisclass"),
                  # Best_Model_Criterion = c(#Classification___Logistic___Ordinal___Elastic___NonGroupedPenalty
                  #                          "cvLoglik", "cvMisclass", "cvBrier", "cvDevPct", "aic", "bic"),
                  #----------------------------------------
                  # Grouping variables
                  #----------------------------------------
                  Grouped_Vars_Index = Grouped_Vars_Index, # NULL이 아니면 그룹 정보를 사용, 그룹 위치 벡터를 넣어야 함.
                  #----------------------------------------
                  # Plotting
                  #----------------------------------------
                  Plot_y_varname = NULL, # proportional logit plot은 하나의 변수만 가능하므로 한 변수 지정
                  Plot_x_varname = NULL, # 지정하지 않으면 plot 안 그려짐
                  AUC_in_Legend = TRUE,
                  #----------------------------------------
                  # Export Results
                  #----------------------------------------
                  path_Export = path_Export)  
  
  return(Logistic)
}










#===============================================================================
# Binomial - NA-removed
#===============================================================================
Which_NA_RM_Data = intersect(grep("_NA", Names_Data), grep("_Full", Names_Data, invert = TRUE))
path_Paper_Results_New = paste0(path_Paper_Results, "/RmNA___NonGroupPenalty")
#--------------------
# @ Only Demographics
#--------------------
# path_Export = paste0(path_Paper_Results_New, "/Demographics")
# 
# 
# 
# for(k in Which_NA_RM_Data){
#   Resulst =  Classification(Logistic = Logistic_Setting_1(ith_Data = Data.list[[k]], 
#                                                           path_Export = paste0(path_Export, "/", Names_Data[k]),
#                                                           Family = "binomial", 
#                                                           Only_Demo = T))
# }
#--------------------
# @ Only Demo + FPCA
#--------------------
# path_Paper_Results_New = paste0(path_Paper_Results, "/Demographics+FPCA")
# for(k in Which_NA_RM_Data){
#   
#   N_Demo = Data.list[[k]]$Train_X %>% names %>% grep("DEMO", ., value=T) %>% length - 1 # remove MMSE
#   
#   Resulst =  Classification(Logistic = Logistic_Setting_1(ith_Data = Data.list[[k]], 
#                                                           path_Export = paste0(path_Export, "/", Names_Data[k]),
#                                                           Family = "binomial",
#                                                           penalty_factor = c(rep(0, N_Demo), rep(1, ncol(Data.list[[k]]$Train_X) - N_Demo - 2) # RID, MMSE 제외
#                                                                              ),
#                                                           Fitting_Method = "ElasticNet",
#                                                           Only_Demo = F))
# }
#--------------------
#  @ Only FPCA
#--------------------
# path_Paper_Results_New = paste0(path_Paper_Results, "/FPCA")
# for(k in Which_NA_RM_Data){
# 
#   N_Demo = Data.list[[k]]$Train_X %>% names %>% grep("DEMO", ., value=T) %>% length - 1 # remove MMSE
# 
#   Resulst =  Classification(Logistic = Logistic_Setting_1(ith_Data = Data.list[[k]],
#                                                           path_Export = paste0(path_Export, "/", Names_Data[k]),
#                                                           Family = "binomial",
#                                                           Fitting_Method = "ElasticNet", 
#                                                           Exclude_Demo = T,
#                                                           Only_Demo = F))
# }








#===============================================================================
# Binomial - Full with NA
#===============================================================================
Index = intersect(grep("_NA", Names_Data, invert = TRUE), grep("_Full", Names_Data, invert = TRUE))
path_Paper_Results_New = paste0(path_Paper_Results, "/WithNA___NonGroupPenalty")
#--------------------
# FPCA
#--------------------
for(k in Index){
  
  print(k)
  
  Resulst =  Classification(Logistic = Logistic_Setting_1(ith_Data = Data.list[[k]],
                                                          path_Export = paste0(path_Paper_Results_New, "/FPCA/", Names_Data[k]),
                                                          Family = "binomial",
                                                          Fitting_Method = "ElasticNet",
                                                          Exclude_Demo = T))
}








# 🟥 GroupPenalty ################################################################
##############################################################################################
# 0. Loading functions
##############################################################################################
# rm(list=ls())
#=============================================================================================
# Mac
#=============================================================================================
# path_OS = "/Users/Ido/"
#============================================================================================
# Windows
#============================================================================================
# path_OS = "C:/Users/lleii/"
#============================================================================================
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
install_packages(c("tidyverse", "dplyr", "clipr", "fda", "tidyr", "stringr", "readr"))
#=============================================================================================
path_Dropbox = paste0(path_OS, "Dropbox")
path_GitHub = list.files(path_Dropbox, pattern = "Github", full.names = T)
path_Rpkgs = list.files(path_GitHub, pattern = "Rpkgs", full.names = T)
Rpkgs = c("ADNIprep", "StatsR", "refineR")
Load = sapply(Rpkgs, function(y){
  list.files(paste0(path_Rpkgs, "/", y, "/R"), full.names = T) %>% walk(source) 
})
#============================================================================================= Data
path_Data = paste0(path_Dropbox, "/Data")
#---------------
# ADNI
#---------------
path_ADNI = list.files(path_Data, full.names = T, pattern = "ADNI")
path_Subjects = list.files(path_ADNI, full.names = TRUE, pattern = "Subjects.Lists") %>% 
  list.files(., full.names = TRUE) %>%
  grep("Subjects_Lists_Exported$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("Final$", ., value = TRUE) %>% 
  list.files(., full.names = TRUE) %>% 
  grep("list.csv$", ., value  =TRUE)
# FDA
path_FD = list.files(path_ADNI, full.names = T, pattern = "Functional.Data")
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names=TRUE)
path_FPCA = list.files(path_Euclidean, pattern = "FPCA", full.names=TRUE)
path_Combined = path_Euclidean %>% list.files(., pattern = "Combined", full.names = T)
#============================================================================================= Papers
path_Papers = path_Data %>% list.files(pattern = "Papers", full.names = T)
path_Paper = path_Papers %>% list.files(pattern = "FDA on RS-fMRI FC Euclidean",full.names=T)
path_Paper_Data = path_Paper %>% list.files(pattern = "Data", full.names = T)
path_Paper_Results = path_Paper %>% list.files(pattern = "Results", full.names = T)
#=============================================================================================











#===============================================================================
# Path
#===============================================================================
path_Data = path_Paper_Data %>% list.files(., pattern = "\\.rds$", full.names=T)
Names_Data = basename_sans_ext(path_Data)






#===============================================================================
# Loading Data
#===============================================================================
# If there is no "NA" at the end of each file name, they include NA for Demo variables
Data.list = lapply(path_Data, readRDS) %>% setNames(Names_Data)











#===============================================================================
# Define a Logistic-setting function
#===============================================================================
path_Export = ith_path_Export = paste0(path_Paper_Results, "/", Names_Data[2])










#===============================================================================
# Exporting AUC
#===============================================================================
Export_AUC = function(root_dir){
  library(readr)
  # Initialize a data frame to store folder names and AUC values
  results <- data.frame(Folder_Name = character(), AUC_Value = character(), stringsAsFactors = FALSE)
  
  # List all folders in the root directory
  folders <- list.dirs(path = root_dir, full.names = FALSE, recursive = FALSE)
  
  # Iterate over each folder
  for (folder in folders) {
    
    folder_path <- file.path(root_dir, folder)
    # List CSV files that contain "AUC" in their names within the folder
    auc_files <- list.files(path = folder_path, pattern = "AUC.*\\.csv$", full.names = TRUE)
    
    # Check if there is at least one AUC file
    if (length(auc_files) > 0) {
      # Try to read the first AUC file found
      tryCatch({
        auc_data <- read_csv(auc_files[1], col_types = cols(.default = col_character()))
        auc_value <- auc_data %>% unlist
      }, error = function(e) {
        auc_value <- "Error reading file"
      })
    } else {
      # If no AUC file is found, set the value to "No Converge"
      auc_value <- "No Converge"
    }
    
    # Add the folder name and AUC value to the results data frame
    results <- rbind(results, data.frame(Folder_Name = folder, AUC_Value = auc_value))
  }
  # Write the results to a CSV file
  write_csv(results, path = file.path(root_dir, "AUC_Results.csv"))
  
  
}















#===============================================================================
# Binomial - NA GroupVar  : Only FPCA
#===============================================================================
Which_NA_RM_Data = intersect(grep("_NA", Names_Data), grep("_Full", Names_Data, invert = TRUE))
path_Export = paste0(path_Paper_Results, "/RmNA___GroupPenalty/FPCA")
Group_Penalty = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP")
#--------------------
# @ Only FPCA
#--------------------
for(k in Which_NA_RM_Data){
  
  kth_Data = Data.list[[k]]
  
  #------------------
  # Basic Setting
  #------------------
  kth_Data$Train_X = kth_Data$Train_X %>% 
    dplyr::select(-contains("DEMO__")) %>% 
    dplyr::select(-contains("MMSC")) %>% 
    dplyr::select(-RID)
  kth_Data$Test_X = kth_Data$Test_X %>% 
    dplyr::select(-contains("DEMO__")) %>% 
    dplyr::select(-contains("MMSC")) %>% 
    dplyr::select(-RID)
  
  
  # Loop for each penalty
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
}
Export_AUC(path_Export)















#===============================================================================
# Binomial - NA GroupVar  : Demo + FPCA
#===============================================================================
Which_NA_RM_Data = intersect(grep("_NA", Names_Data), grep("_Full", Names_Data, invert = TRUE))
path_Export = paste0(path_Paper_Results, "/RmNA___GroupPenalty/Demo+FPCA")
Group_Penalty = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP")
#--------------------
# @ Only FPCA
#--------------------
for(k in Which_NA_RM_Data){
  
  kth_Data = Data.list[[k]]
  
  #------------------
  # Basic Setting
  #------------------
  kth_Data$Train_X = kth_Data$Train_X %>% 
    # dplyr::select(-contains("DEMO__")) %>% 
    dplyr::select(-contains("MMSC")) %>% 
    dplyr::select(-RID)
  kth_Data$Test_X = kth_Data$Test_X %>% 
    # dplyr::select(-contains("DEMO__")) %>% 
    dplyr::select(-contains("MMSC")) %>% 
    dplyr::select(-RID)
  n_Demo = kth_Data$Train_X %>% dplyr::select(contains("DEMO_")) %>% ncol
  kth_Data$Train_X_FeaturesGroupsNums = c(rep(0, n_Demo), kth_Data$Train_X_FeaturesGroupsNums)
  
  
  
  
  # Loop for each penalty
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
}
Export_AUC(path_Export)

















