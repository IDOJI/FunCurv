# 🟥 Load Functions & Packages ##########################################################################
## 🟨Loading my functions ======================================================
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





## 🟨Install and loading Packages ================================
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

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)
install_github("regbook/regbook")
library(regbook)






# 🟥 Subjects Lists ##########################################################################
## 🟨 Path ==================================================================================
path_Euclidean = list.files(path_FD, pattern = "Euclidean", full.names = T)
path_Euclidean_SortedFC = list.files(path_Euclidean, pattern = "Sorted", full.names = T) %>% 
  list.files(., full.names = TRUE, pattern = "\\.rds$") %>% 
  grep("___FunImgAR", ., value = TRUE)

path_Save = paste0(path_Euclidean, "/Subjects_List_Splitted")





## 🟨 Extract RID from FC data ==================================================================================
RID_FC = readRDS(path_Euclidean_SortedFC[1])[[1]] %>% colnames






## 🟨 Subjects ==================================================================================
Subjects = read.csv(path_Subjects)

# Intersection with FC
Subjects_Full = Subjects %>% 
  dplyr::filter(NFQ___BAND.TYPE == "SB") %>% 
  dplyr::select(RID,
                DEMO___DIAGNOSIS_NEW,
                DEMO___SEX, 
                DEMO___AGE,
                DEMO___ADNIMERGE___APOE4, 
                DEMO___ADNIMERGE___PTEDUCAT,
                DEMO___MMSE___MMSCORE,
                PTDEMO___PTHAND
                # DEMO___ADNIMERGE___PTMARRY
                # NFQ___BAND.TYPE,
                # VISCODE2
                ) %>% 
  dplyr::mutate(DEMO___DIAGNOSIS_NEW = case_when(
    DEMO___DIAGNOSIS_NEW %in% c("AD(Possible)", "AD(Probable)", "Dementia") ~ "AD",
    DEMO___DIAGNOSIS_NEW %in% c("LMCI", "EMCI") ~ "MCI",
    TRUE ~ DEMO___DIAGNOSIS_NEW
  )) %>% 
  # dplyr::mutate(DEMO___ADNIMERGE___PTMARRY = factor(DEMO___ADNIMERGE___PTMARRY)) %>% 
  dplyr::mutate(DEMO___SEX = case_when(
    DEMO___SEX == "Male" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(PTDEMO___PTHAND = case_when(
    PTDEMO___PTHAND == "Right" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::arrange(RID) %>% 
  dplyr::mutate(RID = paste0("RID_", sprintf("%04d", RID))) %>% 
  dplyr::filter(RID %in% RID_FC) %>% 
  dplyr::mutate(DEMO___DIAGNOSIS_NEW = factor(DEMO___DIAGNOSIS_NEW, levels = c("CN", "MCI", "AD")))





## 🟨 Subset by Disease Group ==================================================================================
Subjects_List.list = list()
# AD MCI CN
Subjects_List.list[[1]] = Subjects_Full = Subjects_Full
Subjects_List.list[[2]] = Subjects_NonNA = Subjects_Full %>% na.omit
# AD MCI
Subjects_List.list[[3]] = Subjects_AD.MCI_Full = Subjects_Full %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "MCI"))
Subjects_List.list[[4]] = Subjects_AD.MCI_NonNA = Subjects_AD.MCI_Full %>% na.omit
# AD CN
Subjects_List.list[[5]] = Subjects_AD.CN_Full = Subjects_Full %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("AD", "CN"))
Subjects_List.list[[6]] = Subjects_AD.CN_NonNA = Subjects_AD.CN_Full %>% na.omit
# MCI CN
Subjects_List.list[[7]] = Subjects_MCI.CN_Full = Subjects_Full %>% dplyr::filter(DEMO___DIAGNOSIS_NEW %in% c("MCI", "CN"))
Subjects_List.list[[8]] = Subjects_MCI.CN_NonNA = Subjects_MCI.CN_Full %>% na.omit

names(Subjects_List.list) = c("Subjects_Full", "Subjects_Full_NA",
                              "Subjects_ADMCI", "Subjects_ADMCI_NA",
                              "Subjects_ADCN", "Subjects_ADCN_NA",
                              "Subjects_MCICN", "Subjects_MCICN_NA")






## 🟨 Splitting by "Diagnosis" maintaining proportion ==================================================================================
fs::dir_create(path_Save, recurse = T)

for(i in 1:length(Subjects_List.list)){
  
  ith_Splitted_Data = SUB___Fold(Data = Subjects_List.list[[i]], 
                                 Var_1 = "DEMO___DIAGNOSIS_NEW", 
                                 y_Var = "DEMO___DIAGNOSIS_NEW")
  
  saveRDS(ith_Splitted_Data, paste0(path_Save, "/", names(Subjects_List.list)[i], ".rds"))
  
}







# 🟥 2.Smoothing by B spline & Split the data into Train and Test #############################################
## 🟨 Path  ===============================================================================
path_Subjects = list.files(path_Euclidean, pattern = "Subjects", full.names = TRUE) %>% list.files(., full.names=TRUE)
Names_Subjects = path_Subjects %>% basename_sans_ext


path_SortedFC = path_Euclidean %>% 
  list.files(., pattern = "Sorted", full.names = T) %>% 
  list.files(., pattern = "FunImgAR", full.names = T) %>% 
  grep("\\.rds$", ., value = T)
Names_SortedFC = path_SortedFC %>% basename_sans_ext()


path_Smoothing = list.files(path_Euclidean, pattern = "Smoothing", full.names = T)



## 🟨 Load Subjects List  ===============================================================================
Subjects_List = lapply(path_Subjects, readRDS) %>% setNames(Names_Subjects)




## 🟨 Load Subjects List  ===============================================================================
SortedFC.list = lapply(path_SortedFC, readRDS) %>% setNames(Names_SortedFC)





## 🟨 Select FC for each Subject List  ===============================================================================
Pipelines = c("FunImgARCWSF", "FunImgARglobalCWSF")
for(i in seq_along(SortedFC.list)){
  
  ith_SortedFC.list = SortedFC.list[[i]]
  
  ith_Name_SortedFC = Names_SortedFC[i]
  
  ith_Pipeline = Pipelines[i]
  
  
  for(j in seq_along(Subjects_List)){
    
    jth_Subject_List_Name = Names_Subjects[j]
    
    jth_Subject_List = Subjects_List[[j]]
    
    jth_RID_Train = jth_Subject_List$Train_X$RID
    
    jth_RID_Test = jth_Subject_List$Test_X$RID
    
    
    for(k in seq_along(ith_SortedFC.list)){
      
      kth_Brain_Name = names(ith_SortedFC.list)[k]
      
      kth_Brain = ith_SortedFC.list[[k]]
      kth_Brain = kth_Brain[complete.cases(kth_Brain),]
      
      
      kth_x = kth_Brain[,1]
      
      kth_y = kth_Brain[,-1]
      
      kth_y_Train = kth_y[, colnames(kth_y) %in% jth_RID_Train]
      kth_y_Test = kth_y[, colnames(kth_y) %in% jth_RID_Test]
      kth_y_Combined = list(Train = kth_y_Train, Test = kth_y_Test)
      
      # Smoothing : Train
      Results = lapply(seq_along(kth_y_Combined), function(n){
        FDA___Smoothing(Bspline = list(y = kth_y_Combined[[n]],
                                       x = kth_x,
                                       range_vals = c(min(kth_x), max(kth_x)),
                                       nbasis = NULL,
                                       norder = 4,
                                       breaks = kth_x,
                                       labmdas =  exp(seq(-5, -4, 0.1)),
                                       m_int2Lfd = 2,
                                       argvals = kth_x), 
                        path_Export = paste0(path_Smoothing, "/", ith_Pipeline, "___", jth_Subject_List_Name), 
                        file.name = paste0(names(kth_y_Combined)[n], "___", kth_Brain_Name))  
      })
    }
  }
}







## 🟨 Combining Results  ===============================================================================
path_Smoothed_Folders = list.files(path_Smoothing, full.names = T)
Names_Smoothed_Folders = path_Smoothed_Folders %>% basename_sans_ext()

for(i in seq_along(path_Smoothed_Folders)){
  tictoc::tic()
  pattern.vec = c("Train", "Test")
  
  Results = sapply(pattern.vec, function(kth_pattern){
    
    
    ith_Files = list.files(path_Smoothed_Folders[i], full.names = T, pattern = kth_pattern) %>% 
      grep("\\.rds$", ., value = T)
    
    ith_Files_Names = ith_Files %>% 
      basename_sans_ext() %>% 
      gsub(pattern = "Train___", replacement = "", x = .)
    
    ith_Combined = lapply(ith_Files, readRDS) %>% setNames(ith_Files_Names)
    
    saveRDS(ith_Combined, file = paste0(path_Smoothing, "/", Names_Smoothed_Folders[i], "___", kth_pattern, ".rds"))
    
  })
  tictoc::toc()
}











# 🟥 3.FPCA ####################################################################################################
## 🟨 path ==================================================================================
path_Smoothed = list.files(path_Euclidean, full.names = T, pattern = "Smoothing") %>% 
  list.files(., full.names = T)

# Subjects List Names
Names_Subjects = path_Smoothed %>% grep("\\.rds$", ., value = T, invert = T) %>% basename_sans_ext()

# Smoothed files
path_Smoothed_Files = path_Smoothed %>% grep("\\.rds$", ., value = T)
path_Smoothed_Files_Train = path_Smoothed_Files %>% grep("Train", ., value = T)
path_Smoothed_Files_Test = path_Smoothed_Files %>% grep("Test", ., value = T)
Names_Smoothed_Files = path_Smoothed_Files %>% 
  basename_sans_ext() %>% 
  grep("Train", ., value = T) %>% 
  gsub("___Train", "", .)


# Save
path_FPCA = list.files(path_Euclidean, pattern = "FPCA", full.names=T)






## 🟨 Loading smoothed data ==================================================================================
Smoothing_Train.list = lapply(path_Smoothed_Files_Train, readRDS) %>% setNames(basename_sans_ext(path_Smoothed_Files_Train))
Smoothing_Test.list = lapply(path_Smoothed_Files_Test, readRDS) %>% setNames(basename_sans_ext(path_Smoothed_Files_Test))




## 🟨 FPCA for Train data ==================================================================================
Names_Smoothing_Train = names(Smoothing_Train.list)

# (@Completed) compute FPCA 
# FPCA_Train = lapply(seq_along(Smoothing_Train.list), function(k){
# 
#   kth_Smoothing = Smoothing_Train.list[[k]]
# 
#   kth_Regions = names(kth_Smoothing)
# 
#   tictoc::tic()
# 
#   kth_FPCA = lapply(seq_along(kth_Smoothing), function(i){
# 
#     FDA___fPCA(fdobj = kth_Smoothing[[i]]$smoothing$fd,
#                threshold = 0.9,
#                path_Export = paste0(path_FPCA, "/", Names_Smoothing_Train[k]),
#                file.name = kth_Regions[i])
# 
#   }) %>% setNames(kth_Regions)
# 
#   saveRDS(kth_FPCA, paste0(path_FPCA, "/FPCA___", Names_Smoothing_Train[k], ".rds"))
# 
# 
#   tictoc::toc()
# 
# 
#   cat("\n", paste0(crayon::bgRed(basename(Names_Smoothing_Train[k])), crayon::green(" is done!")) ,"\n")
# })


# Load data
path_FPCA_Train = list.files(path_FPCA, full.name = T, pattern = "\\.rds$") %>% 
  grep("Scores", ., value = T, invert = T)
Names_FPCA_Train = path_FPCA_Train %>% basename_sans_ext()
FPCA_Train.list = lapply(path_FPCA_Train, readRDS) %>% setNames(Names_FPCA_Train)





## 🟨 Export FPCA socres for Train ==================================================================================
Extract_fPCA_Scores_with_GroupNums = function(FPCA, path_Export, File.Name){
  # Export directory
  fs::dir_create(path_Export, recurse = T)

  # Group lasso를 위한 numbering
  FPCA_Scores_GroupNum = c()

  # Extract FPCA scores
  FPCA_Scores = lapply(seq_along(FPCA), function(i){

    ith_Region = FPCA[[i]]

    ith_PC_Scores = ith_Region$scores %>% as.data.frame

    names(ith_PC_Scores) = paste0(names(FPCA)[i], "___", 1:ncol(ith_PC_Scores))

    FPCA_Scores_GroupNum <<- c(FPCA_Scores_GroupNum, rep(i, times = ncol(ith_PC_Scores)))

    return(ith_PC_Scores)
  })

  FPCA_Scores = do.call(cbind, FPCA_Scores)

  FPCA_Combined = list(fPCA_Scores = FPCA_Scores, Features_Group_Nums = FPCA_Scores_GroupNum)

  saveRDS(FPCA_Combined, file = paste0(path_Export, "/Scores___", File.Name, "___Train.rds"))

}

# 
# 
# Extracting scores from Train
FPCA_Scores = mapply(Extract_fPCA_Scores_with_GroupNums,
                     FPCA = FPCA_Train.list,
                     File.Name = Names_Smoothed_Files,
                     path_Export = path_FPCA,
                     SIMPLIFY = FALSE)








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


































