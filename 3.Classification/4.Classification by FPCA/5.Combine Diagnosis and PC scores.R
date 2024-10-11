###############################################################################
# 1.1.Loading packages
###############################################################################
# which_OS = Mac = "/Users/Ido/"
which_OS = Windows = "C:/Users/lleii/"
require(dplyr)
require(tidyverse)
require(fda)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)









#===============================================================================
# Loading Subjects Lists 
#===============================================================================
# Windows
path_Subjects = "C:/Users/lleii/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/0.Subjects Lists"
# Mac
path_Subjects = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/0.Subjects Lists"
path_Files = list.files(path_Subjects, full.names=T)
Files_Names = list.files(path_Subjects, full.names=F)
Subjects_Lists = lapply(path_Files, readRDS)
names(Subjects_Lists) = Files_Names

# Extract response variables
Diagnosis.list = lapply(Subjects_Lists, function(y){
  y = y$DEMO___DIAGNOSIS_NEW
  # 1)  AD -> Dementia
  y = gsub("AD\\(Probable\\)|AD\\(Possible\\)|AD", "Dementia", y)
  
  # 2) LMCI, EMCI -> MCI
  y = gsub("LMCI|EMCI", "MCI", y)
  
  # Factorize
  y = factor(y, levels = c("CN", "MCI", "Dementia"))
  return(y)
})
names(Diagnosis.list) = Files_Names








#===============================================================================
# Loading PC scores (multivariate) Data
#===============================================================================
# Windows
path_Scores = "D:/Data___Backup/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/4.FPCA/4.PC Scores"
# Mac
path_Scores = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/4.PC Scores___0.9"
Folders = list.files(path_Scores)
path_Folders = list.files(path_Scores, full.names=T)

PC_Scores.list = lapply(path_Folders, function(y){
  path_RDS = list.files(y, full.names=T)
  readRDS(path_RDS)
})
names(PC_Scores.list) = Folders










#===============================================================================
# Combining Diagnosis  & PC Scores
#===============================================================================
# Manufacturers
Manufacturers = sapply(Folders, function(y){
  y_New = str_split(y, "___")[[1]]
  return(paste0("___", tail(y_New, 1)))
}) %>% unname


#=========
# Combine
#=========
# Windows
path_Export = "D:/Data___Backup/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/4.FPCA/5.PC Scores with Diagnosis"
# Mac
path_Export = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/5.PC Scores with Diagnosis___0.9"
Combined.list = lapply(seq_along(PC_Scores.list), function(i,...){
  ith_Name = names(PC_Scores.list)[i]
  ith_Diagnosis = Diagnosis.list[[grep(pattern = Manufacturers[i], x = names(Diagnosis.list))]]
  ith_Combined = c(list(Diagnosis = ith_Diagnosis), PC_Scores.list[[i]])
  saveRDS(ith_Combined, file = paste0(path_Export, "/", ith_Name, ".rds"))
  return(ith_Combined)
})



















