###############################################################################
# 1.1.Loading packages
###############################################################################
# which_OS = Mac = "/Users/Ido/"
# which_OS = Windows = "C:/Users/lleii/"
require(dplyr)
require(tidyverse)
require(fda)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)






#===============================================================================
# 1.Loading Subjects List
#===============================================================================
Subjects.df = read.csv("C:/Users/lleii/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv")






#===============================================================================
# 2.Extracting Lists by Manufacturer
#===============================================================================
Manufacturer = list(NULL, "GE.MEDICAL.SYSTEMS", "SIEMENS", "Philips")
Extracted_Subjects.list = lapply(Manufacturer, function(y, ...){
  if(is.null(y)){
    Subjects.df %>% filter(NFQ___BAND.TYPE == "SB") %>% return()
  }else{
    Subjects.df %>% filter(MANUFACTURER_NEW == paste0(y, "_SB")) %>% return()
  }
})
names(Extracted_Subjects.list) = Manufacturer
names(Extracted_Subjects.list)[1] = "SB"








#===============================================================================
# Exporting subjects lists
#===============================================================================
path_Export = "C:/Users/lleii/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/0.Subjects Lists"
Data.list = lapply(seq_along(Extracted_Subjects.list), function(k, ...){
  kth_name = names(Extracted_Subjects.list)[k]
  kth_Subjects = Extracted_Subjects.list[[k]]
  saveRDS(kth_Subjects, paste0(path_Export, "/Subjects_List___", kth_name, ".rds"))
  return(kth_Subjects)
})
























