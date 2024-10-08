###############################################################################
# 1.1.Loading packages
###############################################################################
# which_OS = Mac = "/Users/Ido/Library/CloudStorage"
# which_OS = Windows = "C:/Users/lleii/"
require(dplyr)
require(tidyverse)
require(fda)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/refineR/R"), full.names = T) %>% walk(source)
list.files(paste0(which_OS, "/Dropbox/Github/Rpkgs/StatsR/R"), full.names = T) %>% walk(source)





#===============================================================================
# 1.The first FPCA
#===============================================================================
path_Export = paste0(which_OS, "Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/4.FPCA/1.The first FPCA")
path_Smoothed.Data = paste0(which_OS, "/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/3.Smoothing.by.Bspline___BOLD.Signals")
path_Smooothed.Data_Folders = list.files(path_Smoothed.Data, full.names=T)
path_Smoothed_Data_List = sapply(path_Smooothed.Data_Folders, function(y){list.files(y, full.names=T, pattern = "FDA___Smoothing___B.Spline")}) %>% unname
Folders_Smoothed.Data = list.files(path_Smoothed.Data)
FPCA.list = lapply(seq_along(path_Smoothed_Data_List), function(i, ...){
  ith_Data = readRDS(path_Smoothed_Data_List[[i]])
  tictoc::tic()
  ith_Data_FPCA = lapply(ith_Data, function(kth_ROI){
    pca.fd(kth_ROI$fdSmooth$fd, nharm = 50)
  })
  tictoc::toc()
  ith_Export_path = paste0(path_Export, "/", Folders_Smoothed.Data[i])
  dir.create(ith_Export_path, showWarnings = F)
  saveRDS(object = ith_Data_FPCA, paste0(ith_Export_path, "/FPCA.rds"))
  return(ith_Data_FPCA)
})





#===============================================================================
# 2.Select nharm
#===============================================================================
path_FPCA = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/1.The first FPCA"
path_Export = paste0(which_OS, "/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/2.Select nharm___0.9")
threshold = 0.9

# Loading FPCA results
FPCA.list = lapply(list.files(path_FPCA, full.names=T), function(x){
  list.files(x, full.names=T) %>% readRDS
})


# Select N_harm by threshold
n_harm.list = lapply(seq_along(FPCA.list), function(k, ...){
  ROI = paste0("ROI_", fit_length(1:length(FPCA.list[[k]]), 3))
  kth_Data = FPCA.list[[k]]
  tictoc::tic()
  kth_FPCA___nharm = sapply(seq_along(kth_Data), function(i, ...){
    FDA___FPCA___Select.nharm(pca.fd.obj = kth_Data[[i]], 
                              threshold = threshold, 
                              path_Export = paste0(path_Export, "/", Folders_Smoothed.Data[k]), 
                              file.name = ROI[i])
  })  
  tictoc::toc()
  return(kth_FPCA___nharm)
})
names(n_harm.list) = Folders_Smoothed.Data







#===============================================================================
# 3.The second FPCA
#===============================================================================
path_Export = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/3.The second FPCA___0.9"
FPCA_2.list = lapply(seq_along(path_Smoothed_Data_List), function(i, ...){
  # Read Smoothed Data
  ith_Data = readRDS(path_Smoothed_Data_List[i])
  
  
  tictoc::tic()
  # FPCA for each ROI
  ith_Data_FPCA = lapply(seq_along(ith_Data), function(k, ...){
    kth_ROI = ith_Data[[k]]
    pca.fd(kth_ROI$fdSmooth$fd, nharm = n_harm.list[[i]][k])
  })
  
  
  # Exporting FPCA results
  dir.create(path_Export, F)
  ith_Export_path = paste0(path_Export, "/", Folders_Smoothed.Data[i])
  dir.create(ith_Export_path, showWarnings = F)
  saveRDS(object = ith_Data_FPCA, paste0(ith_Export_path, "/FPCA_by_Selected_N.harm.rds"))
  
  
  tictoc::toc()
  return(ith_Data_FPCA)
})








#===============================================================================
# 4.Extract PC Scores
#===============================================================================
path_Import = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/3.The second FPCA___0.9"
path_Export = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/4.PC Scores___0.9"

FPCA_2.list = lapply(list.files(path_Import, full.names=T), function(y){
  path_RDS = list.files(y, full.names=T)
  return(readRDS(path_RDS))
})


PC_Scores.list = lapply(seq_along(FPCA_2.list), function(i, ...){
  # You can extract the principal components, eigenvalues, and other information from the 'fpca' object
  ith_Data = FPCA_2.list[[i]]
  
  dir.create(path_Export, F)
  ith_path_Export = paste0(path_Export, "/", Folders_Smoothed.Data[i])
  ith_file.name = "FPCA_PC_Scores"
  
  
  
  tictoc::tic()
  ith_PC_Scores.list = FDA___FPCA___Extract.Scores.as.DF(FPCA.list = ith_Data, 
                                                         suffix.for.each.score = paste0("ROI_", fit_length(1:length(ith_Data), 3)),
                                                         path_Export = ith_path_Export,
                                                         file.name = ith_file.name)
  tictoc::toc()
  return(ith_PC_Scores.list)
})









#===============================================================================
# 6. Plotting PCA results
#===============================================================================
# # Plot the mean function
# mean_fd = fd(fpca_selected$meanfdcoef, pbasis)
# plot(mean_fd, main = "Mean Function")
# 
# # Plot the first few harmonics (e.g., the first 3)
# for (i in 1:3) {
#   harmonic_fd = fd(fpca_selected$harmonics[, , i], pbasis)
#   plot(harmonic_fd, main = paste("Harmonic", i))
# }
# 
# # Plot the scores for the first few principal components (e.g., the first 3)
# par(mfrow = c(1, 3))
# for (i in 1:3) {
#   plot(fpca_selected$scores[, i], main = paste("Scores for PC", i), xlab = "Observation", ylab = "Score")
# }
# 
# temppca = pca.fd(tempfd$fd, nharm=4)



























