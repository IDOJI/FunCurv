#===============================================================================
# Cutting Data for FDA
#===============================================================================
path_Subjects_List = "C:/Users/lleii/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv"

Pipeline = c("FunImgARCWSF", "FunImgARglobalCWSF")
Data_Type = c("Z.Standardization", "Raw")
Manufacturer = list(NULL, "GE.MEDICAL.SYSTEMS", "SIEMENS", "Philips")

for(i in seq_along(Pipeline)){
  ith_Pipeline = Pipeline[i]
  
  for(j in seq_along(Data_Type)){
    jth_Data_Type = Data_Type[j]
    
    for(k in seq_along(Manufacturer)){
      kth_Manufacturer = Manufacturer[[k]]
      
      path_Import = paste0("C:/Users/lleii/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals/ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type)
      
      if(is.null(kth_Manufacturer)){
        path_Export = paste0("C:/Users/lleii/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type, "___", "SB", "___", "Cut.for.FDA")  
      }else{
        path_Export = paste0("C:/Users/lleii/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type, "___", kth_Manufacturer, "___", "Cut.for.FDA")  
      }
      Data_Cut.list = RS.fMRI_5_BOLD.Signals___FDA___Cutting.Data.by.Min.Timepoints(path_Import, path_Subjects_List, path_Export, BAND.TYPE = "SB", Manufacturer = kth_Manufacturer)
    }
  }
}




