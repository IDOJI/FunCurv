#===============================================================================
# Combining by ROI
#===============================================================================
# 경로 길이가 길어서 파일 옮긴 후 진행함.

Pipeline = c("FunImgARCWSF", "FunImgARglobalCWSF")
Data_Type = c("Z.Standardization", "Raw")
Manufacturer = list(NULL, "GE.MEDICAL.SYSTEMS", "SIEMENS", "Philips")

path_Export = "C:/Users/lleii/Desktop/2.Combined.by.ROI"
for(i in seq_along(Pipeline)){
  ith_Pipeline = Pipeline[i]
  
  for(j in seq_along(Data_Type)){
    jth_Data_Type = Data_Type[j]
    
    for(k in seq_along(Manufacturer)){
      kth_Manufacturer = Manufacturer[[k]]
      
      if(is.null(kth_Manufacturer)){
        # C:\Users\lleii\Dropbox\Github\1.Cut.for.FDA
        path_Data = paste0("C:/Users/lleii/Dropbox/Github/1.Cut.for.FDA/ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type, "___", "SB")  
        file.name = paste0("ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type, "___", "SB")
      }else{
        path_Data = paste0("C:/Users/lleii/Dropbox/Github/1.Cut.for.FDA/ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type, "___", kth_Manufacturer)
        file.name = paste0("ROI___AAL3___", ith_Pipeline, "___", jth_Data_Type, "___", kth_Manufacturer)
      }
      Combined.list = RS.fMRI_5_BOLD.Signals___FDA___Combining.by.ROI(path_Data, path_Export, file.name)
    }
  }
}