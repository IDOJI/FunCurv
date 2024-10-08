#===============================================================================
# Smoothing by B.Spline
#===============================================================================
# norder = 4, knots 고정, lambda만 바꿔서 튜닝
initial_lambda = exp(0)
path_Export = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/3.Smoothing.by.Bspline"
path_Data = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___BOLD.Signals___FDA/2.Combined.by.ROI"

path_Data_List = list.files(path_Data, full.names=T)
Data_List =  list.files(path_Data, full.names=F)

Smoothing.list = lapply(seq_along(Data_List), function(i, ...){
  dir.create(path_Export, T)
  ith_Data.list = readRDS(path_Data_List[i])
  ith_Data_Name = tools::file_path_sans_ext(Data_List[i])
  tictoc::tic()
  ith_Results = FDA___Smoothing___Bspline___Multiple.Functions(Signals.list = ith_Data.list,
                                                               norder = 4,
                                                               initial_lambda = initial_lambda,
                                                               Lfdobj = int2Lfd(2),
                                                               path_Export = paste0(path_Export, "/", ith_Data_Name))
  tictoc::toc() 
  cat("\n", crayon::blue("Smoothing is done!"),"\n")
  return(ith_Results)
})