# 🟩 Load data =========================================================================================================
path_data_list = "/Users/Ido/Library/CloudStorage/Dropbox/✴️DataAnalysis/FunCurv/3.Classification/1.FPCA"
path_all_data_list = list.files(path_data_list, full.names = T)
path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/2.Group Penalty"







# 🟨 Group Penalty =========================================================================================================
alphas = seq(0.1, 1, by = 0.1)  # alpha: 0.1에서 1까지 0.1 간격
lambdas = 10^seq(3, -3, length.out = 100)  # lambda: log scale에서 100개의 값
penalties = c("grLasso", "grMCP", "grSCAD", "gel", "cMCP")

for(path_ith_data in path_all_data_list){
  # path_ith_data = path_all_data_list[2]
  ith_data = basename(path_ith_data)
  path_save_new = file.path(path_save, ith_data)
  dir.create(path_save_new, showWarnings = F)
  exported_file = file.path(path_save_new, "classification results.rds")
  if(file.exists(exported_file)){
    next
  }
  
  # Load the data
  path_ith_data_fpca = list.files(path_ith_data, full.names = T, pattern = "fpca_scores")
  fpca_scores = readRDS(path_ith_data_fpca)
  fpca_scores_train = fpca_scores$train
  fpca_scores_test = fpca_scores$test
  fpca_scores_rep = fpca_scores$rep
  
  # Model fitting
  opt_models_list = lapply(penalties, function(penalty){
    find_opt_model_by_cv_grpreg(fpca_scores_train,
                                fpca_scores_test,
                                fpca_scores_rep,
                                alphas,
                                lambdas,
                                penalty = penalty)
    
  }) %>% setNames(penalties)
  
  
  # save the results
  saveRDS(opt_models_list, exported_file)
  
}









