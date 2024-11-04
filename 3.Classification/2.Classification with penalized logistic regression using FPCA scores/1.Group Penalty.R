# 🟩 Load data =========================================================================================================
path_data_list <- "E:/FunCurv/3.Classification/1.FPCA"
path_all_data_list <- list.files(path_data_list, full.names = TRUE)
path_save <- "E:/FunCurv/3.Classification/2.Classification with penalized logistic regression using FPCA scores/1.Group Penalty"

# 🟨 Group Penalty =========================================================================================================
alphas <- seq(0.1, 1, by = 0.1)  # alpha: 0.1에서 1까지 0.1 간격
lambdas <- 10^seq(3, -3, length.out = 100)  # lambda: log scale에서 100개의 값
penalties <- c("grLasso", "grMCP", "grSCAD", "gel", "cMCP")



for(path_ith_data in path_all_data_list){
  # path_ith_data = path_all_data_list[19]
  ith_data <- basename(path_ith_data)
  path_save_new <- file.path(path_save, ith_data)
  if (!dir.exists(path_save_new)) dir.create(path_save_new, showWarnings = FALSE)
  
  # Load the datax
  path_ith_data_fpca <- list.files(path_ith_data, full.names = TRUE, pattern = "fpca_scores")
  fpca_scores <- readRDS(path_ith_data_fpca)
  fpca_scores_train <- fpca_scores$train
  fpca_scores_test <- fpca_scores$test
  fpca_scores_rep <- fpca_scores$rep
  
  # Model fitting
  for(penalty in penalties){
    # penalty = penalties[1]
    path_exported_file <- file.path(path_save_new, paste0(penalty, ".rds"))
    if(file.exists(path_exported_file)){
      next
    }
    
    # Optimize model with cross-validation
    opt_models <- find_opt_model_by_cv_grpreg(fpca_scores_train,
                                              fpca_scores_test,
                                              fpca_scores_rep,
                                              alphas,
                                              lambdas,
                                              penalty = penalty)
    # Save the results
    saveRDS(opt_models, path_exported_file)
  }
}

