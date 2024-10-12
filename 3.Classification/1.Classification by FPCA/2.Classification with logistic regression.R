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
# Loading Data
#===============================================================================
Clipboard_to_path()
# Mac
path_Data = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/4.FPCA/5.PC Scores with Diagnosis___0.9"
# Windows
path_Data = "C:/Users/lleii/Dropbox/Github/5.PC Scores with Diagnosis___0.9"
path_Data_List = list.files(path_Data, full.names=T)
Data = lapply(path_Data_List, readRDS)

Data_List = sapply(list.files(path_Data), function(y){str_split(y, "\\.rds")[[1]][1]}) %>% unname
names(Data) = Data_List










#===============================================================================
# Splitting Dataset
#===============================================================================
Data_Splitted = lapply(Data, function(ith_Data){
  ith_Data_Splitted = Split.Dataset(ith_Data$PC.Scores, y = ith_Data$Diagnosis, train_percentage = 0.8)
  ith_Data_New = list(Train_Set = ith_Data_Splitted[[1]], Test_Set = ith_Data_Splitted[[2]], Coef_Groups_Ind = ith_Data[[3]])
  return(ith_Data_New)
})
names(Data_Splitted) = Data_List







#===============================================================================
# [Classification] glm : Multinomial logistic with penalty
#===============================================================================
# Assume X is your feature matrix and Y is your response vector
# Also, assume that Y is a factor with levels corresponding to your classes

# Fit the model
fit <- glmnet(X, Y, family = "multinomial", alpha = 1)

# alpha is the elastic net mixing parameter (0 for ridge regression, 1 for lasso)
# You can adjust this parameter based on your needs

# To make predictions:
preds <- predict(fit, newx = X_new, type = "class")

# X_new is the new data for which you want to make predictions








data = read.csv("/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers___Data/ADNI___RS.fMRI___Subjects.Lists/Subjects_Lists_Exported/Final/[Final_Selected]_Subjects_list.csv")
data = data %>% filter(NFQ___BAND.TYPE=="SB")

# SEX
data$DEMO___SEX %>% table



# AGE of total
data$DEMO___AGE %>% sd
data$DEMO___AGE %>% mean

# AGE of each sex
age_F = data %>% filter(DEMO___SEX == "Female") %>% dplyr::select(DEMO___AGE) %>% unlist
age_M = data %>% filter(DEMO___SEX == "Male") %>% dplyr::select(DEMO___AGE) %>% unlist

mean(age_F)
sd(age_F)

mean(age_M)
sd(age_M)


# diagnosis
data$DEMO___DIAGNOSIS_NEW %>% table

Atlas_Labels[grep("Hippo", Atlas_Labels)]




#===============================================================================
# [Classification_1] msgl: Multinomial Sparse Group Lasso
#===============================================================================
path_Export = "/Users/Ido/Library/CloudStorage/Dropbox/Github/Papers/Papers___Wrting/Papers___Exploring Resting-State fMRI Signals in Alzheimer's Disease Using Functional Data Analysis/Data/5.Classification___MSGL___0.9"

Classification___Multinomial___MSGL = function(Train_X, 
                                               Train_y,
                                               Test_X,
                                               Test_y,
                                               features.grouping,
                                               n_fold=5, 
                                               alpha_seq = seq(0, 1, by = 0.01), 
                                               lambda.min = 0.01, 
                                               d=100,
                                               lambda.min.rel=TRUE,
                                               standardize=FALSE,
                                               sparse.data = FALSE, 
                                               parallel.cores = 8,
                                               path_Export){
  #=============================================================================
  # Description
  #=============================================================================
  # Reference : Sparse group lasso and high dimensional multinomial classification 
  # Arguments
  # features.grouping : coefficient groups indicators
  # sparse.data : TRUE = X will be treated as sparse
  
  
  
  
  
  
  #=============================================================================
  # Test data
  #=============================================================================
  # "ROI___AAL3___FunImgARCWSF___Z.Standardization___SB"
  # i=7
  
  # "ROI___AAL3___FunImgARglobalCWSF___Z.Standardization___SB"
  # i=15
  # Train_X = Data_Splitted[[i]]$Train_Set$X
  # Train_y = Data_Splitted[[i]]$Train_Set$y
  # 
  # Test_X = Data_Splitted[[i]]$Test_Set$X
  # Test_y = Data_Splitted[[i]]$Test_Set$y

  # features.grouping = Data_Splitted[[i]]$Coef_Groups_Ind
  
  
  
  
  
  
  
  #=============================================================================
  # Fitting CV
  #=============================================================================
  Fit_CV.list = Classification___Multinomial___MSGL___Fitting.by.CV(X = Train_X,
                                                                    y = Train_y,
                                                                    features.grouping,
                                                                    n_fold,
                                                                    alpha_seq,
                                                                    lambda.min,
                                                                    d,
                                                                    lambda.min.rel,
                                                                    sparse.data,
                                                                    standardize,
                                                                    parallel.cores,
                                                                    path_Export)
  
  
  
  
  
  All_Fit_CV 
  
  
  
  #=============================================================================
  # Find the best model from CV list
  #=============================================================================
  Best_CV = Classification___Multinomial___MSGL___Select.Best.Model(Fit_CV.list, alpha_seq)
  
  
  
  
  #=============================================================================
  # Final fitting
  #=============================================================================
  sglOptim::get_coef(Best_CV$Best_CV)
  sglOptim::models(Best_CV$Best_CV)
  sglOptim::sgl_print(Best_CV$Best_CV)
   (Best_CV$Best_CV$)
  
  Final_Fit = msgl::fit(x = Train_X,
                        classes = Train_y,
                        sampleWeights = NULL,
                        grouping = features.grouping,
                        groupWeights = NULL,
                        parameterWeights = NULL,
                        alpha = Best_CV,
                        standardize = ,lambda = ,d = ,return_indices = ,intercept = ,sparse.data = ,algorithm.config)
    fit(x, classes, sampleWeights = NULL, grouping = NULL,
        groupWeights = NULL, parameterWeights = NULL, alpha = 0.5,
        standardize = TRUE, lambda, d = 100, return_indices = NULL,
        intercept = TRUE, sparse.data = is(x, "sparseMatrix"),
        algorithm.config = msgl.standard.config)
    
    
  
  
  
  # count the number of nonzero features in each model
  # Parmeters = parameters(fit)
  # N_Parmeters = sapply(features(fit), length)
  
    
    list(model.info = fit.cv, CV.Error = CV.Error)
  
  
  #=========================================================================
  # Prediction in Test_set
  #=========================================================================
  Classification___Multinomial___MSGL___Prediction = function(Test_X, Test_y, fitted_model){
    fitted_model = 
    
  }
  
  
  Classification___Multinomial___MSGL___Prediction(Test_X, Test_y, fitted_model){
    
    
  }
  
}



predict


# 각 람다에 대해 선택된 features


fit <- msgl::fit(X, y, alpha = .5, lambda = lambda)

# the nonzero features of model 1, 10 and 25
features(fit)[c(1,10,25)]



sglOptim::features()


data(SimData)


lambda <- msgl::lambda(x, classes, alpha = .5, d = 50, lambda.min = 0.05)
fit <- msgl::fit(x, classes, alpha = .5, lambda = lambda)

# the nonzero parameters of model 1, 10 and 25
parameters(fit)[c(1,10,25)]

# count the number of nonzero parameters in each model
sapply(parameters(fit), sum)


coef(fit)
features.msgl(fit.cv)




















