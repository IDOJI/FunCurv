# 🟥 Load Functions & Packages ##########################################################################
# rm(list = ls())
Sys.setlocale("LC_ALL", "en_US.UTF-8")

## 🟨Install and loading Packages ================================
install_packages = function(packages, load=TRUE) {
  # load : load the packages after installation?
  for(pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
    
    if(load){
      library(pkg, character.only = TRUE, quietly = T)
    }
  }
}

List.list = list()
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer", "reshape2")
List.list[[2]] = stat = c("fda", "MASS", "caret")
List.list[[3]] = data_handling = c("tidyverse", "dplyr", "clipr", "tidyr", "readr", "caret", "readxl")
List.list[[4]] = qmd = c("janitor", "knitr")
List.list[[5]] = texts = c("stringr", "stringi")
List.list[[6]] = misc = c("devtools")
List.list[[7]] = db = c("RMySQL", "DBI", "odbc", "RSQL", "RSQLite")
List.list[[8]] = sampling = c("rsample")
List.list[[9]] = excel = c("openxlsx")
List.list[[10]] = others = c("beepr")

packages_to_install_and_load = unlist(List.list)
install_packages(packages_to_install_and_load)

## 🟧dplyr =======================================================
filter = dplyr::filter
select = dplyr::select






# 🟪 Total function =========================================================================================================================
subjects_list = read.csv("/Users/Ido/Documents/✴️DataAnalysis/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv")



total_function = function(){
  # 🟨 Sampling subjects ===============================================================================================================================
  sampled_subjects_list = sampling_subjects(subjects_list,diagnosis_ratio, diagnosis_groups, n_samples, seed)
  
  
  # 🟨 Sampling data ===============================================================================================================================
  
  
  
}








# 🟩 Sampling =========================================================================================================
sampling_subjects <- function(subjects_list, 
                              diagnosis_ratio = c(0.1, 0.5, 0.4), 
                              diagnosis_groups = c("CN", "MCI", "Dementia"), 
                              n_samples = 100, 
                              seed = 1234) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # 🟨 Subset SB only =============================================================================
  subjects_list <- subjects_list %>%
    dplyr::filter(EPI___BAND.TYPE == "SB")
  
  # 🟨 Diagnosis ratio and group matching =============================================================================
  diagnosis_levels <- unique(subjects_list$DIAGNOSIS_FINAL)
  
  # Check if the sum of ratios equals 1
  if (sum(diagnosis_ratio) != 1) {
    stop("The sum of diagnosis_ratio must equal 1")
  }
  
  # Check if the provided diagnosis_groups match the levels in DIAGNOSIS_FINAL
  if (length(diagnosis_ratio) != length(diagnosis_groups)) {
    stop("The length of diagnosis_ratio must match the number of diagnosis_groups")
  }
  
  # Check if all diagnosis_groups exist in the data
  if (!all(diagnosis_groups %in% diagnosis_levels)) {
    missing_groups <- diagnosis_groups[!diagnosis_groups %in% diagnosis_levels]
    stop(paste("The following diagnosis_groups are not found in DIAGNOSIS_FINAL:", 
               paste(missing_groups, collapse = ", "), 
               "\nPlease use one or more of the valid groups:", 
               paste(diagnosis_levels, collapse = ", ")))
  }
  
  # Create a named vector of ratios for easy matching
  diagnosis_ratio_named <- setNames(diagnosis_ratio, diagnosis_groups)
  
  # Calculate the number of samples per group based on the total number of samples
  sample_sizes <- round(n_samples * diagnosis_ratio_named)
  
  # Initialize an empty list to store sampled data
  sampled_data_list <- list()
  
  # Loop over each diagnosis group and sample accordingly
  for (group in diagnosis_groups) {
    group_data <- subjects_list %>% dplyr::filter(DIAGNOSIS_FINAL == group)
    sampled_data_list[[group]] <- group_data %>% 
      dplyr::sample_n(size = sample_sizes[group], replace = TRUE)
  }
  
  
  return(sampled_data_list)
}


# 🟥 logistic with Group penalty =========================================================================================================
# Load the Birthwt dataset from the package
data(Birthwt)

# Prepare the data for logistic regression
X <- Birthwt$X    # Predictor variables
y <- Birthwt$low  # Binary response variable (low birth weight)
group <- Birthwt$group  # Grouping for the predictors

# Fit the logistic regression model with group Lasso penalty
fit <- grpreg(X, y, group, penalty = "grLasso", family = "binomial")

# Plot the coefficient paths
plot(fit)

# Select the best model using BIC
best_fit <- select(fit, criterion = "BIC")

# Extract the coefficients of the selected model
coef(best_fit)





# 🟥 proportional logistic with group penalty =========================================================================================================











