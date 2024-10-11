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




# 🟥 Define smoothing functions =========================================================================================================
## 🟨 Multiple : smoothing by bspline gcv =======================================================================
smoothing_multiple_ROIs = function(path_FC, n_order, lambdas, path_export){
  library(magrittr)
  library(fda)
  library(crayon)  # Load the crayon package for colored output
  
  # Extract brain atlas name from the path_FC filename
  atlas_name = tools::file_path_sans_ext(basename(path_FC))  # Extract filename without extension
  
  # Create export directory using atlas name
  atlas_export_path = file.path(path_export, atlas_name)
  if (!dir.exists(atlas_export_path)) {
    dir.create(atlas_export_path, recursive = TRUE)
  }
  
  cat(green("Results will be saved in:"), bold(atlas_export_path), "\n")
  
  # Read the RDS file containing the list of ROIs
  FC = readRDS(path_FC)
  
  # Apply smoothing to each ROI in the list
  results = lapply(names(FC), function(roi_name){
    cat(cyan("Processing ROI:"), bold(roi_name), "\n")
    
    # Extract the kth ROI and set up the domain
    kth_ROI = FC[[roi_name]]
    domain = kth_ROI$Euclid_dist
    kth_ROI$ROI = kth_ROI$Euclid_dist = NULL
    
    # Create the file name for saving the results using the ROI name
    file_name = paste0(roi_name, "_smoothed_result.rds")  # Save as .rds file
    file_path = file.path(atlas_export_path, file_name)
    
    # Check if the file already exists and is non-empty
    if (file.exists(file_path) && file.info(file_path)$size > 0) {
      cat(yellow("Skipping ROI:"), bold(roi_name), "(File already exists and is non-empty)\n")
      return(NULL)  # Skip this ROI and return NULL
    }
    
    # Call the smoothing function for each ROI and save the results
    smoothing_result = smoothing_by_bspline_gcv(kth_ROI, domain, n_order, lambdas, atlas_export_path, file_name)
    
    # Completion message
    cat(green("Completed smoothing for ROI:"), bold(roi_name), 
        "(", blue(match(roi_name, names(FC))), "/", length(FC), ")\n")
    
    # Save the smoothing result to the specified file path
    saveRDS(smoothing_result, file = file_path)
    
    return(smoothing_result)
  })
  
  # Remove NULL entries from the results list
  results = results[!sapply(results, is.null)]
  
  # Save the entire results list as an RDS file
  saveRDS(results, file = file.path(atlas_export_path, "results_smoothed.rds"))
  
  return(results)
}



## 🟨 Single : smoothing by bspline gcv =======================================================================
smoothing_by_bspline_gcv = function(kth_ROI, 
                                    domain, 
                                    n_order, 
                                    lambdas, 
                                    path_export = NULL, 
                                    file_name = "smoothing_result",
                                    width = 2000){
  library(magrittr)
  library(fda)
  library(crayon)  # Load the crayon package for colored output
  
  # Convert kth_ROI to matrix
  X = kth_ROI %>% as.matrix
  
  # Initial plot before smoothing
  if (!is.null(path_export)) {
    file_path_before = file.path(path_export, paste0(file_name, "_before.png"))
    
    # Check if the initial plot already exists
    if (file.exists(file_path_before)) {
      cat(yellow("Skipping initial plot: File already exists at"), bold(file_path_before), "\n")
    } else {
      png(filename = file_path_before, width = width, height = 600)
      matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, main = "Original Data Before Smoothing")
      dev.off()
      cat(green("Saved plot before smoothing at:"), bold(file_path_before), "\n")
    }
  } else {
    matplot(x = domain, y = X, type = "l", col = 1:ncol(X), lty = 1, main = "Original Data Before Smoothing")
  }
  
  # Define smoothing result file path
  smoothing_result_file = file.path(path_export, paste0(file_name, "_smoothing_result.rds"))
  
  # Check if the smoothing result file already exists
  if (!is.null(path_export) && file.exists(smoothing_result_file)) {
    cat(yellow("Skipping smoothing and plotting: File already exists at"), bold(smoothing_result_file), "\n")
    return(readRDS(smoothing_result_file))  # Return the saved result if it exists
  }
  
  # Define bspline basis
  fdobj_basis = create.bspline.basis(rangeval = c(min(domain), max(domain)), 
                                     norder = n_order, 
                                     breaks = seq(from = min(domain), to = max(domain), length.out = nrow(kth_ROI)))
  
  # Find the optimal GCV
  gcvs = sapply(lambdas, function(ith_lambda){
    tryCatch({
      fdPar_obj = fdPar(fdobj = fdobj_basis, Lfdobj = int2Lfd(2), lambda = ith_lambda)
      fdSmooth_obj = smooth.basis(argvals = domain, y = X, fdParobj = fdPar_obj)
      mean(fdSmooth_obj$gcv)
    }, error = function(e) {
      cat(red("Error occurred for lambda =", ith_lambda, "\n"))
      NA
    })
  })
  
  # Smoothing using the optimal parameter
  opt_lambda = lambdas[which.min(gcvs)]
  opt_fdPar_obj = fdPar(fdobj = fdobj_basis, Lfdobj = int2Lfd(2), lambda = opt_lambda)
  opt_fdSmooth_obj = smooth.basis(argvals = domain, y = X, fdParobj = opt_fdPar_obj)
  
  # Save the smoothing result to file
  saveRDS(list(fdSmooth_obj = opt_fdSmooth_obj, lambda = opt_lambda), file = smoothing_result_file)
  cat(green("Saved smoothing result at:"), bold(smoothing_result_file), "\n")
  
  # Plot after smoothing
  if (!is.null(path_export)) {
    file_path_after = file.path(path_export, paste0(file_name, "_after.png"))
    
    # Check if the final plot already exists
    if (file.exists(file_path_after)) {
      cat(yellow("Skipping plot after smoothing: File already exists at"), bold(file_path_after), "\n")
    } else {
      png(filename = file_path_after, width = width, height = 600)
      plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, main = paste("Optimal Smoothing with lambda =", opt_lambda))
      dev.off()
      cat(green("Saved plot after smoothing at:"), bold(file_path_after), "\n")
    }
  } else {
    plot(opt_fdSmooth_obj$fd, col = 1:ncol(X), lty = 1, main = paste("Optimal Smoothing with lambda =", opt_lambda))
  }
  
  return(list(fdSmooth_obj = opt_fdSmooth_obj, lambda = opt_lambda))
}








