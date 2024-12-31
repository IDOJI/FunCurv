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
List.list[[1]] = visual = c("ggpubr", "ggplot2", "ggstatsplot", "ggsignif", "rlang", "RColorBrewer")
List.list[[2]] = stat = c("fda", "MASS")
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








# 🟥 Define Functions ##########################################################################
## 🟨 CCA ====================================================================================
CCA___Algorithm1 = function(Cov.list, explained_var_prop = 0.9, epsilon = 1e-6) {
  # 🟧 Define functions for the algorithm ===================================================
  ## ✅ Extracting Upper elements 
  uppper_tri = function(mat){
    mat[upper.tri(mat, diag = FALSE)]
  }
  
  ## ✅  Checking identity
  is.nearly.identity <- function(mat, tolerance = 1e-6) {
    # Check dimension of mat
    if (!is.matrix(mat)) stop("The input is not a matrix.")
    if (nrow(mat) != ncol(mat)) return(FALSE)
    
    # check if the diagonal elements are 1
    if (any(abs(diag(mat) - 1) > tolerance)) return(FALSE)
    
    # Check if the non-diagonal elements are near 0
    off.diagonal <- mat - diag(diag(mat))
    if (any(abs(off.diagonal) > tolerance)) return(FALSE)
    
    return(TRUE)
  }
  
  
  ## ✅  M(U) = \sum_{t}{X_t U U^T X_t} 
  Compute_M_U = function(Cov.list, U){
    Reduce("+", lapply(Cov.list, function(X_t){ X_t %*% U %*% t(U) %*% X_t }))
  }
  
  
  
  ## ✅  f(U) = Tr(U^T M(U) U)
  Compute_f_U = function(Cov.list, U){
    (t(U) %*% Compute_M_U(Cov.list, U) %*% U) %>% diag %>% sum
  }
  
  
  
  
  
  # 🟧 Define U_0 from data =====================================================================
  # random U_0
  # U = U_0 <- qr.Q(qr(matrix(rnorm(n * r), n, r)))
  
  # Sum the squares of the covariance matrices
  M <- Reduce("+", lapply(Cov.list, function(X) { X %*% X }))
  M <- (M + t(M)) / 2 # sym
  
  # Perform Eigenvalue Decomposition on the summed matrix
  eigen_decomp <- eigen(M, symmetric = TRUE)
  
  # Decide r by explained_var_prop
  cum_var <- cumsum(eigen_decomp$values) / sum(eigen_decomp$values)
  r <- which(cum_var >= explained_var_prop)[1]
  
  # Initialize U_0
  U_k = U_0 <- eigen_decomp$vectors[, 1:r, drop = F]
  
  # check the rank
  if(diag(t(U_0) %*% U_0) %>% sum - r > epsilon){
    stop("The rank is not r!")
  }
  
  
  # initialize f_k
  f_k = f_0 = Compute_f_U(Cov.list, U_k)
  
  
  
  # 🟧 Repeat the algorithm =====================================================================
  repeat{
    tictoc::tic()
    ## ✅ 1) Perform the EVD on $M(U_k) = \sum_t{X_t U_k U^T_k X_t}$
    # Compute M(U_k)
    M_U_k <- Compute_M_U(Cov.list, U_k)
    
    # is symmetric?
    if(!isSymmetric(M_U_k)){
      stop("The M(U_k) matrix is not symmetric")
    }
    
    # EVD
    evd_U_k <- eigen(M_U_k, symmetric = TRUE)
    
    
    
    ## ✅ 2) Choose the leading $r$ eigenvectors $U_{k+1}$ 
    # Decide r by explained_var_prop
    cum_var <- cumsum(evd_U_k$values) / sum(evd_U_k$values)
    r <- which(cum_var >= explained_var_prop)[1]
    
    # Update U_k -> U_{k+1}
    U_k <- evd_U_k$vectors[, 1:r, drop = F]
    
    # Check if the matrix is identical : orthogonality of eigenvectors
    if(!is.nearly.identity(t(U_k) %*% U_k)){
      stop("The t(U_k) %*% U_k) is not an identity matrix.")
    }
    
    
    
    ## ✅  3) Compute $Y_t = U^{T}_{k+1} X_t U_{k+1}$\
    # Compute Y_t for each t
    Y_t.list <- lapply(Cov.list, function(X_t) {
      t(U_k) %*% X_t %*% U_k
    })
    
    
    
    ## ✅  4) until {f(U_k+1) - f(U_k)}/{f(U_k)} =< e
    # Maximum problem : Compute the objective function value for convergence check
    f_kp1 = Compute_f_U(Cov.list, U_k)
    tictoc::toc()
    
    
    # Check for convergence
    difference = abs(f_kp1 - f_k) / f_k
    cat("\n", crayon::green("The difference was "), crayon::red(difference),"\n")
    
    if(difference <= epsilon){
      
      break
      
    }else{
      # Update for the next iteration
      f_k = f_kp1
      f_kp1 = NULL
    }
  }# repeat
  
  
  
  
  # 🟧 Extracting only upper elements =====================================================================
  Half_Vectorized = lapply(Y_t.list, uppper_tri) %>% do.call(rbind, .)
  
  
  # 🟧 Return results =====================================================================
  list(Y_t = Y_t.list, Half_Vectorized = Half_Vectorized, U_k = U_k) %>% return()
}











