# 🟨 FPCA + Demographics ===========================================================================================
## 🟩 Non-group penalty ================================================================================================


## 🟩 Group penalty ================================================================================================
## 🟩 Non-group penalty ================================================================================================

## 🟩 Group penalty ================================================================================================
lambdas = exp(seq(-6, 3, length.out = 200))
alphas = seq(0, 1, length.out = 20)
alphas = alphas[alphas!=0]

path_splitted_subjects = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/2.Split train and test data"
# path_splitted_subjects = "C:/Users/clair/Dropbox/✴️DataAnalysis/FunCurv/1.Data Indexing/2.Split train and test data"
# path_splitted_subjects <- "C:/Users/clair/OneDrive/바탕 화면/FunCurv_data/1.Data Indexing/2.Split train and test data"

path_FPCA = "E:/FunCurv/3.Classification/1.FPCA" %>% set_output_path()
# path_FPCA <- "C:/Users/clair/Dropbox/✴️DataAnalysis/FunCurv/3.Classification/1.FPCA_2"
# path_FPCA = "C:/Users/clair/OneDrive/바탕 화면/FunCurv_data/3.Classification/1.FPCA"

path_save = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/2.Classification with penalized logistic regression using FPC scores"
# path_save <- "C:/Users/clair/Dropbox/✴️DataAnalysis/FunCurv/3.Classification/2.Classification with penalized logistic regression using FPC scores"
# path_save = "C:/Users/clair/OneDrive/바탕 화면/FunCurv_data/3.Classification/2.Classification with penalized logistic regression using FPC scores"


demographics = read.csv("/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/1.Data Indexing/1.Subjects List/9.MT1-EPI-Merged-Subjects-List.csv")

path_cv_data = "/Volumes/ADNI_SB_SSD_NTFS_4TB_Sandisk/FunCurv/3.Classification/1.FPCA/AD, CN___FunImgARCWSF_Fisher Z FC"
path_cv_data_all = list.files(path_cv_data, full.names = T)
cv_data = lapply(path_cv_data_all, readRDS) %>% setNames(tools::file_path_sans_ext(basename(path_cv_data_all)))








