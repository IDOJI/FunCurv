# 🟥 Set the seed ==================================================================================================
set.seed(123)



# 🟥 Load data ==================================================================================================
n <- 200  # 샘플 수
p <- 100  # 원 데이터 차원
d <- 10   # 차원 축소 후 차원 수

# (가상의) 원 데이터 생성
X <- matrix(rnorm(n*p), n, p)

# 차원축소: 예를 들어 PCA 사용 (다른 방법: ICA, PLS, 방법 A는 별도 함수 필요)
Z_pca <- prcomp(X, scale. = TRUE)$x[, 1:d]
dim(Z_pca)
class(Z_pca)




# 🟥 Set The True Coef vector ==================================================================================================
beta_true <- c(2, 1.5, 0, 0, -1, 0, 1, 0, 0, 0)


# 🟥 Gen True Response ==================================================================================================
eta <- Z_pca %*% beta_true
p <- exp(eta) / (1 + exp(eta))
Y <- rbinom(n, size=1, prob=p)

# 정규화 로지스틱 회귀(Lasso) 적용
library(glmnet)
fit_a <- cv.glmnet(Z_a, Y, family="binomial", alpha=1)

# 추정 계수
beta_hat_a <- coef(fit_a, s="lambda.min")[-1] # 첫 번째 항은 intercept이므로 제외

# 다른 차원축소 방법 (예: PCA)에 대해서도 동일한 과정 수행
eta_pca <- Z_pca %*% beta_true
p_pca <- exp(eta_pca) / (1 + exp(eta_pca))
Y_pca <- rbinom(n, 1, p_pca)

fit_pca <- cv.glmnet(Z_pca, Y_pca, family="binomial", alpha=1)
beta_hat_pca <- coef(fit_pca, s="lambda.min")[-1]