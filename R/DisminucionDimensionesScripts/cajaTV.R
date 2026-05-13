# X ~ N_3(mu, Sigma) 
mu = c(1,2,3);
Sigma = matrix(
  c(4, 1, 1,
    1, 6, 4, 
    1, 4, 9), 
  nrow = 3, byrow = T)

# Eigen-decomposition of Sigma 
ES = eigen(Sigma,symmetric=T)
lambda = ES$values
U = ES$vectors

# Square-root of Sigma 
Sigma.half = U %*% diag(sqrt(lambda)) %*% t(U)


# Generating MVN random variates

# 1. Generate n*p standard normal random variates
n = 1000
p = 3
Z = matrix(rnorm(n*p),ncol = n, nrow = p)
# 2. Then transform Z to X 
X =Sigma.half %*% Z + mu

library(rgl)
plot3d(X[1,],X[2,],X[3,])
