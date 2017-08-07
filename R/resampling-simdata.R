Ex9.1 <- function(n=100, p=60, n.corr=2, rho=0.5, seed=1) {
  set.seed(seed)
  p1 <- 6
  K <- n.corr + 1
  bb <- numeric(p1*K)
  bb[(0:(p1-1))*K+1] <- c(1,-1,0.5,0.5,-0.5,-0.5)
  A <- matrix(rho, K, K) + (1-rho)*diag(K)
  B <- diag(p-K*p1)
  Sigma <- bdiag(A, A, A, A, A, A, B)
  R <- chol(Sigma)

  X <- as.matrix(matrix(rnorm(n * p), n, p) %*% R)
  colnames(X) <- paste0("V", 1:p)
  y <- X[,1:(p1*K)] %*% bb + rnorm(n)

  varType <- vector("character", p)
  varType[(0:5)*K+1] <- "A"
  varType[c((0:5)*K+2, (0:5)*K+3)] <- "B"
  varType[(p1*K+1):p] <- "N"

  list(X=X, y=y, varType=varType)
}
