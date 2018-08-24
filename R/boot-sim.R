#' Reproduce Table 9.1
#'
#' Reproduces Table 9.1 from the book.  If you specify any options, your results may look different.
#'
#' @param N          Number of simulated realizations
#' @param B          Number of bootstrap replications (for each data set)
#' @param n          Sample size
#' @param p          Number of features
#' @param a          Number of causal ('A') variables
#' @param b          Number of correlated ('B') variables
#' @param rho        Correlation between 'A' and 'B' variables
#' @param noise      Correlation structure between features ('exchangeable' | 'autoregressive')
#' @param rho.noise  Correlation parameter for noise variables
#' @param seed       Random number seed for reproducibility
#' @param ...        Further arguments to \code{\link{genDataABN}}
#'
#' @examples
#' Tab9.1(N=5)   # Increase N for more reliable results

Tab9.1 <- function(N=100, B=100, n=100, p=100, a=10, b=2, rho=0.5, noise='autoregressive', rho.noise=0.8, seed=1, ...) {
  set.seed(seed)
  pb <- txtProgressBar(0, N, style=3)
  cov <- matrix(NA, N, p, dimnames=list(1:N, 1:p))
  for (i in 1:N) {
    Data <- genDataABN(n=n, p=p, a=a, b=b, rho=rho, noise=noise, rho.noise=rho.noise, ...)
    cvfit <- cv.glmnet(Data$X, Data$y)
    res <- boot.glmnet(Data$X, Data$y, lambda=cvfit$lambda.min, B=B, bar=FALSE)
    cov[i,] <- Data$beta >= res$Lower & Data$beta <= res$Upper
    setTxtProgressBar(pb, i)
  }
  out <- data.frame(A=mean(cov[, Data$varType=="A"]),
                    B=mean(cov[, Data$varType=="B"]),
                    N=mean(cov[, Data$varType=="N"]),
                    Overall=mean(cov))
  rownames(out) <- "Coverage"
  out
}
