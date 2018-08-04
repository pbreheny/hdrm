#' Calculate confidence intervals for lasso using bootstrap
#'
#' @param X          Design matrix
#' @param y          Response vector
#' @param N          Number of bootstrap resamples (default 500)
#' @param lambda     Regularization parameter at which solutions are to be bootstrapped (by default, uses cross-valiation to find lambda)
#' @param seed       Seed (optional)
#' @param alpha      Error rate; 0.05 (default) corresponds to 95% confidence interval
#'
#' @example ex/boot.R

boot.glmnet <- function(X, y, N=500, lambda, seed, alpha=0.05, ...) {
  p <- ncol(X)
  n <- nrow(X)
  if (missing(lambda)) {
    cvfit <- cv.glmnet(X, y)
    lambda <- cvfit$lambda.min
  }
  pb <- txtProgressBar(0, N, style=3)
  B <- matrix(NA, N, p, dimnames=list(1:N, colnames(X)))
  if (!missing(seed)) set.seed(seed)
  for (i in 1:N) {
    ind <- sample(1:n, replace=TRUE)
    fit.i <- glmnet(X[ind,], y[ind], nlambda=20, lambda.min=lambda)
    b <- coef(fit.i, s=lambda)
    B[i,] <- b[-1]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  out <- t(apply(B, 2, quantile, probs=c(alpha/2, 1-alpha/2)))
  rownames(out) <- rownames(b)[-1]
  out
}
