#' Calculate confidence intervals for lasso using bootstrap
#'
#' @param X          Design matrix
#' @param y          Response vector
#' @param B          Number of bootstrap resamples (default 500)
#' @param lambda     Regularization parameter at which solutions are to be bootstrapped (by default, uses cross-valiation to find lambda)
#' @param seed       Seed (optional)
#' @param alpha      Error rate; 0.05 (default) corresponds to 95\% confidence interval
#' @param bar        Print a progress bar?
#'
#' @example ex/boot.R

boot.glmnet <- function(X, y, B=500, lambda, seed, alpha=0.05, bar=TRUE) {
  p <- ncol(X)
  n <- nrow(X)
  if (missing(lambda)) {
    cvfit <- cv.glmnet(X, y)
    lambda <- cvfit$lambda.min
  }
  if (bar) pb <- txtProgressBar(0, B, style=3)
  beta_hat <- matrix(NA, B, p, dimnames=list(1:B, colnames(X)))
  if (!missing(seed)) set.seed(seed)
  for (i in 1:B) {
    ind <- sample(1:n, replace=TRUE)
    fit.i <- glmnet(X[ind,], y[ind], nlambda=20, lambda.min=lambda)
    b <- coef(fit.i, s=lambda)
    beta_hat[i,] <- b[-1]
    if (bar) setTxtProgressBar(pb, i)
  }
  if (bar) close(pb)
  out <- as.data.frame(t(apply(beta_hat, 2, quantile, probs=c(alpha/2, 1-alpha/2))))
  rownames(out) <- rownames(b)[-1]
  names(out) <- c('Lower', 'Upper')
  out
}
