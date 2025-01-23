#' Adaptive lasso with cross-validation, using lasso(BIC) as initial estimator
#'
#' Using `cv.glmnet()` to carry out the adaptive lasso will result in an underestimation of error unless you also apply cross-validation to the initial estimator; this function provides a wrapper to do that.
#'
#' @param X           Design matrix, as in `glmnet()`
#' @param y           Response vector, as in `glmnet()`
#' @param nfolds      Number of cv folds (default: 10)
#' @param seed        For reproducibility with respect to the fold assignments
#'
#' @examples
#' attach_data(pollution)
#' cvfit <- cv.adaptive_lasso(X, y)
#' plot(cvfit)
#' summary(cvfit)
#'
#' @export

cv.adaptive_lasso <- function(X, y, nfolds=10, seed) {

  # Full data fit
  fit_init <- ncvreg(X, y, penalty='lasso')
  b <- coef(fit_init, which=which.min(BIC(fit_init)))[-1]
  w <- abs(b)^(-1)
  w <- pmin(w, Inf)
  fit <- ncvreg(X, y, penalty.factor=w, lambda.min=1e-5, penalty='lasso')

  # CV
  if (!missing(seed)) set.seed(seed)
  n <- length(y)
  fold <- sample(1:n %% nfolds)
  fold[fold==0] <- nfolds
  E <- matrix(NA, n, length(fit$lambda))
  pb <- txtProgressBar(0, nfolds, style=3)
  for (i in 1:nfolds) {
    fit0 <- ncvreg(X[fold!=i,], y[fold!=i], penalty='lasso')
    b <- coef(fit0, which=which.min(BIC(fit0)))[-1]
    w <- abs(b)^(-1)
    w <- pmin(w, Inf)
    fit1 <- ncvreg(X[fold!=i,], y[fold!=i], penalty.factor=w, lambda=fit$lambda, penalty='lasso')
    yhat <- predict(fit1, X[fold==i,])
    E[fold==i,] <- ncvreg:::loss.ncvreg(y[fold==i], yhat, 'gaussian')
    setTxtProgressBar(pb, i)
  }
  close(pb)

  # Return as cv.ncvreg object
  ind <- which(apply(is.finite(E), 2, all))
  E <- E[, ind, drop=FALSE]
  lambda <- fit$lambda[ind]
  cve <- apply(E, 2, mean)
  cvse <- apply(E, 2, sd) / sqrt(n)
  min <- which.min(cve)
  structure(list(cve=cve, cvse=cvse, fold=fold, lambda=lambda, fit=fit, min=min, lambda.min=lambda[min],
              null.dev=mean(ncvreg:::loss.ncvreg(y, rep(mean(y), n), fit$family))), class='cv.ncvreg')
}
