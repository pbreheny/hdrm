#' Reproduce Figure 3.7
#'
#' Reproduces Figure 3.7 from the book.  If you specify any options, your results may look different.
#'
#' @param seed     For reproducibility
#' @param nfolds   As in `cv.ncvreg()` (default: 10)
#' @param ...      Additional arguments to `plot.cv.ncvreg()`
#'
#' @examples
#' Fig3.7(nfolds=3)
#' @export

Fig3.7 <- function(seed=1, nfolds=10, ...) {
  brca1 <- read_data('brca1')

  # Adaptive lasso (BIC as initial estimator)
  fit <- ncvreg(brca1$X, brca1$y, penalty='lasso')
  b <- coef(fit, which=which.min(BIC(fit)))[-1]
  w <- abs(b)^(-1)
  w <- pmin(w, Inf)
  if (!missing(seed)) set.seed(seed)
  cvfit <- cv.ncvreg(brca1$X, brca1$y, penalty.factor=w, lambda.min=5e-5, penalty='lasso', nfolds=nfolds)
  plot(cvfit, bty='n', ylab=expression(CV(lambda)), ...)
  summary(cvfit)
}
