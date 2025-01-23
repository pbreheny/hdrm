#' Reproduce Figure 2.9
#'
#' Reproduces Figure 2.9 from the book; if you specify any options, your results may look different.
#'
#' @examples
#' Fig2.9()
#'
#' # By hand
#' attach_data(pollution)
#' cvfit <- cv.glmnet(std(X), y)   # Standardize design matrix
#' rsq <- 1-cvfit$cvm/var(y)
#'
#' @export

Fig2.9 <- function() {
  # Fit
  Data <- read_data('pollution')
  X <- std(Data$X)
  y <- Data$y
  cvfit <- cv.ncvreg(X, y, penalty="lasso", nfolds=length(y))
  plot(cvfit, type="rsq", bty="n", xlab=expression(lambda), xaxt="n")
  at <- c(40, 4, 0.4, 0.04)
  axis(1, at=log(at), labels=at)
}
