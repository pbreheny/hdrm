#' Reproduce Figure 2.7
#'
#' Reproduces Figure 2.7 from the book; if you specify any options, your results may look different.
#'
#' @examples
#' Fig2.7()
#'
#' # Default plot
#' attach_data(pollution)
#' cvfit <- cv.glmnet(std(X), y)   # Standardize design matrix
#' plot(cvfit)
#'
#' # Range of lambda values within 1 SE of minimum
#' lmin <- which(cvfit$lambda==cvfit$lambda.min)
#' range(cvfit$lambda[which(cvfit$cvm < cvfit$cvup[lmin])])
#'
#' @export

Fig2.7 <- function() {
  # Fit
  Data <- read_data('pollution')
  X <- std(Data$X)
  y <- Data$y
  cvfit <- cv.glmnet(X, y)
  l <- cvfit$lambda

  # Plot
  plot(cvfit, xlim=c(rev(range(log(l)))), xlab=expression(log(lambda)), las=1, bty="n", xaxt="n", ylab=expression(CV(lambda)))
  at <- c(40, 4, 0.4, 0.04)
  axis(1, at=log(at), labels=at)
  mtext("Number of nonzero coefficients", 3, 3)
}
