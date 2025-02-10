#' Reproduce Figure 1.4
#'
#' Reproduces Figure 1.4 from the book.  If you specify any options, your results may look different.
#'
#' @param loocv   Include a vertical line at the lambda value that minimizes LOOCV? (Default: true)
#' @param mar     Margins (passed to par)
#'
#' @examples
#' Fig1.4()
#' @export

Fig1.4 <- function(loocv = TRUE, mar = c(5,5,5,7)) {
  op <- par(mar=mar)
  dat <- read_data(pollution)
  XX <- std(dat$X)
  y <- dat$y
  fit <- ridge(XX, y)
  plot(fit, xaxis="both")
  if (loocv) abline(v=log10(fit$lambda[which.min(fit$loocv)]), col="gray", lwd=2, lty=2)
  b <- coef(fit)[-1,1]
  ind <- abs(b) > 15
  text(x=log10(10^(-3.8)), y=b[ind], colnames(XX)[ind], xpd=TRUE)
  text(x=log10(10^(-3.8)), y=b["SO2"], "SO2", xpd=TRUE)
  par(op)
}
