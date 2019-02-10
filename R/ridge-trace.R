#' Reproduce Figure 1.4
#'
#' Reproduces Figure 1.4 from the book.  If you specify any options, your results may look different.
#'
#' @param gcv.line   Draw a vertical line at the lambda value that minimizes GCV? (Default: true)
#' @param mar   Margins (passed to par)
#'
#' @examples
#' Fig1.4()

Fig1.4 <- function(gcv.line = TRUE, mar = c(5,5,5,7)) {
  op <- par(mar=mar)
  Data <- readData("pollution")
  XX <- std(Data$X)
  y <- Data$y
  fit <- ridge(XX, y)
  plot(fit, xaxis="both")
  if (gcv.line) abline(v=log10(fit$lambda[which.min(fit$GCV)]), col="gray")
  b <- coef(fit)[-1,1]
  ind <- abs(b) > 15
  text(x=log10(10^(-3.8)), y=b[ind], colnames(XX)[ind], xpd=TRUE)
  text(x=log10(10^(-3.8)), y=b["SO2"], "SO2", xpd=TRUE)
  par(op)
}
