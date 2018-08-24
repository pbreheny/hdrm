#' Reproduce Figure 1.5
#'
#' Reproduces Figure 1.5 from the book.  If you specify any options, your results may look different.
#'
#' @examples
#' Fig1.5()

Fig1.5 <- function() {
  Data <- readData("pollution")
  XX <- std(Data$X)
  y <- Data$y
  fit <- ridge(XX, y)
  ll <- log10(fit$lambda)
  col <- pal(2)
  plot(ll, fit$GCV/1000, type="l", lwd=3, xlim=c(3,-3), las=1, xlab=expression(lambda), xaxt="n", ylab="Prediction error",
       col=col[2], bty="n", ylim=c(50,250))
  logAxis(1, base=10)
  lines(ll, fit$RSS/1000, col=col[1], lwd=3)
  text(-3.1, 105, "GCV", xpd=TRUE)
  text(-3.1, 65, "RSS", xpd=TRUE)
}
