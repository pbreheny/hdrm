#' Reproduce Figure 2.2
#'
#' Reproduces Figure 2.2 from the book; if you specify any options, your results may look different.
#'
#' @param range      Range for beta coefficient (vector of length 2)
#' @param col        Lasso/ridge color (vector of length 2)
#'
#' @examples Fig2.2()

Fig2.2 <- function(range=c(-2.5,2.5), col=c("#FF4E37FF", "#008DFFFF")) {
  xx <- seq(range[1], range[2], len=99)
  plot(xx, xx, lwd=3, type="l", col="gray", xlab="z", ylab=expression(hat(beta)(z)), bty="n", las=1)
  lines(xx, xx*(abs(xx) > 1), lwd=3, col=col[1])
  lines(xx, (xx - sign(xx))*(abs(xx) > 1), lwd=3, col=col[2])
  toplegend(legend=c("Hard", "Soft"), lwd=3, col=col)
}
