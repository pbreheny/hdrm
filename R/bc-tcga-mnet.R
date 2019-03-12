#' Reproduce Figure 4.4
#'
#' Reproduces Figure 4.4 from the book.  If you specify any options, your results may look different.
#'
#' @param alpha     Vector of alpha values for MNet penalty; there will be one plot per alpha value
#' @param ylim      Vertical limits of plots
#' @param seed      For reproducibility
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' Fig4.4()

Fig4.4 <- function(alpha=c(0.75, 0.5, 0.25), ylim=c(-0.25, 0.5), seed=1, parlist=list(par(mfrow=c(1,3), mar=c(4, 4, 2, 0), cex=1))) {
  bcTCGA <- readData('bcTCGA')
  X <- bcTCGA$X
  y <- bcTCGA$y

  op <- par(parlist)
  for (a in alpha) {
    cvfit <- cv.ncvreg(X, y, alpha=a, seed=seed)
    plot(cvfit$fit, bty='n', ylim=ylim)
    mtext(bquote(alpha==.(a)), line=0.5)
    abline(v=cvfit$lambda.min, lty=2, lwd=2)
  }
  par(op)
}
