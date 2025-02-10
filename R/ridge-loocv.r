#' Reproduce Figure 1.5
#'
#' Reproduces Figure 1.5 from the book. If you specify any options, your results may look different.
#'
#' @param lwd,xlim,ylim,xlab,ylab   As in `plot()`
#' @param ...                       Further arguments to `plot()`
#'
#' @examples
#' Fig1.5()
#' @export

Fig1.5 <- function(lwd=3, xlim=c(3,-3), ylim=c(800,4000), xlab=expression(lambda), ylab="Prediction error", ...) {
  dat <- read_data("pollution")
  XX <- std(dat$X)
  y <- dat$y
  fit <- ridge(XX, y)
  ll <- log10(fit$lambda)
  col <- pal(2)
  plot(ll, fit$loocv, type="l", lwd=3, xlim=xlim, las=1, xlab=xlab, xaxt="n", ylab=ylab, ylim=ylim,
       col=col[2], bty="n")
  log_axis(1, base=10)
  lines(ll, fit$rss/length(y), col=col[1], lwd=3)
  text(-3, 2000, "LOOCV", xpd=TRUE)
  text(-3.1, 1000, "RSS", xpd=TRUE)
}
