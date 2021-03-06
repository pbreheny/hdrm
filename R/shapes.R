#' Reproduce Figure 2.1
#'
#' Reproduces Figure 2.1 from the book; if you specify any options, your results may look different.
#'
#' @param range     Range for beta coefficient (vector of length 2)
#' @param col       Lasso/ridge color (vector of length 2)
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig2.1()
#'
#' @export

Fig2.1 <- function(range=c(-5,5), col=c("#FF4E37FF", "#008DFFFF"), parlist=list(mfrow=c(1,2), mar=c(4, 4, 0.5, 0.5), mgp=c(2, 1, 0), oma=c(0,0,2,0))) {
  op <- par(parlist)
  res <- 99
  x <- seq(range[1], range[2], len=res)
  xx <- seq(0, range[2],len=res)

  matplot(x, cbind(Lasso(x, 1), Ridge(x, 0.3)), type="l", lwd=3, lty=1, col=col,
          xlab=expression(beta), ylab=expression(P(beta)), xaxt="n", yaxt="n", bty="l")
  matplot(xx, cbind(dLasso(xx, 1), dRidge(xx, 0.3)), type="l", lwd=3, lty=1, col=col,
          xlab=expression(abs(beta)), ylab=expression(P*"'"(abs(beta))), xaxt="n", yaxt="n", bty="l")
  axis(1, at=0, labels=0)
  axis(2, at=c(0,1), labels=c(0, expression(lambda)), las=1)
  toplegend(legend=c("Lasso", "Ridge"), lwd=3, col=col)
  par(op)
}
