#' Reproduce Figure 2.3
#'
#' Reproduces Figure 2.3 from the book; if you specify any options, your results may look different.
#'
#' @param lambda    Regularization parameter
#' @param n         Sample size
#' @param sigma     Standard deviation of noise
#' @param ymax      What density should Pr(1) correspond to?
#'
#' @examples Fig2.3()

Fig2.3 <- function(lambda=0.5, n=10, sigma=1, ymax=1.5, ...) {
  se <- sigma/sqrt(n)
  op <- par(mar=c(5,5,1.5,0.5), mfrow=c(1,2))

  # Left side
  b  <- 0
  plot(0, 0, type="n", xlim=c(-2,2), ylim=c(0, ymax), las=1, xlab=expression(hat(beta)), ylab="Density", bty="n")
  bb <- seq(-2,0,len=49)
  polygon(c(bb, rev(bb)), c(dnorm(bb-lambda, b, se), rep(0, length(bb))), col="gray80", border=NA)
  bb <- seq(0,2,len=49)
  polygon(c(bb, rev(bb)), c(dnorm(bb+lambda, b, se), rep(0, length(bb))), col="gray80", border=NA)
  p <- pnorm((lambda-b)/se) - pnorm((-lambda-b)/se)
  lines(0, p*ymax, type="h", lwd=3)
  mtext(expression(beta[0]==0))

  # Right side
  par(mar=c(5,0.5,1.5,5))
  b  <- 1
  plot(0, 0, type="n", xlim=c(-2,2), ylim=c(0, ymax), las=1, xlab=expression(hat(beta)), ylab="", bty="n", yaxt="n")
  bb <- seq(-2,0,len=49)
  polygon(c(bb, rev(bb)), c(dnorm(bb-lambda, b, se), rep(0, length(bb))), col="gray80", border=NA)
  bb <- seq(0,2,len=49)
  polygon(c(bb, rev(bb)), c(dnorm(bb+lambda, b, se), rep(0, length(bb))), col="gray80", border=NA)
  p <- pnorm((lambda-b)/se) - pnorm((-lambda-b)/se)
  lines(0, p*ymax, type="h", lwd=3)
  axis(4, at=c(0,ymax/2,ymax), label=c(0, 0.5, 1), las=1)
  mtext("Probability", 4, line=3)
  mtext(expression(beta[0]==1))
  par(op)
}
