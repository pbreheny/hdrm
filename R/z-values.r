#' Reproduce Example 6.1 and Figure 6.1
#'
#' Reproduces Example 6.1 and Figure 6.1 from the book. If you specify any options, your results may look different.
#'
#' @examples
#' out <- Ex6.1()
#' Fig6.1(out)
#' @export

Ex6.1 <- function() {
  Data <- read_data("Golub1999")
  X <- Data$X
  y <- Data$y
  summ <- summary(lm(X~y))
  tstat <- sapply(summ, function(s) s$coef[2,3])
  p <- sapply(summ, function(s) s$coef[2,4])
  z <- qnorm(p/2) * sign(-tstat)
  out <- list(p=p, z=z)
}

#' @rdname Ex6.1
#'
#' @param out       Output of Ex6.1()
#'
#' @export

Fig6.1 <- function(out) {
  zz <- seq(-10, 10, len=299)
  h <- hist(out$z, breaks=seq(min(zz), max(zz), length = 99), plot=FALSE)
  f0 <- function(z) dnorm(z, 0, 1)
  ylim=c(0, max(c(h$density, f0(0))))
  plot(h, main="", border=FALSE, col="lightgray", freq=FALSE, las=1, ylim=ylim)
  lines(zz, f0(zz), col=pal(2)[2], lwd=2)
}
