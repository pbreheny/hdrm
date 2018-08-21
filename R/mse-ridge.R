#' Reproduce Figure 1.3
#'
#' Reproduces Figure 1.3 from the book.  If you specify any options, your results may look different.
#'
#' @param rho   Correlation
#' @param n     a
#'
#' @examples
#' Fig1.3()

Fig1.3 <- function(rho=0.5, n=20) {
  lam <- c(0, exp(seq(log(0.001), log(10), length=99)))
  Y <- matrix(NA, 100, 4)
  for (i in 1:100) {
    Y[i,] <- mse_ridge_helper(rho=rho, lam[i], n=n)
  }
  matplot(lam, Y, type="l", col=c("gray50", pal(3)), lwd=3, lty=1, xlab=expression(lambda), las=1, ylab="", bty="n")
  if ((rho==0.5) & (n==20)) {
    text(5.5, 0.10, "MSE", xpd=T)
    text(9, 0.03, "Var", xpd=T)
    text(9, 0.085, "Bias", xpd=T)
  } else {
    toplegend(legend=c('MSE', 'Bias', 'Var'), lwd=3, col=pal(3))
  }
}
mse_ridge_helper <- function(rho, lam, n) {
  G <- matrix(c(1, rho, rho,1), 2, 2)

  ## OLS
  bias.ols <- rep(0, 2)
  var.ols <- solve(G)/n
  mse.ols <- sum(diag(var.ols)) + crossprod(bias.ols)

  ## ridge
  W <- solve(G + lam/n*diag(2))
  var.ridge <- W %*% G %*% W / n
  bias.ridge <- lam/n * W %*% matrix(c(1,1), 2, 1)
  mse.ridge <- sum(diag(var.ridge)) + crossprod(bias.ridge)
  val <- c(mse.ols, mse.ridge, crossprod(bias.ridge), sum(diag(var.ridge)))
  names(val) <- c("mse.ols", "mse.ridge", "bias.ridge", "var.ridge")
  val
}
