f <- function(rho, lam, n) {
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

## Fig() here plus more
##lam <- c(0, exp(seq(log(0.001), log(0.1), length=19)), exp(seq(log(0.1), log(10), length=80)))
lam <- c(0, exp(seq(log(0.001), log(10), length=99)))
Y <- matrix(NA, 100, 4)
for (i in 1:100) {
  Y[i,] <- f(0.5, lam[i], 20)
}
pdf("mse-ridge.pdf", 4, 3.5)
par(mar=c(5,4,0.1,5))
matplot(lam, Y, type="l", col=c("gray50", pal(3)), lwd=3, lty=1, xlab=expression(lambda), las=1, ylab="", bty="n")
#text(3, 0.145, "MSE (OLS)")
text(5.5, 0.10, "MSE", xpd=T)
text(9, 0.03, "Var", xpd=T)
text(9, 0.085, "Bias", xpd=T)
dev.off()

## Bivariate example
x1 <- rnorm(20)
x2 <- rnorm(20, mean=x1, sd=.01)
v <- f(cor(x1, x2), 1, 20)
v[1]/v[2]
