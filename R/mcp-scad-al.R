#' Reproduce Figure 3.2
#'
#' Reproduces Figure 3.2 from the book; if you specify any options, your results may look different.
#'
#' @param n         Sample size
#' @param p         Number of features
#' @param seed      Random number seed for reproducibility
#' @param ylim      Vertical limits, passed to plot()
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig3.2()

Fig3.2 <- function(n=200, p=1000, seed=105, ylim=c(-4.1,4.1), parlist=list(mfrow=c(2,2), mar=c(4.5, 4.5, 3, 0.5))) {
  if (p < 40) stop('p must be at least 40')

  # Gen data
  X <- matrix(rnorm(n*p), nrow=n, ncol=p)
  z1 <- rnorm(n); z2 <- rnorm(n)
  X[,1:4]   <- X[,1:4]+z1
  X[,5]     <- X[,5]+2*z1
  X[,6]     <- X[,6]+1.5*z1
  X[,7:20]  <- X[,7:20]+0.5*z1
  X[,21:40] <- X[,21:40]+0.5*z2
  beta <- c(4, 2, -4,-2, rep(0, p-4))
  y <- rnorm(n, X%*%beta, sd=1.5)

  op <- par(parlist)

  # MCP
  fit <- ncvreg(X, y, gamma=3)
  plot(fit, col=pal(4), lwd=2, shade=FALSE, bty='n', ylim=ylim)
  mtext('MCP', line=0.5)

  # SCAD
  fit <- ncvreg(X, y, gamma=4, penalty='SCAD')
  plot(fit, col=pal(4), lwd=2, shade=FALSE, bty='n', ylim=ylim)
  mtext('SCAD', line=0.5)

  # Lasso
  fit <- ncvreg(X, y, penalty='lasso', lambda.min=0.002)
  plot(fit, col=pal(4), lwd=2, shade=FALSE, bty='n', ylim=ylim)
  mtext('Lasso', line=0.5)

  # Adaptive lasso
  fit <- ncvreg(X, y, penalty='lasso', lambda.min=0.002)
  beta <- fit$beta
  L <- length(fit$lambda)
  for (l in 1:L) {
    w <- pmin(1e6, 1/abs(fit$beta[-1,l]))
    fit.al <- ncvreg(X, y, lambda=fit$lambda[1:l], penalty='lasso', penalty.factor=w)
    beta[,l] <- fit.al$beta[,l]
  }
  fit$beta <- beta
  nz <- which(apply(beta[-1,], 2, function(x) any(x!=0)))
  xlim <- c(fit$lambda[nz[1]-1], fit$lambda[100])
  plot(fit, col=pal(4), lwd=2, shade=FALSE, xlim=xlim, bty='n', ylim=ylim)
  mtext('Adaptive lasso', line=0.5)
}
