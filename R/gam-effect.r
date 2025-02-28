#' Reproduce Figure 3.4
#'
#' Reproduces Figure 3.4 from the book; if you specify any options, your results may look different.
#'
#' @param n         Sample size
#' @param p         Number of features
#' @param seed      Random number seed for reproducibility
#' @param ylim      Vertical limits, passed to plot()
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig3.4()
#' @export

Fig3.4 <- function(n=200, p=1000, seed=105, ylim = c(-4,4), parlist=list(mfrow=c(1,3), mar=c(4, 4, 2, 0), cex=1)) {
  if (!missing(seed)) {
    original_seed <- .GlobalEnv$.Random.seed
    on.exit(.GlobalEnv$.Random.seed <- original_seed)
    set.seed(seed)
  }

  # Generate data
  X <- matrix(rnorm(n*p), nrow=n, ncol=p)
  z1 <- rnorm(n); z2 <- rnorm(n)
  X[,1:4]   <- X[,1:4]+z1
  X[,5]     <- X[,5]+2*z1
  X[,6]     <- X[,6]+1.5*z1
  X[,7:20]  <- X[,7:20]+0.5*z1
  X[,21:40] <- X[,21:40]+0.5*z2
  beta <- c(4, 2, -4,-2, rep(0, 996))
  y <- rnorm(n, X%*%beta, sd=1.5)

  op <- par(parlist)
  col <- c("#FF4E37FF", "#5FA600FF", "#00C1C9FF", "#D63EFFFF")

  # 1.2
  fit <- ncvreg(X, y, gamma=1.5)
  plot(fit, col=col, lwd=2, shade=FALSE, bty='n', ylim=ylim)
  mtext(expression(gamma==1.5), line=0.5)

  # 2.7
  fit <- ncvreg(X, y, gamma=2.7)
  plot(fit, col=col, lwd=2, shade=FALSE, bty='n', ylim=ylim)
  mtext(expression(gamma==2.7), line=0.5)

  # 6
  fit <- ncvreg(X, y, gamma=6)
  plot(fit, col=col, lwd=2, shade=FALSE, bty='n', ylim=ylim)
  mtext(expression(gamma==6), line=0.5)
}
