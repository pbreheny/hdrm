#' Reproduce Example 4.2
#'
#' Reproduces Example 4.2 from the book.  If you specify any options, your results may look different.
#'
#' @param N       Number of simulated realizations
#' @param rho     Vector of correlations
#' @param n       Sample size
#' @param p       Number of features
#' @param p1      Number of non-null features
#' @param b       Coefficient associated with the non-null features
#' @param alpha   Elastic net tuning parameter
#' @param corr    Correlation structure, either `cs` (compound symmetric) or `bd` (block diagonal)
#' @param seed    Seed for reproducibility
#'
#' @examples Ex4.2(N=10)
#'
#' @export

Ex4.2 <- function(N=1000, rho=seq(0, 0.9, 0.1), n=50, p=100, p1=5, b=0.5, alpha=0.5, corr=c('cs', 'bd'), seed=1) {
  set.seed(seed)
  corr <- match.arg(corr)
  R <- length(rho)
  alpha <- c(1, alpha)
  beta <- c(rep(b, p1), rep(0, 100-p1))

  res <- array(0, dim=c(N, R, 2), dimnames=list(1:N, rho, c("lasso", "enet")))
  pb <- txtProgressBar(0, N, style=3)
  for (i in 1:N) {
    for (j in 1:R) {
      if (corr == 'cs') {
        Data <- genData(n, p, rho=rho[j], beta=beta)
        pData <- genData(n, p, rho=rho[j], beta=beta)
      } else {
        Data <- genDataGrp(n, J=20, K=5, rho.g=rho[j], beta=beta)
        pData <- genDataGrp(n, J=20, K=5, rho.g=rho[j], beta=beta)
      }
      for (k in 1:2) {
        fit <- with(Data, glmnet(X, y, alpha=alpha[k]))
        P <- predict(fit, newx=pData$X)
        ind <- which.min(apply(pData$y-P, 2, crossprod))
        res[i,j,k] <- crossprod(beta - fit$beta[,ind])
      }
    }
    setTxtProgressBar(pb, i)
  }
  res
}

#' Reproduce Figure 4.1
#'
#' Reproduces Figure 4.1 from the book.  If you specify any options, your results may look different.
#'
#' @param cs        Output from Ex4.2 with compound symmetric structure
#' @param bd        Output from Ex4.2 with block diagonal structure
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' cs <- Ex4.2(N=5, corr='cs')
#' bd <- Ex4.2(N=5, corr='bd')
#' Fig4.1(cs, bd)

Fig4.1 <- function(cs, bd, parlist=list(mfrow=c(1,2), mar=c(5,5,0.5,0.5), oma=c(0,0,2,0))) {
  labs <- c("Lasso", "Elastic Net")
  op <- par(parlist)
  a <- apply(cs, 2:3, mean)
  b <- apply(bd, 2:3, mean)
  matplot(as.numeric(rownames(a)), a, type='l', lty=1, lwd=3, col=pal(2), bty='n', las=1, xlab=expression(rho), ylab="MSE")
  matplot(as.numeric(rownames(b)), b, type='l', lty=1, lwd=3, col=pal(2), bty='n', las=1, xlab=expression(rho), ylab="MSE")
  toplegend(legend=labs, lwd=3, col=pal(2))
  par(op)
}
