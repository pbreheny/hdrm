#' Reproduce Example 3.1
#'
#' Reproduces Example 3.1 from the book.  If you specify any options, your results may look different.
#'
#' @param N      Number of simulated realizations
#' @param n      Sample size
#' @param p      Number of features
#' @param p1     Number of non-null features
#' @param SNR    Vector of SNR values
#' @param seed   Seed for reproducibility
#'
#' @return A list of two arrays, `MCP`, containing the MSE for MCP, and `SCAD`, containing the MSE for SCAD.
#'
#' @examples Ex3.1(N=2)
#'
#' @export

Ex3.1 <- function(N=500, n=50, p=100, p1=6, SNR=c(1, 2, 4), seed=2) {
  set.seed(seed)
  gam <- 2^seq(0.5, 5, len=19)

  mMSE <- sMSE <- array(NA, dim=c(N, length(SNR), length(gam)), dimnames=list(1:N, SNR, gam))
  pb <- txtProgressBar(0, N, style=3)
  for (i in 1:N) {
    for (j in 1:length(SNR)) {
      Data <- gen_data(n, p, p1, SNR=SNR[j])
      pData <- gen_data(n, p, p1, SNR=SNR[j])
      for (k in 1:length(gam)) {
        fit <- with(Data, ncvreg(X, y, gamma=gam[k]))
        pred <- predict(fit, pData$X)
        lam <- fit$lambda[which.min(apply(pred-pData$y, 2, crossprod))]
        mMSE[i,j,k] <- crossprod(Data$beta - coef(fit, lambda=lam)[-1])

        fit <- with(Data, ncvreg(X, y, gamma=gam[k]+1, penalty='SCAD'))
        pred <- predict(fit, pData$X)
        lam <- fit$lambda[which.min(apply(pred-pData$y, 2, crossprod))]
        sMSE[i,j,k] <- crossprod(Data$beta - coef(fit, lambda=lam)[-1])
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  list(MCP=mMSE, SCAD=sMSE)
}

#' Reproduce Figure 3.5
#'
#' Reproduces Figure 3.5 from the book; if you specify any options, your results may look different.
#'
#' @param out       Output of Ex3.1()
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' out <- Ex3.1(N=3)
#' Fig3.5(out)
#' @export

Fig3.5 <- function(out, parlist=list(mfrow=c(1,2), mar=c(5,5,1,0.5), oma=c(0,0,3,0))) {

  if (missing(out)) stop("You need to run the code in Ex3.1() first and pass it to Fig3.5()")
  mMSE <- out$MCP
  sMSE <- out$SCAD
  SNR <- as.numeric(dimnames(mMSE)[[2]])
  gam <- as.numeric(dimnames(mMSE)[[3]])
  col <- pal(dim(mMSE)[2])

  op <- par(parlist)
  matplot(log2(gam), t(apply(mMSE, 2:3, mean)), type='l', xaxt='n', lwd=3, lty=1, col=col,
          xlab=expression(gamma), ylab="Mean squared error", las=1, bty='n')
  axis(1, at=1:5, labels=2^(1:5))
  mtext('MCP')
  matplot(log2(gam), t(apply(sMSE, 2:3, mean)), type='l', xaxt='n', lwd=3, lty=1, col=col,
          xlab=expression(gamma), ylab="Mean squared error", las=1, bty='n')
  axis(1, at=1:5, labels=2^(1:5)+1)
  mtext('SCAD')
  par(mfrow=c(1,2), mar=c(5,5,1,0.5), oma=c(0,0,3,0))
  toplegend(legend=c("SNR: ", SNR), lwd=3, col=c('white',col))
  par(op)
}
