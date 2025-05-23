#' Reproduce Example 4.4 and Figure 4.3
#'
#' Reproduces Example 4.4 and Figure 4.3 from the book.  If you specify any options, your results may look different.  Note that this simulation in particular is very time-consuming if run with the full N=100 replications.
#'
#' @param N       Number of simulated realizations
#' @param s       Signal strength (coefficient value for the non-null features)
#' @param n       Sample size
#' @param p       Number of features
#' @param p1      Number of non-null features
#' @param rho     Correlation between features (compound symmetric)
#' @param seed    Seed for reproducibility
#'
#' @examples
#' res <- Ex4.4(N=2, s=c(0.3, 1))
#' Fig4.3(res)
#' @export

Ex4.4 <- function(N=100, s=seq(0.1, 1.1, 0.2), n=100, p=500, p1=12, rho=0.7, seed=1) {
  original_seed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- original_seed)
  set.seed(seed)

  S <- length(s)
  alpha <- seq(1, 0, length.out=5)
  penalty <- c('lasso', 'MCP')

  res <- array(0, dim=c(N, S, 2, 6), dimnames=list(1:N, s, c('enet', 'mnet'), c(alpha, 'cv')))
  alpha[5] <- 0.001
  pb <- txtProgressBar(0, N, style=3)
  for (i in 1:N) {
    for (j in 1:S) {
      Data <- gen_data(n, p, p1, beta=s[j], rho=rho)
      pData <- gen_data(n, p, p1, beta=s[j], rho=rho)
      for (k in 1:2) {
        min_p <- numeric(5)
        for (a in 1:5) {
          fit <- with(Data, ncvreg(X, y, penalty=penalty[k], alpha=alpha[a], warn=FALSE))
          P <- predict(fit, X=pData$X)
          PE <- apply(pData$y-P, 2, crossprod)
          res[i,j,k,a] <- crossprod(Data$beta - coef(fit, which=which.min(PE))[-1])
          min_p[a] <- min(PE)
        }
        res[i,j,k,6] <- res[i,j,k,which.min(min_p)]
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  res
}

#' @rdname Ex4.4
#'
#' @param res       Output from Ex4.4
#' @param parlist   List of arguments to pass to `par()`
#'
#' @export

Fig4.3 <- function(res, parlist=list(mfrow=c(1,2), mar=c(4,4,2,0.5))) {
  col1 <- c('gray', pal(5))
  col2 <- c("gray", pal(3))
  op <- par(parlist)

  # Left
  rMSE <- apply(res, 2:4, median)
  rMSE <- cbind(rMSE[, 'enet', '1'], rMSE[,'mnet',1:5])/rMSE[,'enet','1']
  s <- as.numeric(rownames(rMSE))
  matplot(s, rMSE, col=col1, lwd=3, type='l', lty=1, xlab='Signal (s)', ylab='Relative MSE', bty='n', las=1, ylim=c(0, 3))
  leg <- c("Lasso","MCP",expression(alpha==0.75), expression(alpha==0.5), expression(alpha==0.25), expression(alpha==0.0))
  toplegend(legend=leg, horiz=FALSE, ncol=3, col=col1, lwd=3)

  # Right
  rMSE <- apply(res, 2:4, median)
  rMSE <- cbind(rMSE[,,'1'], rMSE[,,'cv'])/rMSE[,'enet','1']
  s <- as.numeric(rownames(rMSE))
  matplot(s, rMSE, col=col2, lwd=3, type='l', lty=1, xlab='Signal (s)', ylab='Relative MSE', bty='n', las=1, ylim=c(0, 1.5))
  toplegend(legend=c("Lasso","MCP","Enet","Mnet"), col=col2, lwd=3, horiz=FALSE, ncol=2)
  par(op)
}
