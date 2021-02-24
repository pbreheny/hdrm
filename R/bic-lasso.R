#' Reproduce Figure 2.6
#'
#' Reproduces Figure 2.6 from the book; if you specify any options, your results may look different.
#'
#' @param EBIC   Include the extended BIC (EBIC, Chen & Chen 2008) in the figure?
#'
#' @examples
#' Fig2.6()
#' Fig2.6(EBIC=TRUE)
#'
#' @export

Fig2.6 <- function(EBIC=FALSE) {
  # Fit
  Data <- readData('pollution')
  X <- std(Data$X)
  y <- Data$y
  fit <- ncvreg(X, y, penalty="lasso")
  ll <- log(fit$lambda)

  # IC
  IC <- cbind(AIC(fit), BIC(fit))
  lab <- c("AIC", "BIC")
  if (EBIC) {
    p <- ncol(X)
    df <- predict(fit, type="nvars")
    EBIC <- BIC(fit) + 2*(lgamma(p+1) - lgamma(df+1) - lgamma(p-df+1))
    IC <- cbind(IC, EBIC)
    col <- pal(3)
    lab <- c(lab, 'EBIC')
  } else {
    col <- c("#FF4E37FF", "#008DFFFF")
  }

  # Plot
  matplot(ll, IC, type="l", lwd=3, lty=1, xlab=expression(lambda), xaxt="n", bty="n", xlim=rev(range(ll)), col=col, las=1, ylab="Information criterion")
  at <- c(40, 4, 0.4, 0.04)
  axis(1, at=log(at), labels=at)
  toplegend(legend=lab, lwd=3, col=col)
  set.seed(6)
  for (i in 1:length(col)) {
    abline(v=ll[which.min(IC[,i])]+runif(1, -0.03, 0.03), lty=2, lwd=2, col=col[i])
  }
}
