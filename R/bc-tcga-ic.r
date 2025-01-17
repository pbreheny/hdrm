#' Reproduce Figure 2.11
#'
#' Reproduces Figure 2.11 from the book.  If you specify any options, your results may look different.
#'
#' @param fit     `ncvreg()` fit to the TCGA data; see examples
#'
#' @examples
#' attachData(bcTCGA)
#' fit <- ncvreg(X, y, penalty='lasso', lambda.min=0.005)
#' Fig2.11(fit)
#'
#' @export

Fig2.11 <- function(fit) {
  ll <- log(fit$lambda)
  IC <- cbind(AIC(fit), BIC(fit))
  matplot(ll, IC, xlim=rev(range(ll)), col=pal(2), type='l', lwd=3, lty=1, bty='n',
          xlab=expression(lambda), xaxt='n', las=1, ylab="AIC/BIC")
  at <- seq(max(ll), min(ll), length=5)
  axis(1, at=at, labels=round(exp(at), 2))
  toplegend(legend=c("AIC", "BIC"), col=pal(2), lwd=3)
}
