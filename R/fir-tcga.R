# fit <- ncvreg(X, y, penalty="lasso")
Fig5.6 <- function(fit) {
  obj <- fir(fit)
  par(mar=c(5,5,5,0.5), mfrow=c(1,2))
  plot(obj, lwd=3, log.l=TRUE, bty="n")
  plot(obj, type="EF", lwd=3, log.l=TRUE, bty="n")
}
