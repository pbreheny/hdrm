Fig1.4 <- function() {
  if (par('mar')[4] < 5) message("Suggested margin: par(mar=c(5,5,5,7))")
  loadData("pollution")
  XX <- std(X)
  fit <- ridge(XX, y)
  plot(fit, xaxis="both")
  abline(v=log10(fit$lambda[which.min(fit$GCV)]), col="gray")
  b <- coef(fit)[-1,1]
  ind <- abs(b) > 15
  text(x=log10(10^(-3.8)), y=b[ind], colnames(X)[ind], xpd=TRUE)
  text(x=log10(10^(-3.8)), y=b["SO2"], "SO2", xpd=TRUE)
}
