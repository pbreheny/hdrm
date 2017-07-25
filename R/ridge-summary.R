Tab1.1 <- function() {
  loadData('pollution')
  XX <- std(X)
  fit.ridge <- ridge(XX, y)
  t.ridge <- summary(fit.ridge, which=which.min(fit$GCV))[,3]
  fit.ols <- lm(y ~ ., data=as.data.frame(XX))
  t.ols <- summary(fit.ols)$coef[,3]
  out <- cbind(t.ridge, t.ols)[-1,]
  ord <- order(out[,1], decreasing=TRUE)
  out[ord,]

  ind <- which.min(fit$GCV)
  l <- lam[ind]
  W <- solve(crossprod(X)/n+l*diag(ncol(X)))
  b <- coef(fit)[ind,-1]
  s2 <- RSS[ind]/(n-df[ind])
  V <- s2/n*W
  z <- b/sqrt(diag(V))
  ord <- order(z, decreasing=TRUE)
  fit.ols <- lm(y-mean(y)~0+., data=as.data.frame(X))
  z.ols <- summary(fit.ols)$coef[,3]
  Tab <- cbind(z, z.ols)[ord,]
  rownames(Tab) <- colnames(X)[ord]
  Tab
  require(xtable)
  xtable(Tab)

}