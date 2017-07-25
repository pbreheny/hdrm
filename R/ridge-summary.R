Tab1.1 <- function() {
  loadData('pollution')
  XX <- std(X)
  fit.ridge <- ridge(XX, y)
  t.ridge <- summary(fit.ridge, which=which.min(fit.ridge$GCV))[,3]
  fit.ols <- lm(y ~ ., data=as.data.frame(XX))
  t.ols <- summary(fit.ols)$coef[,3]
  out <- cbind(t.ridge, t.ols)[-1,]
  ord <- order(out[,1], decreasing=TRUE)
  out[ord,]
}