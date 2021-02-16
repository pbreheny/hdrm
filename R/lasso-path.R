#' Reproduce Figure 2.5
#'
#' Reproduces Figure 2.5 from the book; if you specify any options, your results may look different.
#'
#' @param cv    Carry out cross-validation?  Default: true
#' @param mar   Margins, passed to `par()`
#'
#' @examples
#' Fig2.5()
#'
#' # default glmnet plot
#' attachData(pollution)
#' fit <- glmnet(std(X), y)   # Standardize design matrix
#' plot(fit, label=TRUE)
#'
#' # Nonzero coefficients at CV-selected lambda
#' cvfit <- cv.glmnet(X, y, nfolds=length(y))
#' colnames(X)[unlist(predict(cvfit, type="nonzero"))]
#'
#' @export

Fig2.5 <- function(cv=TRUE, mar=rep(5,4)) {
  op <- par(mar=mar)
  Data <- readData('pollution')
  X <- std(Data$X)
  y <- Data$y

  if (cv) {
    cvfit <- cv.glmnet(X, y)
    fit <- cvfit$glmnet.fit
  } else {
    fit <- glmnet(X, y)
  }
  l <- fit$lambda
  b <- coef(fit)[-1,]
  nv <- sapply(predict(fit, type="nonzero"), length)
  matplot(log(l), t(b), col=pal(ncol(X)), lwd=3, bty="n", xlim=rev(range(log(l))), type="l", lty=1, xaxt="n", xlab=expression(lambda), ylab="", las=1)
  lines(log(l), rep(0, length(log(l))), lwd=3, col="gray")
  mtext(expression(hat(beta)), 2, 3, las=1)
  at <- c(40, 4, 0.4, 0.04)
  axis(1, at=log(at), labels=at)
  ind <- abs(b[,ncol(b)]) > 15
  text(x=log(min(l)/2.5), y=b[which(ind),ncol(b)], colnames(X)[ind], xpd=TRUE)
  text(x=log(min(l)/2.5), y=10, "SO2", xpd=TRUE)
  if (cv) abline(v=log(cvfit$lambda.min), col="gray")
  ind <- seq(1, length(l), length=5)
  axis(3, at=log(l)[ind], labels=round(nv[ind], 1))
  mtext("Number of nonzero coefficients", 3, 3)
  par(op)
}
