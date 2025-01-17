#' Reproduce Figure 2.12
#'
#' Reproduces Figure 2.12 from the book.  If you specify any options, your results may look different.
#'
#' @param cvfit     `cv.ncvreg()` fit to the TCGA data; see examples
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' # Set up data
#' attachData(Koussounadis2014)
#' sDay <- splines::ns(sData$Day, df=2)
#' X0 <- model.matrix(~ Treatment*sDay, sData)[,-1]
#' multiplier <- rep(0:1, c(ncol(X0), ncol(X)))
#' XX <- cbind(X0, X)
#'
#' # Fit
#' fold <- c(4, 3, 10, 7, 3, 2, 5, 9, 6, 9, 7, 4, 4, 2, 1, 7, 1, 4, 6, 9, 5, 9, 9,
#'           6, 4, 8, 2, 2, 5, 5, 1, 1, 5, 7, 9, 9, 8, 7, 4, 5,  4, 2, 8, 9, 3, 1,
#'           8, 8, 10, 10, 3, 8, 3, 3, 9, 5, 1, 10, 10, 2, 7, 2, 4, 4, 8, 7, 6, 6,
#'           7, 10, 3, 6, 10, 2, 7, 5, 2, 6, 6, 1, 1, 4, 6, 10, 7, 8, 10, 8, 1, 5,
#'           5, 10, 6, 10, 3, 1, 3, 8, 2, 9, 3)
#' cvfit <- cv.ncvreg(XX, y, penalty.factor=multiplier, penalty='lasso', fold=fold)
#'
#' # Plot
#' Fig2.12(cvfit)
#'
#' @export

Fig2.12 <- function(cvfit, parlist=list(mfrow=c(1,2), mar=c(5,5,5,0.5))) {
  op <- par(parlist)
  fit <- cvfit$fit
  xlim <- log(c(fit$lambda[1], cvfit$lambda.min))
  ylim <- c(-0.3, 0.4)
  plot(fit, log=TRUE, xlab=expression(lambda), xaxt="n", bty="n", xlim=xlim, lwd=2, ylim=ylim)
  at <- seq(xlim[1], xlim[2], length=5)
  axis(1, at=at, labels=round(exp(at), 2))
  abline(v=log(cvfit$lambda.min), col="gray", lty=2, lwd=2)
  mtext("Variables selected", 3, 2.5)
  nv <- predict(fit, lam=exp(at), type="nvars")
  axis(3, at=at, labels=nv)

  ll <- log(fit$lambda)
  plot(cvfit, xlab=expression(lambda), xaxt="n", bty="n", type='rsq', selected=FALSE)
  at <- seq(max(ll), min(ll), length=5)
  axis(1, at=at, labels=round(exp(at), 2))
  mtext("Variables selected", 3, 2.5)
  nv <- predict(fit, lam=exp(at), type="nvars")
  axis(3, at=at, labels=nv)
  par(op)
}
