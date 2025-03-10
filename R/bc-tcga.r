#' Reproduce Figure 2.10
#'
#' Reproduces Figure 2.10 from the book.  If you specify any options, your results may look different.
#'
#' @param cvfit     `glmnet()` fit to the TCGA data; see examples
#' @param which     `left`, `right`, or `both`
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' attach_data(brca1)
#' cvfit <- cv.glmnet(X, y)
#' Fig2.10(cvfit)
#' Fig2.10(cvfit, 'left')
#' Fig2.10(cvfit, 'right')
#' @export

Fig2.10 <- function(cvfit, which = c('both', 'left', 'right'), parlist) {
  which <- match.arg(which)
  if (missing(parlist)) {
    if (which == 'both') {
      parlist <- list(mfrow=c(1,2), mar=c(5,5,5,0.5))
    } else {
      parlist <- list(mar=c(5,5,5,0.5))
    }
  }
  fit <- cvfit$glmnet.fit
  op <- par(parlist)
  if (which == 'both' | which == 'left') {
    xlim <- log(c(fit$lambda[1], cvfit$lambda.min))
    nv <- sapply(predict(fit, type="nonzero"), length)[48]
    plot(fit, xvar="lambda", las=1, xlab=expression(lambda), xaxt="n", bty="n",
         xlim=xlim, col=pal(nv), lwd=2)
    at <- seq(xlim[1], xlim[2], length=5)
    axis(1, at=at, labels=round(exp(at), 2))
    abline(v=log(cvfit$lambda.min), col="gray", lty=2, lwd=2)
    abline(v=log(cvfit$lambda.1se), col="gray", lty=2, lwd=2)
    mtext("Variables selected", 3, 2.5)
  }
  if (which == 'both' | which == 'right') {
    ll <- log(fit$lambda)
    plot(cvfit, las=1, xlab=expression(lambda), xaxt="n", bty="n", xlim=rev(range(ll)), ylab=expression(CV(lambda)))
    at <- seq(max(ll), min(ll), length=5)
    axis(1, at=at, labels=round(exp(at), 2))
    mtext("Variables selected", 3, 2.5)
  }
  par(op)
}
