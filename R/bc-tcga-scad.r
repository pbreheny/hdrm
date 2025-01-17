#' Reproduce Figure 3.9
#'
#' Reproduces Figure 3.9 from the book.  If you specify any options, your results may look different.
#'
#' @param gam       Gamma parameter (default: 8)
#' @param seed      For reproducibility
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig3.9()
#' @export

Fig3.9 <- function(gam=8, seed=1, parlist=list(mfrow=c(1,2))) {
  bcTCGA <- readData('bcTCGA')
  X <- bcTCGA$X
  y <- bcTCGA$y

  set.seed(seed)
  cvfit <- cv.ncvreg(X, y, penalty='SCAD', gam=gam)
  fit <- cvfit$fit
  xlim <- log(c(fit$lambda[1], cvfit$lambda.min))
  op <- par(parlist)
  plot(fit, xlim=xlim, log.l=TRUE, bty='n')
  plot(cvfit, bty='n')
  par(op)
  invisible(cvfit)
}
