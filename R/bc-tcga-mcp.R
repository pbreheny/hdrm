#' Reproduce Figure 3.8
#'
#' Reproduces Figure 3.8 from the book.  If you specify any options, your results may look different.
#'
#' @param seed      For reproducibility
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig3.8()
#' @export

Fig3.8 <- function(seed=1, parlist=list(mfrow=c(2,2))) {
  bcTCGA <- readData('bcTCGA')
  X <- bcTCGA$X
  y <- bcTCGA$y

  # MCP (gam=3, gam=7)
  set.seed(seed)
  cvfit3 <- cv.ncvreg(X, y)
  fit3 <- cvfit3$fit
  set.seed(seed)
  cvfit7 <- cv.ncvreg(X, y, gam=7)
  fit7 <- cvfit7$fit

  op <- par(parlist)
  xlim <- log(c(fit3$lambda[1], cvfit3$lambda.min))
  plot(fit3, xlim=xlim, log.l=TRUE, bty='n')
  plot(cvfit3, bty='n')
  xlim <- log(c(fit7$lambda[1], cvfit7$lambda.min))
  plot(fit7, xlim=xlim, log.l=TRUE, bty='n')
  plot(cvfit7, bty='n')
  par(op)
  invisible(list(cvfit3=cvfit3, cvfit7=cvfit7))
}
