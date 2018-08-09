#' Reproduce Figure
#'
#' Reproduces Figure from the book.  If you specify any options, your results may look different.
#'
#' @param out   Output of Ex9.1
#' @param B     Number of bootstrap replications
#' @param seed  Random number seed for reproducibility
#'
#' @examples
#' Data <- Ex9.1()
#' res <- Fig(Data)
#' covered <- Data$beta >= res$Lower & Data$beta <= res$Upper
#' mean(covered)
#' table(Data$varType, covered)

Fig <- function(out, B=100, seed=2) {
  set.seed(seed)
  cvfit <- cv.glmnet(out$X, out$y)
  res <- boot.glmnet(out$X, out$y, lambda=cvfit$lambda.min, B=B)
  all_coef <- cbind(coef(cvfit)[-1], res)
  nz_coef <- all_coef[all_coef[,1] != 0,]
  CIplot(nz_coef, sort=FALSE, xlab=expression(beta), xlim=c(-1.1, 1.1))
  for (i in 1:nrow(nz_coef)) {
    b <- out$beta[rownames(nz_coef)[i]]
    line <- nrow(nz_coef) - i + 1
    lines(c(b,b), c(line-0.5, line+0.5), col="gray", lty=2, lwd=2, xpd=1)
  }
  colnames(all_coef) <- c('Estimate', 'Lower', 'Upper')
  return(invisible(all_coef))
}
