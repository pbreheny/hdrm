#' Reproduce Figure 9.4
#'
#' Reproduces Figure 9.4 from the book.  If you specify any options, your results may look different.
#'
#' @param out   Output of Ex9.1
#' @param B     Number of bootstrap replications
#' @param seed  Random number seed for reproducibility
#'
#' @examples
#' Data <- Ex9.1()
#' res <- Fig9.4(Data)
#' covered <- Data$beta >= res$Lower & Data$beta <= res$Upper
#' mean(covered)
#' table(Data$varType, covered)
#' @export

Fig9.4 <- function(out, B=100, seed=2) {
  set.seed(seed)
  res <- boot_ncvreg(out$X, out$y, nbootB=B)
  all_coef <- res$confidence_intervals
  colnames(all_coef) <- c("Coef","Lower","Upper")
  nz_coef <- all_coef[all_coef[,1] != 0,]
  ci_plot(nz_coef, sort=FALSE, xlab=expression(beta), xlim=c(-1.1, 1.1))
  for (i in 1:nrow(nz_coef)) {
    b <- out$beta[rownames(nz_coef)[i]]
    line <- nrow(nz_coef) - i + 1
    lines(c(b,b), c(line-0.5, line+0.5), col="gray", lty=2, lwd=2, xpd=1)
  }
  colnames(all_coef) <- c('Estimate', 'Lower', 'Upper')
  return(invisible(all_coef))
}
