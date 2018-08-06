#' Reproduce Figure 9.3
#'
#' Reproduces Figure 9.3 from the book.  If you specify any options, your results may look different.
#'
#' @param out   Output of Ex9.1
#' @param N     Number of simulated realizations
#' @param seed  Random number seed for reproducibility
#'
#' @examples
#' out <- Ex9.1()
#' res <- Fig9.3(out)
#' covered <- out$beta >= res$Lower & out$beta <= res$Upper
#' mean(covered)
#' table(out$varType, covered)

Fig9.3 <- function(out, N=100, seed=1) {
  set.seed(seed)
  cvfit <- cv.glmnet(out$X, out$y)
  res <- boot.glmnet(out$X, out$y, lambda=cvfit$lambda.min)
  BB <- cbind(coef(cvfit)[-1], res)
  B <- BB[BB[,1] != 0,]
  CIplot(B, sort=FALSE, xlab=expression(beta), xlim=c(-1.1, 1.1))
  for (i in 1:nrow(B)) {
    b <- out$beta[rownames(B)[i]]
    line <- nrow(B) - i + 1
    lines(c(b,b), c(line-0.5, line+0.5), col="gray", lty=2, lwd=2, xpd=1)
  }
  colnames(BB) <- c('Estimate', 'Lower', 'Upper')
  return(invisible(as.data.frame(BB)))
}
