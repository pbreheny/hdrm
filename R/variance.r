#' Reproduce Figure 1.1
#'
#' Reproduces Figure 1.1 from the book; if you specify any options, your results may look different.
#'
#' @param n     Sample size
#' @param p     Number of features
#' @param seed  Random seed
#'
#' @examples Fig1.1()
#'
#' @export

Fig1.1 <- function(n=20, p=n-1, seed=1) {
  original_seed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- original_seed)
  set.seed(seed)

  X <- std(matrix(rnorm(n*p),n,p))
  bigVar <- numeric(p)
  for (i in 1:p) {
    bigVar[i] <- max(diag(solve(crossprod(X[,1:i]))))
  }
  plot(1:p, log(20*bigVar, 2), yaxt="n", ylab="Largest variance", xlab="Number of columns included",
       pch=19, bty="n", xlim=c(0,n))
  log_axis(2, base=2)
  invisible(20*bigVar)
}
