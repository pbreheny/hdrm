#' Reproduce Table 4.2
#'
#' Reproduces Table 4.2 from the book.  If you specify any options, your results may look different.
#'
#' @param p      Restrict analysis to the `p` genes with the largest variances in expression
#' @param seed   For reproducibility
#'
#' @examples Tab4.2()
#'
#' @export

Tab4.2 <- function(p=5000, seed=12) {
  Data <- read_data('Scheetz2006')
  alpha <- seq(1, 0.25, -0.25)

  # Restrict to n w/ highest variance
  p <- 5000
  ind <- order(apply(Data$X, 2, var), decreasing=TRUE)[1:p]
  names(ind) <- colnames(Data$X)[ind]
  X <- Data$X[,ind]
  y <- Data$y

  # Enet
  fold <- ncvreg::assign_fold(y, 10, seed=seed)
  R <- S <- matrix(NA, 2, 4)
  for (j in 1:4) {
    cvfit <- cv.glmnet(X, y, alpha=alpha[j], fold=fold)
    R[1,j] <- 1-min(cvfit$cvm)/var(y)
    S[1,j] <- length(predict(cvfit, type='nonzero')[[1]])
  }

  # Mnet
  for (j in 1:4) {
    cvfit <- cv.ncvreg(X, y, alpha=alpha[j], fold=fold)
    R[2,j] <- 1-min(cvfit$cve)/var(y)
    S[2,j] <- predict(cvfit, type='nvars')
  }
  out <- cbind(c(R[1,], R[2,]), c(S[1,], S[2,]))
  rownames(out) <- paste(rep(c('enet', 'mnet'), each=4), alpha, sep='-')
  colnames(out) <- c('rsq', 'size')
  out
}
