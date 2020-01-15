#' Reproduce Table 4.1
#'
#' Reproduces Table 4.1 from the book.  If you specify any options, your results may look different.
#'
#' @param seed   For reproducibility
#'
#' @examples Tab4.1()
#'
#' @export

Tab4.1 <- function(seed=1) {
  Data <- readData('bcTCGA')
  alpha <- seq(1, 0.25, -0.25)

  # Enet
  R <- S <- matrix(NA, 2, 4)
  for (j in 1:4) {
    set.seed(seed)
    cvfit <- with(Data, cv.glmnet(X, y, alpha=alpha[j]))
    R[1,j] <- 1-min(cvfit$cvm)/var(Data$y)
    S[1,j] <- length(predict(cvfit, type='nonzero')[[1]])
  }

  # Mnet
  for (j in 1:4) {
    cvfit <- with(Data, cv.ncvreg(X, y, alpha=alpha[j], seed=seed))
    R[2,j] <- 1-min(cvfit$cve)/var(Data$y)
    S[2,j] <- predict(cvfit, type='nvars')
  }
  out <- cbind(c(R[1,], R[2,]), c(S[1,], S[2,]))
  rownames(out) <- paste(rep(c('enet', 'mnet'), each=4), alpha, sep='-')
  colnames(out) <- c('rsq', 'size')
  out
}
