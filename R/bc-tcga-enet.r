#' Reproduce Table 4.1
#'
#' Reproduces Table 4.1 from the book.  If you specify any options, your results may look different.
#'
#' @param seed   For reproducibility
#'
#' @examples
#' Tab4.1()
#' @export

Tab4.1 <- function(seed=1) {
  dat <- read_data('brca1')
  alpha <- seq(1, 0.25, -0.25)

  # Enet
  R <- S <- matrix(NA, 2, 4)
  fold <- ncvreg::assign_fold(dat$y, 10, seed=seed)
  for (j in 1:4) {
    cvfit <- cv.glmnet(dat$X, dat$y, alpha=alpha[j], foldid = fold)
    R[1,j] <- 1-min(cvfit$cvm)/var(dat$y)
    S[1,j] <- length(predict(cvfit, type='nonzero')[[1]])
  }

  # Mnet
  for (j in 1:4) {
    cvfit <- cv.ncvreg(dat$X, dat$y, alpha=alpha[j], foldid = fold)
    R[2,j] <- 1-min(cvfit$cve)/var(dat$y)
    S[2,j] <- predict(cvfit, type='nvars')
  }
  out <- cbind(c(R[1,], R[2,]), c(S[1,], S[2,]))
  rownames(out) <- paste(rep(c('enet', 'mnet'), each=4), alpha, sep='-')
  colnames(out) <- c('rsq', 'size')
  out
}
