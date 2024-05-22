#' Simulate data for regression models in which the design matrix X is orthonormal
#'
#' @param n      Sample size
#' @param p      Number of features
#' @param p1     Number of nonzero features
#' @param beta   Vector of regression coefficients in the generating model, or, if a scalar, the value of each nonzero regression coefficient.
#' @param family Generate `y according to linear "gaussian" or logistic "binomial" model
#' @param SNR    Signal to noise ratio
#' @param signal Should the beta coefficients be homogeneous (default) or heterogeneous
#'
#' @examples
#' Data <- gen_ortho(5, 2)
#' Data$X
#' crossprod(Data$X)
#' @export

gen_ortho <- function(n, p, p1=floor(p/2), beta, family=c("gaussian","binomial"), SNR=1,
                    signal = c("homogeneous","heterogeneous")) {
  family <- match.arg(family)
  signal <- match.arg(signal)
  if (p > n) stop("Cannot generate orthonormal design matrix if p is larger than n")

  # Gen X
  X <- qr.Q(qr(matrix(rnorm(n*p), n, p)))

  # Gen beta
  if (missing(beta) || length(beta)==1) {
    j <- 1:p
    s <- c(-1,1)[j%%2+1]
    b <- (j <= p1)
    if (missing(beta)) {
      if (signal=="heterogeneous") b <- b*rev(j)
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(drop(crossprod(b)))
    } else {
      beta <- b*s*beta
    }
  }

  # Gen y
  y <- genY(X%*%beta, family=family, sigma=1)

  # Label and return
  w <- 1 + floor(log10(p))
  vlab <- paste0('V', formatC(1:p, format='d', width=w, flag='0'))
  colnames(X) <- names(beta) <- vlab
  list(X=X, y=y, beta=beta, family=family)
}
