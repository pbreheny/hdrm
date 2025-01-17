#' Simulate data for regression models
#'
#' @param n      Sample size
#' @param p      Number of features
#' @param p1     Number of nonzero features
#' @param beta   Vector of regression coefficients in the generating model, or, if a scalar, the value of each nonzero regression coefficient.
#' @param family Generate \code{y} according to linear "gaussian" or logistic "binomial" model
#' @param SNR    Signal to noise ratio
#' @param signal Should the beta coefficients be homogeneous (default) or heterogeneous
#' @param corr   Correlation structure between features ('exchangeable' | 'autoregressive')
#' @param rho    Correlation coefficient
#'
#' @example ex/genData.R
#'
#' @export

genData <- function(n, p, p1=floor(p/2), beta, family=c("gaussian","binomial"), SNR=1,
                    signal = c("homogeneous","heterogeneous"), corr=c("exchangeable", "autoregressive"),
                    rho = 0) {
  family <- match.arg(family)
  signal <- match.arg(signal)
  corr <- match.arg(corr)

  # Gen X, S
  S <- genS(p, rho, corr)
  X <- genX(n, p, S)

  # Gen beta
  if (missing(beta) || length(beta)==1) {
    j <- 1:p
    s <- c(-1,1)[j%%2+1]
    b <- (j <= p1)
    if (missing(beta)) {
      if (signal=="heterogeneous") b <- b*rev(j)
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(Matrix::drop(Matrix::crossprod(b,S) %*% b))
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

genS <- function(p, rho, corr) {
  if (corr=='exchangeable') {
    S <- matrix(rho, p, p) + (1-rho)*diag(p)
  } else if (corr=='autoregressive') {
    RHO <- matrix(rho^(0:(p-1)), p, p, byrow=TRUE)
    S <- Matrix::bandSparse(p, k=0:(p-1), diagonals=RHO, symmetric=TRUE)
  }
  S
}
genX <- function(n, p, S) {
  R <- chol(S)
  as.matrix(matrix(rnorm(n*p), n, p) %*% R)
}

genY <- function(eta,family=c("gaussian","binomial"),sigma=1) {
  family=match.arg(family)
  n <- length(eta)
  if (family=="gaussian") y <- rnorm(n,mean=eta,sd=sigma)
  else if (family=="binomial")
  {
    pi. <- exp(eta)/(1+exp(eta))
    pi.[eta > log(.9999/.0001)] <- 1
    pi.[eta < log(.0001/.9999)] <- 0
    y <- rbinom(n,1,pi.)
  }
  return(y)
}
