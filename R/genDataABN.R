#' Simulate data according to Causal/Correlated/Noise paradigm
#'
#' @param n          Sample size
#' @param p          Number of features
#' @param a          Number of causal ('A') variables
#' @param b          Number of correlated ('B') variables per causal ('A') variable
#' @param rho        Correlation between 'A' and 'B' variables
#' @param family     Generate \code{y} according to linear "gaussian" or logistic "binomial" model
#' @param signal     Should the groups be heterogeneous (in beta) or homogeneous?
#' @param noise      Correlation structure between features ('exchangeable' | 'autoregressive')
#' @param rho.noise  Correlation parameter for noise variables
#' @param beta       Vector of regression coefficients in the generating model.  Should be either a scalar, in which case it represents the value of each nonzero regression coefficient, or a vector, in which case it should be of length \code{a}
#' @param SNR        Signal to noise ratio
#'
#' @example ex/genDataABN.R
#'
#' @export

genDataABN <- function(n=100, p=60, a=6, b=2, rho=0.5, family=c("gaussian","binomial"), signal=c('homogeneous', 'heterogeneous'), noise=c('exchangeable', 'autoregressive'),
                   rho.noise=0, beta, SNR=1) {
  family <- match.arg(family)
  noise <- match.arg(noise)
  signal <- match.arg(signal)
  K <- b + 1

  # Gen X, S
  sigmaList <- vector('list', a+1)
  for (i in 1:a) {
    sigmaList[[i]] <- matrix(rho, K, K) + (1-rho)*diag(K)
  }
  sigmaList[[a+1]] <- genS(p-K*a, rho.noise, noise)
  S <- Matrix::.bdiag(sigmaList)
  X <- genX(n, p, S)

  # Gen beta
  if (missing(beta) || length(beta)==1) {
    bb <- c(-1,1)[(1:a)%%2+1]
    if (missing(beta)) {
      if (signal=="heterogeneous") bb <- bb*(a:1)
      bbb <- numeric(p)
      bbb[((1:a)-1)*K+1] <- bb
      beta <- bbb*sqrt(SNR)/sqrt(drop(crossprod(bbb,S)%*%bbb))
    } else {
      bbb <- numeric(p)
      bbb[((1:a)-1)*K+1] <- bb
      beta <- bbb*beta
    }
  } else {
    bb <- beta
    beta <- numeric(p)
    beta[((1:a)-1)*K+1] <- bb
  }

  # Gen y
  y <- genY(X%*%beta, family=family, sigma=1)

  # Return
  varType <- vector("character", p)
  varType[((1:a)-1)*K+1] <- "A"
  for (j in 1:b) {
    varType[((1:a)-1)*K+1+j] <- "B"
  }
  varType[(a*K+1):p] <- "N"
  varLab <- vector("character", p)
  varLab[varType=="A"] <- paste0("A", 1:sum(varType=="A"))
  varLab[varType=="B"] <- paste0("B", 1:sum(varType=="B"))
  varLab[varType=="N"] <- paste0("N", 1:sum(varType=="N"))
  colnames(X) <- varLab
  names(beta) <- varLab
  list(X=X, y=y, beta=beta, family=family, varType=varType)
}
