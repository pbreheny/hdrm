#' Simulate data for regression models
#'
#' This function is designed to scale efficiently to high dimensions, and
#' therefore imposes some restrictions. For example, correlation must be
#' positive.
#'
#' Note that if beta is not supplied, this function must calculate the SNR to
#' determine an appropriate coefficient size. This will be slow if the dimension
#' is large and beta is not sparse.
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
#' @examples
#'
#' @export

gen_data_abn <- function(n=100, p=60, a=6, b=2, rho=0.5, family=c("gaussian", "binomial"), signal=c('homogeneous', 'heterogeneous'), noise=c('exchangeable', 'autoregressive'),
                       rho.noise=0, beta, SNR=1) {

  family <- match.arg(family)
  signal <- match.arg(signal)
  noise <- match.arg(noise)
  K <- b + 1

  # Gen X
  columns <- list()
  for (i in 1:a) {
    columns[[i]] <- gen_x(n, K, rho, noise)
  }
  columns[[a + 1]] <- gen_x(n, p - a*K, rho.noise, noise)
  X <- do.call(cbind, columns)

  # Gen beta
  if (missing(beta) || length(beta)==1) {
    bb <- c(-1,1)[(1:a)%%2+1]
    if (missing(beta)) {
      if (signal=="heterogeneous") bb <- bb*(a:1)
      bbb <- numeric(p)
      bbb[((1:a)-1)*K+1] <- bb
      beta <- bbb*sqrt(SNR)/sqrt(drop(crossprod(bbb)))
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
  y <- gen_y(X%*%beta, family=family, sigma=1)


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

