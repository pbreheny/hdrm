#' Simulate data according to Causal/Correlated/Noise paradigm
#'
#' This function is designed to scale efficiently to high dimensions, and
#' therefore imposes some restrictions. For example, correlation must be
#' positive.
#'
#' Note that if beta is not supplied, this function must calculate the SNR to
#' determine an appropriate coefficient size. This will be slow if the dimension
#' is large and beta is not sparse.
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
#' @examples
#' Data <- gen_data_abn(n=100, p=20, a=2, b=3)
#' expect_equal(dim(Data$X), c(100, 20))
#' expect_equal(length(Data$y), 100)
#' expect_equal(Data$varType[1:8], rep(c('A', 'B', 'B', 'B'), 2))
#' with(Data, data.frame(beta, varType))
#
#' gen_data_abn(100, 10, 2, 1)$beta
#' gen_data_abn(100, 10, 2, 1, rho=0.9)$beta
#' gen_data_abn(100, 10, 2, 1, rho=0.9, rho.noise=0.9)$beta
#' gen_data_abn(100, 10, 2, 1, SNR=3)$beta
#' gen_data_abn(100, 10, 2, 1, SNR=3, signal='het')$beta
#' gen_data_abn(100, 10, 2, 1, beta=3)$beta
#' gen_data_abn(100, 10, 2, 1, beta=2:1)$beta
#'
#' gen_data_abn(10, 20, 2, 3, family='binomial')$y
#'
#' gen_data_abn(1000, 10, 2, 2, rho=0.25, rho.noise=0.0, noise='exch')$X |> cor() |> round(digits=2)
#' gen_data_abn(1000, 10, 2, 2, rho=0.5, rho.noise=0.5, noise='exch')$X |> cor() |> round(digits=2)
#' gen_data_abn(1000, 10, 2, 2, rho=0.75, rho.noise=0.9, noise='auto')$X |> cor() |> round(digits=2)
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
  } else if (length(beta) == a) {
    bb <- beta
    beta <- numeric(p)
    beta[((1:a)-1)*K+1] <- bb
  } else if (length(beta) == p) {
    if (sum(beta[!(((1:a)-1)*K+1)]) != 0 & sum(beta[!(((1:a)-1)*K+1)]) != 0 & rho != 0) {
      warning("User supplied all beta values, and there is correlation between causal variables. Make sure this is what you expected")
    }
  }

  # Gen y
  y <- gen_y(X%*%beta, family=family, sigma=1)


  # Return
  varType <- vector("character", p)
  varType[((1:a)-1)*K+1] <- "A"
  if (b > 0) {
    for (j in 1:b) {
      varType[((1:a)-1)*K+1+j] <- "B"
    }
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

