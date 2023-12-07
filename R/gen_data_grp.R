#' Simulate grouped data for regression models
#'
#' @param n         Sample size
#' @param J         Number of groups
#' @param K         Number of features per group
#' @param beta      Vector of regression coefficients in the generating model, or, if a scalar, the value of each nonzero regression coefficient
#' @param family    Generate \code{y} according to linear "gaussian" or logistic "binomial" model
#' @param J1        Number of nonzero groups
#' @param K1        Number of nonzero coefficients per group
#' @param SNR       Signal to noise ratio
#' @param signal    Should the groups be heterogeneous (in beta) or homogeneous?
#' @param signal.g  Should the coefficients within a group be heterogeneous or homogeneous?
#' @param rho       Correlation between groups
#' @param rho.g     Correlation between parameters within a group
#'
#' @example ex/genDataGrp.R
#'
#' @export

gen_data_grp <- function(n, J, K=1, beta, family=c("gaussian","binomial"), J1=ceiling(J/2), K1=K, SNR=1, signal = c("homogeneous","heterogeneous"),
                       signal.g = c("homogeneous","heterogeneous"), rho = 0, rho.g = rho) {
  family <- match.arg(family)
  signal <- match.arg(signal)
  signal.g <- match.arg(signal.g)

  # Gen X
  columns <- list()
  common_factor <- sqrt(rho)*rnorm(n)
  for (i in 1:J) {
    z <- rnorm(n)
    columns[[i]] <- common_factor + sqrt(rho.g - rho)*rnorm(n) + sqrt(1-rho.g) * matrix(rnorm(n*K), n, K)
  }
  X <- do.call(cbind, columns)


  # Gen beta
  j <- rep(1:J, rep(K,J))
  k <- rep(1:K,J)
  if (missing(beta) || length(beta)==1) {
    b <- (j <= J1) * (k <= K1)
    s <- c(1,-1)[1+j%%2] * c(1,-1)[1+k%%2]
    if (missing(beta)) {
      if (signal=="heterogeneous") b <- b*j
      if (signal.g=="heterogeneous") b <- b*k
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(drop(crossprod(b)))
    } else {
      beta <- b*s*beta
    }
  }

  # Gen y
  y <- gen_y(X%*%beta, family=family, sigma=1)

  # Label and return
  gw <- 1 + floor(log10(J))
  glab <- paste0('G', formatC(j, format='d', width=gw, flag='0'))
  vw <- 1 + floor(log10(K))
  vlab <- paste0('V', formatC(k, format='d', width=vw, flag='0'))
  lab <- paste(glab, vlab, sep='_')
  colnames(X) <- names(beta) <- lab
  list(X=X, y=y, beta=beta, family=family, group=j)
}
