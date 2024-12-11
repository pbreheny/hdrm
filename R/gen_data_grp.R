#' Simulate grouped data for regression models
#'
#' This function is designed to scale efficiently to high dimensions, and
#' therefore imposes some restrictions. For example, correlation must be
#' positive.
#'
#' Note that if beta is not supplied, this function must calculate the SNR to
#' determine an appropriate coefficient size. This will be slow if the dimension
#' is large and beta is not sparse.
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
#' @examples
#' Data <- gen_data_grp(100, 10, 5, J1=3, K1=2)
#' expect_equal(dim(Data$X), c(100, 50))
#' head(Data$y)
#' B <- matrix(Data$beta, ncol=10)
#' expect_false(any(B[1:2, 1:3]==0))
#' expect_true(all(B[3:5, 1:3]==0))
#' expect_true(all(B[, 4:10]==0))
#' expect_equal(Data$group, rep(1:10, each=5))
#'
#' gen_data_grp(100, 3, 3, J1=2, K1=2)$beta
#' gen_data_grp(100, 3, 3, J1=2, K1=2, SNR=2)$beta
#' gen_data_grp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8)$beta
#' gen_data_grp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het')$beta
#' gen_data_grp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het', signal.g='het')$beta
#' gen_data_grp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het', b=1)$beta
#'
#' gen_data_grp(1000, 3, 3, rho=0)$X |> cor() |> round(digits=2)
#' gen_data_grp(1000, 3, 3, rho=0.7)$X |> cor() |> round(digits=2)
#' gen_data_grp(1000, 3, 3, rho=0.3, rho.g=0.8)$X |> cor() |> round(digits=2)
#' gen_data_grp(1000, 3, 3, rho=0.1, rho.g=0.5)$X |> cor() |> round(digits=2)
#' @export

gen_data_grp <- function(n, J, K=1, beta, family=c("gaussian","binomial"), J1=ceiling(J/2), K1=K, SNR=1, signal = c("homogeneous","heterogeneous"),
                       signal.g = c("homogeneous","heterogeneous"), rho = 0, rho.g = rho, rho.gz = rho) {
  family <- match.arg(family)
  signal <- match.arg(signal)
  signal.g <- match.arg(signal.g)

  # Gen X
  columns <- list()
  common_factor <- sqrt(rho)*rnorm(n)
  for (i in 1:J1) {
    z <- rnorm(n)
    columns[[i]] <- common_factor + sqrt(rho.g - rho)*rnorm(n) + sqrt(1-rho.g) * matrix(rnorm(n*K), n, K)
  }
  for (i in (J1 + 1):J) {
    z <- rnorm(n)
    columns[[i]] <- common_factor + sqrt(rho.gz - rho)*rnorm(n) + sqrt(1-rho.gz) * matrix(rnorm(n*K), n, K)
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
