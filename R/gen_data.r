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
#' @examples
#' dat <- gen_data(100, 100, 10)
#' dim(dat$X)
#' head(dat$y)
#' head(dat$beta)
#'
#' gen_data(100, 10, 5)$beta
#' gen_data(100, 10, 5, SNR=2)$beta
#' gen_data(100, 10, 5, SNR=2, corr='exch', rho=0.7)$beta
#' gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.7)$beta
#' gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.7, signal='het')$beta
#' gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het')$beta
#' gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het', b=1)$beta
#'
#' gen_data(10, 10, 5, family='binomial')$y
#'
#' gen_data(1000, 10, rho=0.0, corr='exch')$X |> cor() |> round(digits=2)
#' gen_data(1000, 10, rho=0.7, corr='exch')$X |> cor() |> round(digits=2)
#' gen_data(1000, 10, rho=0.7, corr='auto')$X |> cor() |> round(digits=2)
#' gen_data(1000, 3, 3, rho=0)$X |> cor() |> round(digits=2)
#' @export

gen_data <- function(n, p, p1=floor(p/2), beta, family=c("gaussian","binomial"), SNR=1,
                    signal = c("homogeneous","heterogeneous"), corr=c("exchangeable", "autoregressive"),
                    rho = 0) {
  family <- match.arg(family)
  signal <- match.arg(signal)
  corr <- match.arg(corr)

  # Gen X, S
  X <- gen_x(n, p, rho, corr)

  # Gen beta
  if (missing(beta) || length(beta)==1) {
    j <- 1:p
    s <- c(-1,1)[j%%2+1]
    b <- (j <= p1)
    if (missing(beta)) {
      if (signal=="heterogeneous") b <- b*rev(j)
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(drop(crossprod(b)))
      #beta <- b*sqrt(SNR)/sqrt(calc_bsb(b, rho, corr))
    } else {
      beta <- b*s*beta
    }
  }

  # Gen y
  y <- gen_y(X%*%beta, family=family, sigma=1)

  # Label and return
  w <- 1 + floor(log10(p))
  vlab <- paste0('V', formatC(1:p, format='d', width=w, flag='0'))
  colnames(X) <- names(beta) <- vlab
  list(X=X, y=y, beta=beta, family=family)
}

gen_x <- function(n, p, rho, corr=c('exchangeable', 'autoregressive')) {
  corr <- match.arg(corr)
  if (corr == 'exchangeable') {
    z <- rnorm(n)
    sqrt(rho)*z + sqrt(1-rho) * matrix(rnorm(n*p), n, p)
  } else if (corr == 'autoregressive') {
    Z <- cbind(rnorm(n), matrix(rnorm(n*(p-1), sd=sqrt(1-rho^2)), n, p-1))
    apply(Z, 1, filter, filter=rho, method='recursive') |> t()
  }
}

calc_bsb <- function(b, rho, corr) {
  if (corr == 'exchangeable') {
    sum(rho*tcrossprod(b)) + (1-rho)*crossprod(b) |> drop()
  } else if (corr == 'autoregressive') {
    out <- crossprod(b)
    bb <- tcrossprod(b)
    for (j in 1:min(10, length(b)-1)) {
      out <- out + 2 * rho^j * sum(Matrix::band(bb, j, j))
    }
    drop(out)
  }
}

gen_y <- function(eta, family=c("gaussian", "binomial"), sigma=1) {
  family=match.arg(family)
  n <- length(eta)
  if (family=="gaussian") {
    rnorm(n, mean=eta, sd=sigma)
  } else if (family=="binomial") {
    pi. <- exp(eta)/(1+exp(eta))
    pi.[eta > log(.9999/.0001)] <- 1
    pi.[eta < log(.0001/.9999)] <- 0
    rbinom(n,1,pi.)
  }
}
