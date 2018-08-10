#' Reproduce Example 9.1
#'
#' Reproduces Example 9.1 from the book.  If you specify any options, your results may look different.
#'
#' @param n       Sample size
#' @param p       Number of features
#' @param a       Number of causal ('A') variables
#' @param b       Number of correlated ('B') variables
#' @param rho     Correlation between 'A' and 'B' variables
#' @param beta    Coefficients for causal variables
#' @param seed    Random number seed for reproducibility
#' @param ...     Further arguments to \code{\link{genDataABN}}
#'
#' @examples
#' out <- Ex9.1()

Ex9.1 <- function(n=100, p=60, a=6, b=2, rho=0.5, beta=c(1,-1,0.5,-0.5,0.5,-0.5), seed=8, ...) {
  set.seed(seed)
  genDataABN(n=n, p=p, a=a, b=b, rho=rho, beta=beta, ...)
}
