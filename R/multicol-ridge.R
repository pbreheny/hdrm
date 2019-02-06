#' Reproduce Example 1.2
#'
#' Reproduces Example 1.2 from the book.
#'
#' @param seed     Random number seed for reproducibility
#' @param lambda   Regularization parameter for ridge regression
#'
#' @examples
#' Ex1.2()
#' Ex1.2(lam=0.01)

Ex1.2 <- function(seed=11, lambda=0.1) {
  set.seed(seed)
  x1 <- rnorm(20)
  x2 <- rnorm(20, mean=x1, sd=.01)
  y <- rnorm(20, mean=3+x1+x2)
  cat("lm:\n")
  print(coef(lm(y~x1+x2)))
  cat("ridge:\n")
  coef(ridge(y~x1+x2), lambda=lambda)
}
