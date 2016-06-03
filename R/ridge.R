ridge.formula <- function(formula, data, subset, na.action, lambda = 0, contrasts = NULL, ...) {
  m <- match.call(expand.dots = FALSE)
  m$contrasts <- m$lambda <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  y <- model.response(m)
  X <- model.matrix(Terms, m, contrasts)
  if (Inter <- attr(Terms, "intercept")) X <- X[, -Inter]
  ridge.matrix(X, y, lambda)
}
ridge.matrix <- function (XX, yy, lambda) {
  require(ncvreg)
  std <- .Call("standardize", XX, PACKAGE="ncvreg")
  X <- std[[1]]
  n <- nrow(X)
  p <- ncol(X)
  y <- yy - mean(y)
  Xs <- svd(X)
  d <- Xs$d
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(lambda, rep(dx, k))
  rhs <- crossprod(Xs$u, y)
  a <- drop(d * rhs)/div
  dim(a) <- c(dx, k)
  coef <- Xs$v %*% a
  dimnames(coef) <- list(colnames(X), format(lambda))
  GCV <- colSums((y - X %*% coef)^2)/(n - colSums(matrix(d^2/div, 
                                                         dx)))^2
  res <- list(coef = drop(coef), scales = std[[2]], lambda = lambda, GCV = GCV)
  class(res) <- "ridge"
  res
}
ridge <- function(obj,...) UseMethod("ridge")
