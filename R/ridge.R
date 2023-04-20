#' Ridge regression
#'
#' @param obj         There are two options for running `ridge()`; either supply a formula and data frame as in `lm()`, or an X and y matrix as in `glmnet()`; see examples
#' @param y           If using the matrix interface, the vector of responses
#' @param lambda      An optional value or vector of values for the regression coefficient; if not supplied, this will be set up automatically
#' @param data        If using the formula interface, a data.frame/list/environment containing the variables in the formula
#' @param subset      If using the formula interface, a vector describing the subset of the data to be used in fitting the model
#' @param na.action   If using the formula interface, a function indicating what should happen when the data contain NAs, as in `lm()`
#' @param contrasts   If using the formula interface, a list to be passed to `model.matrix()`
#' @param ...         Additional arguments to be passed to methods
#'
#' @seealso [plot.ridge()], `coef.ridge()`, `predict.ridge()`, `summary.ridge()`, `confint.ridge()`
#'
#' @examples
#' attachData(pollution)
#' fit <- ridge(X, y)
#' summary(fit, lambda=0.1)
#' plot(fit)
#' plot(fit, xaxis='df')
#' plot(fit, xaxis='both')
#' plot(fit, standardize=TRUE)
#' coef(fit)
#' coef(fit, standardize=TRUE)
#' coef(fit, standardize=TRUE, lambda=0.1)
#' p1 <- cbind(1, std(X)) %*% coef(fit, standardize=TRUE, lambda=1)
#' p2 <- cbind(1, X) %*% coef(fit, lambda=1)
#' head(p1); head(p2) # identical
#' @export

ridge <- function(obj, ...) UseMethod("ridge")

#' @rdname ridge
#' @export

ridge.formula <- function(obj, data, subset, na.action, contrasts = NULL, ...) {
  m <- match.call(expand.dots = FALSE)
  names(m)[which(names(m)=='obj')] <- 'formula'
  mm <- match(c("formula", "data", "subset", "na.action"), names(m), 0L)
  m <- m[c(1, mm)]
  m[[1]] <- quote(stats::model.frame)
  m <- eval(m, parent.frame())
  Terms <- attr(m, "terms")
  y <- model.response(m)
  X <- model.matrix(Terms, m, contrasts)
  if (Inter <- attr(Terms, "intercept")) X <- X[, -Inter]
  ridge.matrix(X, y, ...)
}

#' @rdname ridge
#' @export

ridge.matrix <- function (obj, y, lambda, ...) {
  if (missing(lambda)) lambda <- 10^(seq(-3, 3, length=49))
  X <- std(obj)
  n <- nrow(X)
  p <- ncol(X)
  my <- mean(y)
  yy <- y - my
  Xs <- svd(X)
  d <- Xs$d
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(n*lambda, rep(dx, k))
  rhs <- crossprod(Xs$u, yy)
  a <- drop(d * rhs)/div
  dim(a) <- c(dx, k)
  coef <- Xs$v %*% a
  dimnames(coef) <- list(colnames(obj), format(lambda))

  df <- colSums(matrix(d^2/div, dx))
  Y <- X %*% coef
  RSS <- colSums((yy - Y)^2)
  GCV <- RSS/(1-df/n)^2

  beta <- matrix(0, nrow = nrow(coef) + 1, ncol = length(lambda))
  beta[-1,] <- coef/attr(X, 'scale')
  beta[1, ] <- my - crossprod(attr(X, 'center'), beta[-1,])
  if (is.null(colnames(obj))) colnames(obj) <- paste0('V', 1:p)
  dimnames(beta) <- list(c("(Intercept)", colnames(obj)), lambda)

  res <- list(beta = drop(beta), lambda = lambda, GCV = GCV, df=df, RSS=RSS, n=n, SVD=Xs, center=attr(X, 'center'), scale=attr(X, 'scale'), ymean=my)
  class(res) <- "ridge"
  res
}

#' Plot a ridge regression coefficient path
#'
#' @param x             An object of class `"ridge"`, as returned by `ridge()`
#' @param xaxis         One of `"loglam"`, `"df"`, or `"both"`. If `"both"`, the
#' bottom axis is lambda (log scale), and the top is df.
#' @param standardize   As in `coef()` (default: FALSE)
#' @param xlab,ylab     As in `plot()`
#' @param ...           Additional arguments to `matplot()`
#'
#' @export

plot.ridge <- function(x, xaxis=c('loglam', 'df', 'both'), standardize=FALSE, xlab, ylab, ...) {
  xaxis <- match.arg(xaxis)
  B <- t(coef(x, standardize=standardize)[-1,])
  col <- pal(ncol(B))
  if (xaxis=='loglam' | xaxis=='both') {
    ll <- log10(x$lambda)
    if (missing(xlab)) xlab <- expression(lambda)
    matplot(ll, B, lty=1, col=col, type="l", lwd=3, xaxt="n",
            xlab=xlab, ylab="", las=1, xlim=rev(range(ll)), bty="n", ...)
    log_axis(1, base=10)
  } else if (xaxis=='df') {
    matplot(x$df, B, lty=1, col=col, type="l", lwd=3,
            xlab='Degrees of freedom', ylab="", las=1, bty="n")
  }
  if (xaxis=='both') {
    ind <- seq(1, length(ll), length=5)
    axis(3, at=ll[ind], labels=round(x$df[ind], 1))
    mtext("Degrees of freedom", 3, 2.5)
  }
  if (missing(ylab)) {
    mtext(expression(hat(beta)), 2, 3, las=1)
  } else {
    mtext(ylab, 2, 3)
  }
}

#' @rdname ridge
#'
#' @param object        A `ridge` object, as returned by `ridge()`
#' @param which         Indices of `lambda` at which coefficients / predictions are required. By default, all
#' indices are returned. If `lambda` is specified, this will override `which`.
#' @param standardize   Return coefficients on standardized scale (default: FALSE)
#' @param drop          If requesting coefficients for a single lambda value, drop matrix down to a vector (default: TRUE)
#'
#' @export

coef.ridge <- function(object, lambda, which=1:length(object$lambda), standardize=FALSE, drop=TRUE, ...) {
  if (length(object$lambda)==1) {
    if (!missing(lambda) && lambda != object$lambda) stop(paste0("Cannot return fit for lambda=", lambda, "; fit does not contain a regularization path"))
    beta <- matrix(object$beta, ncol=1, dimnames=list(names(object$beta), object$lambda))
  } else if (!missing(lambda)) {
    ind <- approx(object$lambda, seq(object$lambda), lambda)$y
    l <- floor(ind)
    r <- ceiling(ind)
    w <- ind%%1
    beta <- (1-w)*object$beta[, l, drop = FALSE] + w*object$beta[, r, drop = FALSE]
    colnames(beta) <- round(lambda, 4)
  } else {
    beta <- object$beta[, which, drop = FALSE]
  }
  if (standardize) {
    beta[1,] <- object$ymean
    beta[-1,] <- beta[-1,] * object$scale
  }
  if (drop) beta <- drop(beta)
  beta
}

#' @rdname ridge
#' @export

summary.ridge <- function(object, lambda, which, ...) {
  if (length(object$lambda)==1) {
    if (!missing(lambda) && lambda != object$lambda) stop(paste0("Cannot return fit for lambda=", lambda, "; fit does not contain a regularization path"))
    ind <- 1
  } else if (missing(which)) {
    ind <- which.min(abs(lambda-object$lambda))
  } else {
    ind <- which
  }
  l <- object$lambda[ind]
  W <- tcrossprod(sweep(object$SVD$v, 2, object$SVD$d^2/object$n + l, '/'), object$SVD$v)
  b <- coef(object, which=ind)
  bb <- coef(object, standardize=TRUE, which=ind)
  rdf <- object$n-object$df[ind]-1
  s2 <- object$RSS[ind]/rdf
  V <- s2/object$n*W
  S <- diag(1/object$scale)
  x <- object$center
  SE <- sqrt(c(s2/object$n + crossprod(x, S %*% V %*%S) %*% x, diag(V)/object$scale^2))
  p <- 2*pt(-abs(-b/SE), rdf)
  Tab <- data.frame(b, bb, SE, b/SE, p)
  colnames(Tab) <- c('Estimate', 'Standardized', 'SE', 't', 'p')
  attr(Tab, "rdf") <- rdf
  Tab
}

#' @rdname ridge
#'
#' @param X   Matrix of predictor values at which predictions are required.
#'
#' @export

predict.ridge <- function(object, X, lambda, which=1:length(object$lambda), drop=TRUE, ...) {
  beta <- coef(object, lambda=lambda, which=which, drop=FALSE)
  if (!inherits(beta, 'matrix')) beta <- matrix(beta, ncol=1)
  alpha <- beta[1,]
  beta <- beta[-1,,drop=FALSE]
  out <- sweep(X %*% beta, 2, alpha, "+")
  if (!drop) return(out)
  drop(out)
}

#' @rdname ridge
#'
#' @param level   Confidence level (default: 0.95)
#' @param parm    Which parameters to construct confidence intervals for; either a vector of indices or a vector of names (default: all)
#'
#' @export

confint.ridge <- function(object, parm, level=0.95, X, lambda, which, ...) {
  s <- summary(object, lambda, which)
  m <- -qt((1-level)/2, attr(s, "rdf"))
  val <- cbind(Lower=s$Estimate-m*s$SE, Upper=s$Estimate+m*s$SE)
  rownames(val) <- if (inherits(object$beta, 'array')) rownames(object$beta) else names(object$beta)
  if (!missing(parm)) val <- val[parm,]
  attr(val, 'level') <- level
  val
}
