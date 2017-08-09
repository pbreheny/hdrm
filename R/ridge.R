ridge.formula <- function(formula, data, subset, na.action, contrasts = NULL, ...) {
  m <- match.call(expand.dots = FALSE)
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
ridge.matrix <- function (XX, yy, lambda=10^(seq(-3, 3, length=49))) {
  X <- std(XX)
  n <- nrow(X)
  p <- ncol(X)
  y <- yy - mean(yy)
  Xs <- svd(X)
  d <- Xs$d
  k <- length(lambda)
  dx <- length(d)
  div <- d^2 + rep(n*lambda, rep(dx, k))
  rhs <- crossprod(Xs$u, y)
  a <- drop(d * rhs)/div
  dim(a) <- c(dx, k)
  coef <- Xs$v %*% a
  dimnames(coef) <- list(colnames(XX), format(lambda))

  df <- colSums(matrix(d^2/div, dx))
  Y <- X %*% coef
  RSS <- colSums((y - Y)^2)
  GCV <- RSS/(1-df/n)^2

  beta <- matrix(0, nrow = nrow(coef) + 1, ncol = length(lambda))
  beta[-1,] <- coef/attr(X, 'scale')
  beta[1, ] <- mean(yy) - crossprod(attr(X, 'center'), beta[-1,])
  if (is.null(colnames(XX))) colnames(XX) <- paste0('V', 1:p)
  dimnames(beta) <- list(c("(Intercept)", colnames(XX)), lambda)

  res <- list(beta = drop(beta), lambda = lambda, GCV = GCV, df=df, RSS=RSS, n=n, SVD=Xs, center=attr(X, 'center'), scale=attr(X, 'scale'))
  class(res) <- "ridge"
  res
}
plot.ridge <- function(x, xaxis=c('loglam', 'df', 'both'), xlab, ylab, ...) {
  xaxis <- match.arg(xaxis)
  B <- t(x$beta[-1,])
  col <- pal(ncol(B))
  if (xaxis=='loglam' | xaxis=='both') {
    ll <- log10(x$lambda)
    if (missing(xlab)) xlab <- expression(lambda)
    matplot(ll, B, lty=1, col=col, type="l", lwd=3, xaxt="n",
            xlab=xlab, ylab="", las=1, xlim=rev(range(ll)), bty="n")
    logAxis(1, base=10)
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
coef.ridge <- function(object, lambda, which=1:length(object$lambda), drop = TRUE, ...) {
  if (length(object$lambda)==1) {
    if (!missing(lambda) && lambda != object$lambda) stop(paste0("Cannot return fit for lambda=", lambda, "; fit does not contain a regularization path"))
    return(object$beta)
  }
  if (!missing(lambda)) {
    ind <- approx(object$lambda, seq(object$lambda), lambda)$y
    l <- floor(ind)
    r <- ceiling(ind)
    w <- ind%%1
    beta <- (1-w)*object$beta[, l, drop = FALSE] + w*object$beta[, r, drop = FALSE]
    colnames(beta) <- round(lambda, 4)
  }
  else beta <- object$beta[, which, drop = FALSE]
  if (drop)
    return(drop(beta))
  else return(beta)
}
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
  rdf <- object$n-object$df[ind]-1
  s2 <- object$RSS[ind]/rdf
  V <- s2/object$n*W
  S <- diag(1/object$scale)
  x <- object$center
  SE <- sqrt(c(s2/object$n + crossprod(x, S %*% V %*%S) %*% x, diag(V)/object$scale^2))
  Tab <- data.frame(b, SE, b/SE)
  colnames(Tab) <- c('Estimate', 'SE', 't')
  attr(Tab, "rdf") <- rdf
  Tab
}
predict.ridge <- function(object, X, lambda, which=1:length(object$lambda), drop=TRUE, ...) {
  beta <- coef(object, lambda=lambda, which=which, drop=FALSE)
  if (class(beta) == 'numeric') beta <- matrix(beta, ncol=1)
  alpha <- beta[1,]
  beta <- beta[-1,,drop=FALSE]
  out <- sweep(X %*% beta, 2, alpha, "+")
  if (!drop) return(out)
  drop(out)
}
confint.ridge <- function(object, X, lambda, which, parm, level=0.95, ...) {
  s <- summary(object, lambda, which)
  m <- -qt((1-level)/2, attr(s, "rdf"))
  val <- cbind(Lower=s$Estimate-m*s$SE, Upper=s$Estimate+m*s$SE)
  if (!missing(parm)) val <- val[parm,]
  val
}
ridge <- function(obj,...) UseMethod("ridge")