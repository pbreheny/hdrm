logAxis <- function(side=1, base, disp=base, n=5, style=NULL, ...) {
  if (missing(base)) base <- exp(1)
  is.x <- side%%2 == 1
  usr <- if(is.x) par("usr")[1:2] else par("usr")[3:4]
  r <- usr*log(base)/log(disp)
  px <- pretty(r, n=n)
  if (r[2] > r[1]) {
    px <- px[px > r[1] & px < r[2]]
  } else {
    px <- px[px < r[1] & px > r[2]]
  }
  if (is.null(style)) style <- if (isTRUE(all.equal(px, as.integer(px)))) "pow" else "dec"
  if (style=="pow") {
    at <- px*log(disp)/log(base)
    lab <- disp^px
  } else {
    s <- seq(usr[1], usr[2], len=n)
    s <- s - s[which.min(abs(s))]
    at <- s/max(range(s)/usr[1:2])
    lab <- formatC(base^at, digits=1, format="f")
  }
  a <- axis(side, at=at, labels=lab, las=1, ...)
}
toplegend <- function(horiz=TRUE, ...) {
  if (par("oma")[3]==0) {
    x <- mean(par("usr")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    y  <- mean(c(yy[2],par("usr")[4]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, horiz=horiz, ...)
  } else {
    g <- par("mfrow")
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform.coord(xx, c(g[2]-1,g[2])/g[2])
    yyy <- transform.coord(yy, c(g[1]-1,g[1])/g[1])
    yyyy <- transform.coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2],yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, horiz=horiz, ...)
  }
}
rightlegend <- function(...) {
  if (par("oma")[3]==0) {
    y <- mean(par("usr")[3:4])
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    x <- mean(c(xx[2],par("usr")[2]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)
  } else {
    g <- par("mfrow")
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform.coord(xx, c(g[2]-1,g[2])/g[2])
    yyy <- transform.coord(yy, c(g[1]-1,g[1])/g[1])
    yyyy <- transform.coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2],yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)
  }
}
transform.coord <- function(x,p) {
  ba <- (x[2]-x[1])/(p[2]-p[1])
  a <- x[1]-p[1]*ba
  b <- a + ba
  c(a,b)
}
pal <- function(n, alpha=1)
{
  if (n==2) {
    val <- hcl(seq(15,375,len=4), l=60, c=150, alpha=alpha)[c(1,3)]
  } else val <- hcl(seq(15,375,len=n+1), l=60, c=150, alpha=alpha)[1:n]
  val
}
lfdrPlot <- function(z, pi0=1, delta=0, sigma=1, lfdrReturn=TRUE, ...) {
  # Calculation
  dens <- density(z, bw="nrd")
  f <- approxfun(dens$x, dens$y)
  f0 <- function(z) pi0*dnorm(z, mean=delta, sd=sigma)
  lfdr <- pmin(f0(z)/f(z), 1)

  # Plot
  h <- hist(z, breaks=seq(min(z), max(z), length = 99), plot=FALSE)
  zz <- seq(min(z), max(z), len=299)
  ylim=c(0, max(c(h$density, dens$y, f0(delta))))
  #plot(h, main="", border=FALSE, col="lightgray", freq=FALSE, las=1, ylim=ylim)
  plot(h, main="", border=FALSE, col="lightgray", freq=FALSE, las=1, ylim=ylim)
  lines(dens, col=pal(2)[1], lwd=2)
  lines(zz, f0(zz), col=pal(2)[2], lwd=2)
  fdr.zz <- f0(h$mids)/f(h$mids)
  y <- pmax(h$density * (1 - fdr.zz), 0)
  for (k in 1:length(h$mids)) lines(rep(h$mids[k],2), c(0, y[k]), lwd = 2, col = "red")

  if (lfdrReturn) return(lfdr)
}
ridge.formula <- function(formula, data, subset, na.action, contrasts = NULL, ...) {
  m <- match.call(expand.dots = FALSE)
  m$contrasts <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
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
  beta[-1,] <- coef/attr(X, 'scaled:scale')
  beta[1, ] <- mean(yy) - crossprod(attr(X, 'scaled:center'), beta[-1,])
  if (is.null(colnames(XX))) colnames(XX) <- paste0('V', 1:p)
  dimnames(beta) <- list(c("(Intercept)", colnames(XX)), lambda)

  res <- list(beta = drop(beta), lambda = lambda, GCV = GCV, df=df, RSS=RSS, n=n, SVD=Xs, scale=attr(X, 'scaled:scale'))
  class(res) <- "ridge"
  res
}
plot.ridge <- function(x, xaxis=c('loglam', 'df'), ...) {
  xaxis <- match.arg(xaxis)
  B <- t(x$beta[-1,])
  col <- pal(ncol(B))
  if (xaxis=='loglam') {
    ll <- log10(x$lambda)
    matplot(ll, B, lty=1, col=col, type="l", lwd=3, xaxt="n",
            xlab=expression(lambda), ylab="", las=1, xlim=rev(range(ll)), bty="n")
    logAxis(1, base=10)
  } else {
    matplot(x$df, B, lty=1, col=col, type="l", lwd=3,
            xlab='Degrees of freedom', ylab="", las=1, bty="n")
  }
}
coef.ridge <- function(object, lambda, which=1:length(object$lambda), drop = TRUE, ...) {
  if (!missing(lambda)) {
    ind <- approx(object$lambda, seq(object$lambda), lambda)$y
    l <- floor(ind)
    r <- ceiling(ind)
    w <- ind%%1
    beta <- (1 - w) * object$beta[, l, drop = FALSE] + w *
      object$beta[, r, drop = FALSE]
    colnames(beta) <- round(lambda, 4)
  }
  else beta <- object$beta[, which, drop = FALSE]
  if (drop)
    return(drop(beta))
  else return(beta)
}
summary.ridge <- function(object, lambda, which, ...) {
  if (missing(which)) {
    ind <- which.min(abs(lambda-object$lambda))
  } else {
    ind <- which
  }
  l <- object$lambda[ind]
  W <- tcrossprod(sweep(object$SVD$v, 2, object$SVD$d^2/object$n + l, '/'), object$SVD$v)
  b <- coef(object, which=ind)
  s2 <- object$RSS[ind]/(object$n-object$df[ind])
  V <- s2/object$n*W
  SE <- sqrt(c(s2/object$n, diag(V)/object$scale^2))
  Tab <- cbind(b,SE, b/SE)
  colnames(Tab) <- c('Estimate', 'SE', 't')
  Tab
}
predict.ridge <- function(object, X, lambda, which=1:length(object$lambda), ...) {
  beta <- coef(object, lambda=lambda, which=which, drop=FALSE)
  alpha <- beta[1,]
  beta <- beta[-1,,drop=FALSE]
  sweep(X %*% beta, 2, alpha, "+")
}
confint.ridge <- function(object, X, lambda, which, parm, level=0.95, ...) {
  if (missing(which)) {
    ind <- which.min(abs(lambda-object$lambda))
  } else {
    ind <- which
  }
  l <- object$lambda[ind]
  W <- tcrossprod(sweep(object$SVD$v, 2, object$SVD$d^2/object$n + l, '/'), object$SVD$v)
  b <- coef(object, which=ind)
  s2 <- object$RSS[ind]/(object$n-object$df[ind])
  V <- s2/object$n*W
  SE <- sqrt(c(s2/object$n, diag(V)/object$scale^2))
  m <- -qt((1-level)/2, object$n-object$df[ind])
  val <- cbind(Lower=b-m*SE, Upper=b+m*SE)
  if (!missing(parm)) val <- val[parm,]
  val
}
ridge <- function(obj,...) UseMethod("ridge")
#' Generates a random design matrix and outcome.
#'
#' This function generated a random design matrix \code{X} and
#' response vector \code{y} for use in regression testing/simulation.
#'
#' @param n Sample size
#' @param J Number of groups
#' @param K Number of explanatory variables per group
#' @param beta Vector of regression coefficients in the generating model, or, if a scalar, the value of each nonzero regression coefficient.
#' @param family Generate \code{y} according to linear (\code{"gaussian"})
#' or logistic (\code{"binomial"}) model
#' @param J0 Number of nonzero groups
#' @param K0 Number of nonzero coefficients per group
#' @param SNR Signal to noise ratio
#' @param sig Should the groups be heterogeneous (in beta) or homogeneous
#' @param sig.g Should the coefficients within a group be heterogeneous or homogeneous
#' @param rho Correlation between groups
#' @param rho.g Correlation between parameters within a group
#'
#' @export
genData <- function(n, J, K=1, beta, family=c("gaussian","binomial"), J0=ceiling(J/2), K0=K, SNR=1, sig = c("homogeneous","heterogeneous"), sig.g = c("homogeneous","heterogeneous"), rho = 0, rho.g = rho, corr=c("exchangeable", "autoregressive")) {
  family <- match.arg(family)
  sig <- match.arg(sig)
  sig.g <- match.arg(sig.g)
  corr <- match.arg(corr)

  ## Gen X, S
  if (corr=="exchangeable") {
    X <- genX(n=n, J=J, K=K, rho=rho, rho.g=rho.g)
  } else {
    require(Matrix)
    RHO <- matrix(rho^(0:(J-1)), J, J, byrow=TRUE)
    S <- bandSparse(J, k=0:(J-1), diagonals=RHO, symmetric=TRUE)
    R <- chol(S)
    X <- as.matrix(matrix(rnorm(n*J), n, J) %*% R)
  }

  j <- rep(1:J,rep(K,J))

  ## Gen beta
  if (missing(beta) || length(beta)==1) {
    k <- rep(1:K,J)
    b <- (j <= J0) * (k <= K0)
    s <- c(1,-1)[1+j%%2] * c(1,-1)[1+k%%2]
    if (missing(beta)) {
      S <- matrix(rho, nrow=J*K, ncol=J*K)
      for (i in 1:J) S[(i-1)*K+1:K,(i-1)*K+1:K] <- rho.g
      diag(S) <- rep(1,J*K)
      if (sig=="heterogeneous") b <- b*j
      if (sig.g=="heterogeneous") b <- b*k
      b <- b*s
      beta <- b*sqrt(SNR)/sqrt(crossprod(b,S)%*%b)
    } else beta <- b*s*beta
  }

  ## Gen y
  y <- genY(X%*%beta, family=family, sigma=1)
  return(list(X=X,y=y,beta=beta,family=family,group=j))
}

## rho  : correlation across all explanatory variables
## rho.g: correlation within group (must be at least rho)
genX <- function(n, J, K=1, rho=0, rho.g=rho, corr=corr) {
  a <- sqrt(rho/(1-rho.g))
  b <- sqrt((rho.g-rho)/(1-rho.g))
  Z <- rnorm(n)
  ZZ <- t(matrix(rep(rnorm(n*J), rep(K,n*J)), ncol=n))
  ZZZ <- matrix(rnorm(n*J*K),nrow=n)
  return(matrix(as.numeric(a*Z + b*ZZ + ZZZ),nrow=n)/sqrt(1+a^2+b^2))
}

genY <- function(eta,family=c("gaussian","binomial"),sigma=1) {
  family=match.arg(family)
  n <- length(eta)
  if (family=="gaussian") y <- rnorm(n,mean=eta,sd=sigma)
  else if (family=="binomial")
  {
    pi. <- exp(eta)/(1+exp(eta))
    pi.[eta > log(.9999/.0001)] <- 1
    pi.[eta < log(.0001/.9999)] <- 0
    y <- rbinom(n,1,pi.)
  }
  return(y)
}
CIplot.matrix <- function(obj, labels=rownames(B), sort=TRUE, pxlim, xlim, ylim, sub, diff=(ncol(B)==4), null=0, n.ticks=6, mar, axis=!add, trans, p.label=FALSE, xlab="", ylab="", add=FALSE, setupOnly=FALSE, lwd=2, replaceUnderscore=TRUE, ...) {
  B <- obj
  if (sort) B <- B[order(B[,1], decreasing=TRUE),,drop=FALSE]

  ## Set up margins
  if (missing(mar)) {
    m1 <- 5
    nn <- if (is.null(labels)) 10 else max(nchar(labels))
    m2 <- nn/3+.5
    m3 <- 2
    m4 <- if (diff) 6 else 2
    op <- par(mar=c(m1, m2, m3, m4))
  } else op <- par(mar=mar)
  n <- nrow(B)
  if (!missing(trans)) B[,1:3] <- trans(B[,1:3])

  ## Set up plot structure and add points
  if (missing(pxlim)) {
    pxlim <- if (missing(xlim)) pretty(range(B[,2:3], na.rm=TRUE),n=n.ticks-1) else pretty(xlim, n=n.ticks-1)
  }
  if (missing(ylim)) ylim <- c(0.5,n+0.5)
  if (add) {
    points(B[n:1,1], 1:n, pch=19)
  } else if (setupOnly) {
    plot(B[n:1,1], 1:n, type="n", xlim = range(pxlim), ylim=ylim, ylab=ylab, axes=FALSE, pch=19, xlab=xlab, ...)
    return(invisible(NULL))
  } else {
    plot(B[n:1,1], 1:n, xlim = range(pxlim), ylim=ylim, ylab=ylab, axes=FALSE, pch=19, xlab=xlab, ...)
  }

  ## Add lines, p-values
  for (i in 1:n) {
    dots <- list(...)
    col <- if ("col" %in% names(dots)) rep_len(dots$col[n-i+1], n) else "black"
    lines(c(B[i,2:3]), c(n-i+1,n-i+1), lwd=lwd, col=col)
    if (diff) {
      p <- formatP(B[,4], label=p.label)
      p[is.na(B[,4])] <- ""
      mtext(at=n-i+1,p[i],line=1,side=4,las=1, cex=0.8*par("cex"), adj=0)
    }
  }
  if (axis) axis(1, pxlim)
  if (diff) {
    if (!missing(trans)) null <- trans(null)
    abline(v=null,col="gray")
  }
  if (!missing(sub)) mtext(sub,3,0,cex=0.8)

  ## Add labels
  if (replaceUnderscore) labels <- gsub("_", " ", labels)
  if (!add) {
    ind <- !is.na(B[,1])
    lapply(which(ind), function(l) text(x=par("usr")[1], adj=1, y=(n:1)[l], labels=labels[[l]], xpd=TRUE, cex=.8)) ## List approach is necessary for compatibility with expressions
    if (sum(!ind) > 0) {
      a <- diff(par("usr")[1:2])/diff(par("plt")[1:2])
      b <- par("usr")[1] - a*par("plt")[1]
      text(x=b+a*.01, adj=0, y=(n:1)[!ind], labels=labels[!ind], xpd=TRUE, cex=.8)
    }
  }
  par(op)
  invisible(B)
}
CIplot.lm <- function(obj, intercept=FALSE, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, ...)
{
  fit <- obj
  p <- length(coef(fit))
  j <- if (intercept) 1:p else 2:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,4])
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),,drop=FALSE]
  if (plot) CIplot(B,xlab=xlab,...)
  return(invisible(B))
}
CIplot.glm <- function(obj,...) CIplot.lm(obj,...)
CIplot.mer <- function(obj, intercept=FALSE, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, n.sim=10000, ...)
{
  fit <- obj
  p <- length(fit@fixef)
  j <- if (intercept) 1:p else 2:p
  B <- cbind(fit@fixef[j], confint(fit, j, n.sim=n.sim))
  if (!missing(tau)) B[,1:3] <- B[,1:3]*tau
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) CIplot(B,xlab=xlab,...)
  return(invisible(B))
}
CIplot.coxph <- function(obj, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- 1:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,5])
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) CIplot(B,xlab=xlab,...)
  return(invisible(B))
}
CIplot <- function(obj,...) UseMethod("CIplot")
formatP <- function(p,digits=2,label=FALSE) {
  val <- formatC(p,digits=digits,format="f")
  for (d in -(digits:4)) val[p < 10^d] <- paste("<",formatC(10^d))
  if (any(p < .01, na.rm=TRUE) & !label) val[substr(val,1,2)=="0."] <- paste("  ",val[substr(val,1,2)=="0."])
  if (label) {
    val[p >= 10^(-digits)] <- paste("p =",val[p >= 10^(-digits)])
    val[p < 10^(-digits)] <- paste("p",val[p < 10^(-digits)])
  }
  val
}
revlevel <- function(x) factor(x, levels=rev(levels(x)))
array2df <- function(X,vars=paste("V",1:ncol(df),sep="")) {
  df <- cbind(do.call("expand.grid",dimnames(X)),as.numeric(X))
  names(df) <- vars
  df
}
