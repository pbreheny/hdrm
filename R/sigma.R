#' Reproduce Figure 2.8
#'
#' Reproduces Figure 2.8 from the book; if you specify any options, your results may look different.
#'
#' @param n         Sample size
#' @param p         Number of features
#' @param N         Number of splits for the refitted CV estimator
#' @param parlist   List of arguments to pass to `par()`
#' @param seed      Random number seed for reproducibility
#'
#' @examples Fig2.8(N=5)  # Increase N for a more reliable estimate
#'
#' @export

Fig2.8 <- function(n=100, p=1000, N=25, parlist=list(mfrow=c(1,2), mar=c(5,5,0.5,0.5), oma=c(0,0,2,0)), seed=1) {
  set.seed(seed)

  # Null case ---------------------------------------------------------------

  # Generate data
  X <- std(matrix(rnorm(n*p), n, p))
  b <- rep(0, p)
  y <- rnorm(n, X%*%b)

  # Fit
  cvfit <- cv.ncvreg(X, y, penalty="lasso", lambda.min=0.05)
  fit <- cvfit$fit
  n <- nrow(X)
  l <- fit$lambda
  ll <- log(l)
  ll1 <- ll
  nv <- predict(fit, type="nvars")+1

  # Estimate sigma using RSS, CVE
  Sigma <- array(NA, dim=c(length(l), 3, 2))
  Y <- predict(fit, X)
  RSS <- apply(y-Y, 2, crossprod)
  Sigma[,1,1] <- ifelse(n > nv, suppressWarnings(sqrt(RSS/(n-nv))), NA)
  Sigma[,2,1] <- cvfit$cve

  # Fan estimator
  V <- matrix(NA, N, length(l))
  lmvar <- function(y, X, s) {
    if (!length(s)) return(var(y))
    ols <- lm(y~X[,s])
    summary(ols)$sigma^2
  }
  pb <- txtProgressBar(0, N, style=3)
  for (i in 1:N) {
    ind <- sample(1:n, n/2)
    X1 <- X[ind,]
    X2 <- X[-ind,]
    y1 <- y[ind]
    y2 <- y[-ind]
    fit1 <- glmnet(X1, y1, lambda=fit$lambda)
    fit2 <- glmnet(X2, y2, lambda=fit$lambda)
    s1 <- predict(fit1, type="nonzero")
    s2 <- predict(fit2, type="nonzero")
    v1 <- v2 <- numeric(length(l))
    for (j in 1:length(l)) {
      v1[j] <- lmvar(y1, X1, s2[[j]])
      v2[j] <- lmvar(y2, X2, s1[[j]])
    }
    V[i,] <- 0.5*v1 + 0.5*v2
    setTxtProgressBar(pb, i)
  }
  Sigma[,3,1] <- sqrt(apply(V, 2, mean, na.rm=TRUE))

  # Signal case -------------------------------------------------------------

  # Generate data
  b <- numeric(p); b[1:5] <- 1
  y <- rnorm(n, X%*%b)

  # Fit
  cvfit <- cv.ncvreg(X, y, penalty="lasso", lambda.min=0.01)
  fit <- cvfit$fit
  n <- nrow(X)
  l <- fit$lambda
  ll <- log(l)
  ll2 <- ll
  nv <- predict(fit, type="nvars")+1

  # Estimate sigma using RSS, CVE
  Y <- predict(fit, X)
  RSS <- apply(y-Y, 2, crossprod)
  Sigma[,1,2] <- ifelse(n > nv, suppressWarnings(sqrt(RSS/(n-nv))), NA)
  Sigma[,2,2] <- cvfit$cve

  ## Fan estimator
  V <- matrix(NA, N, length(l))
  pb <- txtProgressBar(0, N, style=3)
  for (i in 1:N) {
    ind <- sample(1:n, n/2)
    X1 <- X[ind,]
    X2 <- X[-ind,]
    y1 <- y[ind]
    y2 <- y[-ind]
    fit1 <- glmnet(X1, y1, lambda=fit$lambda)
    fit2 <- glmnet(X2, y2, lambda=fit$lambda)
    s1 <- predict(fit1, type="nonzero")
    s2 <- predict(fit2, type="nonzero")
    v1 <- v2 <- numeric(length(l))
    for (j in 1:length(l)) {
      v1[j] <- lmvar(y1, X1, s2[[j]])
      v2[j] <- lmvar(y2, X2, s1[[j]])
    }
    V[i,] <- 0.5*v1 + 0.5*v2
    setTxtProgressBar(pb, i)
  }
  Sigma[,3,2] <- sqrt(apply(V, 2, mean, na.rm=TRUE))

  # Plot --------------------------------------------------------------------

  op <- par(parlist)
  col <- c("black", "#FF4E37FF", "#008DFFFF")
  matplot(ll1, Sigma[,,1], type="l", col=col, lty=1, lwd=3, xlim=rev(range(ll1)), ylab=expression(hat(sigma)), las=1, xaxt="n", xlab=expression(lambda), bty="n")
  abline(h=1, lty=2, col="gray", lwd=2)
  at <- seq(max(ll1), min(ll1), length=5)
  axis(1, at=at, labels=round(exp(at), 2))
  matplot(ll2, Sigma[,,2], type="l", col=col, lty=1, lwd=3, xlim=rev(range(ll2)), ylab=expression(hat(sigma)), las=1, xaxt="n", xlab=expression(lambda), bty="n", ylim=c(0, 3))
  abline(h=1, lty=2, col="gray", lwd=2)
  at <- seq(max(ll2), min(ll2), length=5)
  axis(1, at=at, labels=round(exp(at), 2))
  toplegend(legend=c("Plug-in", "CV", "RCV"), col=col, lwd=3)
  par(op)
}
