Fig9.3 <- function(res, N=100, seed=1) {
  X <- res$X
  y <- res$y
  p <- ncol(X)
  n <- nrow(X)
  cvfit <- cv.glmnet(X, y)
  pb <- txtProgressBar(0, N, style=3)
  B <- matrix(NA, N, p, dimnames=list(1:N, colnames(X)))
  set.seed(seed)
  for (i in 1:N) {
    ind <- sample(1:n, replace=TRUE)
    #ind <- as.logical(sample(rep(0:1, each=n/2)))
    fit.i <- glmnet(X[ind,], y[ind], lambda=cvfit$lambda)
    B[i,] <- coef(fit.i, s=cvfit$lambda.min)[-1]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  cvg <- numeric(p)
  SE <- apply(B, 2, SE)

#  apply(B, 1, function(x) res$)

  S <- apply(SS, 2:3, mean)
  colnames(S) <- ncvreg:::lamNames(fit$lambda)
  q <- apply(Q, 2, mean)

  l <- fit$lambda
  col <- rep(rgb(0.6, 0.6, 0.6, 0.25), p)
  col[res$varType=="A"] <- "red"
  matplot(l, t(S), type="l", lty=1, xlim=rev(range(l)), col=col, lwd=2, las=1, bty="n",
          xlab=expression(lambda), ylab="Stability")

  return(invisible(list(Selected=q, Stability=S)))
}
