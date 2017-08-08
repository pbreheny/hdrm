Fig9.2 <- function(res, N=100) {
  X <- res$X
  y <- res$y
  p <- ncol(X)
  n <- nrow(X)
  fit <- glmnet(X, y)
  pb <- txtProgressBar(0, N, style=3)
  SS <- array(NA, dim=c(N, p, length(fit$lambda)), dimnames=list(1:N, colnames(X), fit$lambda))
  Q <- matrix(NA, N, length(fit$lambda))
  for (i in 1:N) {
    #ind <- sample(1:n, replace=TRUE)
    ind <- as.logical(sample(rep(0:1, each=n/2)))
    fit.i <- glmnet(X[ind,], y[ind], lambda=fit$lambda)
    SS[i,,] <- as.matrix(coef(fit.i)[-1,]!=0)
    Q[i,] <- sapply(predict(fit.i, type="nonzero"), length)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  S <- apply(SS, 2:3, mean)
  q <- apply(Q, 2, mean)

  l <- fit$lambda
  col <- rep(rgb(0.6, 0.6, 0.6, 0.25), p)
  col[res$varType=="A"] <- "red"
  matplot(l, t(S), type="l", lty=1, xlim=rev(range(l)), col=col, lwd=2, las=1, bty="n",
          xlab=expression(lambda), ylab="Stability")

  return(invisible(list(Selected=q, Stability=S)))
}