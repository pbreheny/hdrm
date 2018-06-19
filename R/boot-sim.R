Ex9.3 <- function(N=100, B=100, n=100, p=60, n.corr=2, rho=0.5, seed=1) {
  N=10; B=100; n=100; p=60; n.corr=2; rho=0.5; seed=1
  require(glmnet)
  pb <- txtProgressBar(0, N, style=3)
  set.seed(seed)
  for (i in 1:N) {
    Data <- genABC(n=n, p=p, n.corr=n.corr, rho=rho)
    cvfit <- cv.glmnet(Data$X, Data$y)
    BB <- matrix(NA, B, p, dimnames=list(1:N, colnames(Data$X)))
    for (j in 1:B) {
      ind <- sample(1:n, replace=TRUE)
      fit.j <- glmnet(Data$X[ind,], Data$y[ind], lambda=cvfit$lambda)
      BB[j,] <- coef(fit.j, s=cvfit$lambda.min)[-1]
    }
  }
    setTxtProgressBar(pb, i)

  #B <-
  close(pb)
  cvg <- numeric(p)
  SE <- apply(B, 2, SE)

  #apply(B, 1, function(x) res$)

  S <- apply(SS, 2:3, mean)
  colnames(S) <- ncvreg:::lamNames(fit$lambda)
  q <- apply(Q, 2, mean)

  l <- fit$lambda
  col <- rep(rgb(0.6, 0.6, 0.6, 0.25), p)
  col[res$varType=="A"] <- "red"
  matplot(l, t(S), type="l", lty=1, xlim=rev(range(l)), col=col, lwd=2, las=1, bty="n",
          xlab=expression(lambda), ylab="Stability")

}
