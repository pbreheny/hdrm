Ex9.2 <- function(N=100) {
  loadData("bcTCGA")
  p <- ncol(X)
  colnames(X) <- paste0("V", 1:p)
  X.TCGA <- X
  y.TCGA <- y

  pb <- txtProgressBar(0, N, style=3)
  P <- matrix(1, N, ncol(X.TCGA))
  for (i in 1:N) {
    ind <- as.logical(sample(rep(0:1, each=length(y.TCGA)/2)))
    cvfit <- cv.glmnet(X.TCGA[ind,], y.TCGA[ind], nfolds=5)
    b <- coef(cvfit, s=cvfit$lambda.min)[-1]
    XX <- X.TCGA[!ind,which(b!=0)]
    yy <- y.TCGA[!ind]
    fit <- lm(yy~XX)
    summ <- summary(fit)$coefficients[-1,]
    var.id <- as.numeric(gsub("XXV", "", rownames(summ)))
    P[i, var.id] <- summ[,4]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(apply(P, 2, median))
}
