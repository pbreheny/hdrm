Fig9.1 <- function(res, N=100) {
  pb <- txtProgressBar(1, N, style=3)
  n <- nrow(res$X)
  p <- ncol(res$X)
  P <- matrix(1, N, p)
  for (i in 1:N) {
    ind <- as.logical(sample(rep(0:1, each=n/2)))
    cvfit <- cv.glmnet(res$X[ind,], res$y[ind])
    b <- coef(cvfit, s=cvfit$lambda.min)[-1]
    XX <- res$X[!ind,which(b!=0)]
    fit <- lm(res$y[!ind]~XX)
    summ <- summary(fit)$coefficients[-1,]
    var.id <- as.numeric(gsub("XXV", "", rownames(summ)))
    P[i, var.id] <- summ[,4]
    setTxtProgressBar(pb, i)
  }
  pval <- apply(P, 2, median)
  boxplot(pval[res$varType=="A"],
          pval[res$varType=="B"],
          pval[res$varType=="N"],
          col="gray", frame.plot=FALSE, pch=19,
          names=c("A", "B", "N"), las=1, ylim=c(0,1), ylab="p")
  return(invisible(pval))
}
