res <- Ex9.1()

Fig9.1(res, N=10)

sum(Ex9.2(N=3) < 0.05)

out <- Fig9.2(res, N=100)

# TCGA
require(glmnet)
fit <- glmnet(X.TCGA, y.TCGA, lambda.min=0.1)
pb <- txtProgressBar(1, 100, style=3)
SS <- array(NA, dim=c(100, ncol(X.TCGA), length(fit$lambda)), dimnames=list(1:100, colnames(X.TCGA), fit$lambda))
Q <- matrix(NA, 100, length(fit$lambda))
for (i in 1:100) {
  ind <- as.logical(sample(rep(0:1, each=length(y.TCGA)/2)))
  fit.i <- glmnet(X.TCGA[ind,], y.TCGA[ind], lambda=fit$lambda)
  SS[i,,] <- as.matrix(coef(fit.i)[-1,]!=0)
  Q[i,] <- sapply(predict(fit.i, type="nonzero"), length)
  setTxtProgressBar(pb, i)
}
S <- apply(SS, 2:3, mean)
q <- apply(Q, 2, mean)
l <- fit$lambda
col <- rep("gray", ncol(X.TCGA))
col[which(apply(S, 1, max) > 0.6)] <- "red"
png("stability2.png", 6, 5, units="in", res=150)
par(mar=c(5,5,1,1))
matplot(l, t(S), type="l", lty=1, xlim=rev(range(l)), col=col, lwd=2, las=1, bty="n",
        xlab=expression(lambda), ylab="Stability")
dev.off()
table(col)

EV <- q^2/(p*(2*.9-1))
FDR <- EV/sapply(predict(fit, type="nonzero"), length)
max(which(FDR < .1))
fit$lambda[max(which(FDR < .1))]
