require(Sleuth3)
require(MASS)
require(ncvreg)
y <- ex1217$Mortality
X <- .Call("standardize", as.matrix(ex1217[,-(1:2)]), PACKAGE="ncvreg")[[1]]
rownames(X) <- ex1217[,1]
colnames(X) <- names(ex1217)[-(1:2)]

## Verify
n <- nrow(X)
lam <- 1
b <- lm.ridge(y~X, lambda=lam*n)$coef
W <- solve(crossprod(X)/n+lam*diag(ncol(X)))
cbind(W %*% crossprod(X,y-mean(y))/n, b)

## Fit
lam <- 10^(seq(-3, 3, length=49))
ll <- log10(lam)
fit <- lm.ridge(y~X, lambda=lam*n)
##fit <- ridge(y~X, lambda=lam)
pdf("ridge-trace.pdf", 6, 5)
par(mar=c(5,5,5,7))
matplot(ll, coef(fit)[,-1], lty=1, col=pal(15), type="l", lwd=3, xaxt="n", 
        xlab=expression(lambda), ylab="", las=1, xlim=c(3,-3), bty="n")
mtext(expression(hat(beta)), 2, 3, las=1)
at <- 10^c(3, 1, -1, -3)
axis(1, at=log10(at), labels=at)
ind <- abs(coef(fit)[1,-1]) > 15
text(x=log10(10^(-3.8)), y=coef(fit)[1,-1][ind], colnames(X)[ind], xpd=TRUE)
text(x=log10(10^(-3.8)), y=10, "SO2", xpd=TRUE)
abline(v=ll[which.min(fit$GCV)], col="gray")
ind <- seq(1, length(ll), length=5)
axis(3, at=ll[ind], labels=round(df[ind], 1))
mtext("Degrees of freedom", 3, 3)
dev.off()

## Prediction accuracy
XX <- cbind(1, X)
Y <- tcrossprod(XX, coef(fit))
d <- svd(X)$d
dx <- length(d)
div <- d^2 + rep(n*lam, rep(dx, length(lam)))
n <- nrow(X)
RSS <- colSums((y - Y)^2)
df <- colSums(matrix(d^2/div, dx))
GCV <- RSS/(1-df/n)^2
pdf("ridge-gcv.pdf", 5, 4)
par(mar=c(5,5,1,2))
col <- pal(2)
plot(ll, GCV/1000, type="l", lwd=3, xlim=c(4,-3), las=1, xlab=expression(lambda), xaxt="n", ylab="Prediction error", 
     col=col[2], bty="n", ylim=c(50,250))
at <- 10^c(3, 1, -1, -3)
axis(1, at=log10(at), labels=at)
lines(ll, RSS/1000, col=col[1], lwd=3)
text(-3.1, 105, "GCV", xpd=TRUE)
text(-3.1, 65, "RSS", xpd=TRUE)
dev.off()
lam[which.min(GCV)]

## Significance
ind <- which.min(GCV)
l <- lam[ind]
W <- solve(crossprod(X)/n+l*diag(ncol(X)))
b <- coef(fit)[ind,-1]
s2 <- RSS[ind]/(n-df[ind])
V <- s2/n*W
z <- b/sqrt(diag(V))
ord <- order(z, decreasing=TRUE)
fit.ols <- lm(y-mean(y)~0+., data=as.data.frame(X))
z.ols <- summary(fit.ols)$coef[,3]
Tab <- cbind(z, z.ols)[ord,]
rownames(Tab) <- colnames(X)[ord]
Tab
require(xtable)
xtable(Tab)
