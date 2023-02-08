.test = "ridge works for linear regression"
n <- 50
p <- 10
X <- matrix(rnorm(n*p), n, p)
b <- rnorm(p)
y <- rnorm(n, X%*%b)
b0 <- lm(y~X)$coef
b1 <- coef(ridge(y~X, lambda=0))
check(b0, b1)
b1 <- coef(ridge(y~X, lambda=1e9))
check(b1, c(mean(y), rep(0, p)), tol=1e-6)

.test = "ridge.formula, ridge.matrix work and agree"
b1 <- coef(ridge(y~X))
b2 <- coef(ridge(X, y))
check(b1,b2)

.test = "summary.ridge is correct"
s0 <- summary(lm(y~X))$coef
s1 <- summary(ridge(y~X, lambda=0))
check(s0[,1:3], as.matrix(s1))

.test = "confint.ridge is correct"
c0 <- confint(lm(y~X))
c1 <- confint(ridge(y~X, lambda=0))
check(c0, c1)

.test = "predict.ridge is correct"
p0 <- predict(lm(y~., data=as.data.frame(X)), as.data.frame(X[8:10,]))
p1 <- predict(ridge(y~X, lambda=0), X[8:10,])
check(p0, p1)

.test = "plot.ridge works"
plot(ridge(y~X))
