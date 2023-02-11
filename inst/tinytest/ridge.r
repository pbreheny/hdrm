if (interactive()) library(tinytest)


# ridge works for linear regression
n <- 50
p <- 10
X <- matrix(rnorm(n*p), n, p)
b <- rnorm(p)
y <- rnorm(n, X%*%b)
b0 <- lm(y~X)$coef
b1 <- coef(ridge(y~X, lambda=0))
expect_equal(b0, b1)
b1 <- coef(ridge(y~X, lambda=1e9))
expect_equivalent(b1, c(mean(y), rep(0, p)), tol=1e-6)

# ridge.formula, ridge.matrix work and agree
b1 <- coef(ridge(y~X))
b2 <- coef(ridge(X, y))
expect_equivalent(b1, b2)

# summary.ridge is correct
s0 <- summary(lm(y~X))$coef
s1 <- summary(ridge(y~X, lambda=0))
expect_equivalent(s0, as.matrix(s1[,-2]))

# confint.ridge is correct
c0 <- confint(lm(y~X))
c1 <- confint(ridge(y~X, lambda=0))
expect_equivalent(c0, c1)

# predict.ridge is correct
p0 <- predict(lm(y~., data=as.data.frame(X)), as.data.frame(X[8:10,]))
p1 <- predict(ridge(y~X, lambda=0), X[8:10,])
expect_equivalent(p0, p1)

# plot.ridge works
expect_silent(plot(ridge(y~X)))
