if (interactive()) library(tinytest)

dat <- gen_data(100, 100, 10)
expect_equal(dim(dat$X), c(100, 100))
head(dat$y)
head(dat$beta)

gen_data(100, 10, 5)$beta
gen_data(100, 10, 5, SNR=2)$beta
gen_data(100, 10, 5, SNR=2, corr='exch', rho=0.7)$beta
gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.7)$beta
gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.7, signal='het')$beta
gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het')$beta
gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het', b=1)$beta

gen_data(10, 10, 5, family='binomial')$y

gen_data(1000, 10, rho=0.0, corr='exch')$X |> cor() |> round(digits=2)
gen_data(1000, 10, rho=0.7, corr='exch')$X |> cor() |> round(digits=2)
gen_data(1000, 10, rho=0.7, corr='auto')$X |> cor() |> round(digits=2)
gen_data(1000, 3, 3, rho=0)$X |> cor() |> round(digits=2)

# timing check: exch
n <- 100
p <- 3000
p1 <- 5
rho <- 0.5
res <- bench::mark(
  genData(n, p, p1, rho=rho, corr='exchangeable'),
  gen_data(n, p, p1, rho=rho, corr='exchangeable'),
  check=FALSE)
summary(res)
summary(res, relative = TRUE)
ggplot2::autoplot(res)

# timing check: auto
n <- 100
p <- 3000
p1 <- 5
rho <- 0.5
res <- bench::mark(
  genData(n, p, p1, rho=rho, corr='autoregressive'),
  gen_data(n, p, p1, rho=rho, corr='autoregressive'),
  check=FALSE)
summary(res)
summary(res, relative = TRUE)
ggplot2::autoplot(res)

# bsb
rho <- 0.4
b <- rnorm(10)
E <- (1-rho)*diag(10) + matrix(rho, 10, 10)
expect_equal(crossprod(b, E) %*% b |> drop(), hdrm:::calc_bsb(b, rho, 'exchangeable'))
rho <- 0.9
RHO <- matrix(rho^(0:9), 10, 10, byrow=TRUE)
A <- Matrix::bandSparse(10, k=0:9, diagonals=RHO, symmetric=TRUE)
expect_equal(crossprod(b, A) %*% b |> drop(), hdrm:::calc_bsb(b, rho, 'autoregressive'))
