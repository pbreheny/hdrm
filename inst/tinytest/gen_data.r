if (interactive()) library(tinytest)

## Dimension check
dat <- gen_data(100, 100, 10)
expect_equal(dim(dat$X), c(100, 100))
expect_equal(length(dat$y), 100)
expect_equal(length(dat$beta), 100)

## Y checks
expect_true(abs(sd(dat$y - dat$X %*% dat$beta) - 1) < .1)
expect_true(all(gen_data(10, 10, 5, family='binomial')$y %in% c(0, 1)))
# head(dat$beta)

## Beta Checks
b1 <- gen_data(100, 10, 5)$beta
expect_equal(sum(b1 != 0), 5)
expect_equal(sum(b1^2), 1)

b2 <- gen_data(100, 10, 5, SNR=2)$beta
expect_true(all(abs((b2[1:5] / b1[1:5])^2 - 2) < 1e-9))
expect_equal(sum(b2^2), 2)

b3 <- gen_data(100, 10, 5, SNR=2, corr='exch', rho=0.7)$beta
expect_true(all(b2 == b3))
expect_equal(sum(b3^2), 2)

b4 <- gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.7)$beta
expect_true(all(b2 == b4))
expect_equal(sum(b4^2), 2)

b5 <- gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.7, signal='het')$beta
expect_equal(length(unique(b5)), 6)
expect_true(all(table(sign(b5)) == c(2, 5, 3)))
expect_equal(sum(b5^2), 2)

b6 <- gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het')$beta
expect_equal(length(unique(b6)), 6)
expect_true(all(table(sign(b6)) == c(2, 5, 3)))
expect_equal(sum(b6^2), 2)

b7 <- gen_data(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het', b=1)$beta
expect_true(all(b7 %in% c(-1, 0, 1)))
expect_true(all(table(sign(b7)) == c(2, 5, 3)))

## X checks
all(abs(gen_data(10000, 10, rho=0.0, corr='exch')$X |> cor() |> round(digits=2) - diag(10)) < .05)
all(abs(gen_data(10000, 10, rho=0.7, corr='exch')$X |> cor() |> round(digits=2) - .7 - diag(10)*.3) < .05)
abs(sum(gen_data(10000, 10, rho=0.7, corr='auto')$X |> cor() |> round(digits=2)) - sum(rep(.7^(1:9), (9:1) * 2)) - 10) < (.01 * 100)
all(abs(gen_data(10000, 3, 3, rho=0)$X |> cor() |> round(digits=2) - diag(3)) < .05)

# timing check: exch
n <- 100
p <- 3000
p1 <- 5
rho <- 0.5
res <- bench::mark(
  genData(n, p, p1, rho=rho, corr='exchangeable'),
  gen_data(n, p, p1, rho=rho, corr='exchangeable'),
  check=FALSE, time_unit = "ms")
expect_true(res$median[1] / res$median[2] > 100)
# summary(res, relative = TRUE)
# ggplot2::autoplot(res)

# timing check: auto
n <- 100
p <- 3000
p1 <- 5
rho <- 0.5
res <- bench::mark(
  genData(n, p, p1, rho=rho, corr='autoregressive'),
  gen_data(n, p, p1, rho=rho, corr='autoregressive'),
  check=FALSE, time_unit = "ms")
# summary(res)
# summary(res, relative = TRUE)
# ggplot2::autoplot(res)
expect_true(res$median[1] / res$median[2] > 100)

# bsb
rho <- 0.4
b <- rnorm(10)
E <- (1-rho)*diag(10) + matrix(rho, 10, 10)
expect_equal(crossprod(b, E) %*% b |> drop(), calc_bsb(b, rho, 'exchangeable'))
rho <- 0.9
RHO <- matrix(rho^(0:9), 10, 10, byrow=TRUE)
A <- Matrix::bandSparse(10, k=0:9, diagonals=RHO, symmetric=TRUE)
expect_equal(crossprod(b, A) %*% b |> drop(), calc_bsb(b, rho, 'autoregressive'))
