if (interactive()) library(tinytest)

Data <- gen_data_abn(n=100, p=20, a=2, b=3)
expect_equal(dim(Data$X), c(100, 20))
expect_equal(length(Data$y), 100)
expect_equal(Data$varType[1:8], rep(c('A', 'B', 'B', 'B'), 2))
with(Data, data.frame(beta, varType))

gen_data_abn(100, 10, 2, 1)$beta
gen_data_abn(100, 10, 2, 1, rho=0.9)$beta
gen_data_abn(100, 10, 2, 1, rho=0.9, rho.noise=0.9)$beta
gen_data_abn(100, 10, 2, 1, SNR=3)$beta
gen_data_abn(100, 10, 2, 1, SNR=3, signal='het')$beta
gen_data_abn(100, 10, 2, 1, beta=3)$beta
gen_data_abn(100, 10, 2, 1, beta=2:1)$beta

gen_data_abn(10, 20, 2, 3, family='binomial')$y

gen_data_abn(1000, 10, 2, 2, rho=0.25, rho.noise=0.0, noise='exch')$X |> cor() |> round(digits=2)
gen_data_abn(1000, 10, 2, 2, rho=0.5, rho.noise=0.5, noise='exch')$X |> cor() |> round(digits=2)
gen_data_abn(1000, 10, 2, 2, rho=0.75, rho.noise=0.9, noise='auto')$X |> cor() |> round(digits=2)

# timing check: exch
n <- 100
p <- 3000
a <- 5
b <- 2
rho <- 0.5
res <- bench::mark(
  genDataABN(n, p, a, b, rho=rho, noise='exchangeable'),
  gen_data_abn(n, p, a, b, rho=rho, noise='exchangeable'),
  check=FALSE)
summary(res)
summary(res, relative = TRUE)
ggplot2::autoplot(res)

# timing check: auto
n <- 100
p <- 3000
a <- 5
b <- 2
rho <- 0.5
res <- bench::mark(
  genDataABN(n, p, a, b, rho=rho, noise='autoregressive'),
  gen_data_abn(n, p, a, b, rho=rho, noise='autoregressive'),
  check=FALSE)
summary(res)
summary(res, relative = TRUE)
ggplot2::autoplot(res)

