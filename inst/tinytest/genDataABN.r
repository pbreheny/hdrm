if (interactive()) library(tinytest)

Data <- genDataABN(n=100, p=20, a=2, b=3)
expect_equal(dim(Data$X), c(100, 20))
expect_equal(length(Data$y), 100)
expect_equal(Data$varType[1:8], rep(c('A', 'B', 'B', 'B'), 2))
with(Data, data.frame(beta, varType))

genDataABN(100, 10, 2, 1)$beta
genDataABN(100, 10, 2, 1, rho=0.9)$beta
genDataABN(100, 10, 2, 1, rho=0.9, rho.noise=0.9)$beta
genDataABN(100, 10, 2, 1, SNR=3)$beta
genDataABN(100, 10, 2, 1, SNR=3, signal='het')$beta
genDataABN(100, 10, 2, 1, beta=3)$beta
genDataABN(100, 10, 2, 1, beta=2:1)$beta

genDataABN(10, 20, 2, 3, family='binomial')$y

genDataABN(1000, 10, 2, 2, rho=0.25, rho.noise=0.0, noise='exch')$X |> cor() |> round(digits=2)
genDataABN(1000, 10, 2, 2, rho=0.5, rho.noise=0.5, noise='exch')$X |> cor() |> round(digits=2)
genDataABN(1000, 10, 2, 2, rho=0.75, rho.noise=0.9, noise='auto')$X |> cor() |> round(digits=2)
