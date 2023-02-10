if (interactive()) library(tinytest)

Data <- genData(100, 100, 10)
expect_equal(dim(Data$X), c(100, 100))
head(Data$y)
head(Data$beta)

genData(100, 10, 5)$beta
genData(100, 10, 5, SNR=2)$beta
genData(100, 10, 5, SNR=2, corr='exch', rho=0.7)$beta
genData(100, 10, 5, SNR=2, corr='auto', rho=0.7)$beta
genData(100, 10, 5, SNR=2, corr='auto', rho=0.7, signal='het')$beta
genData(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het')$beta
genData(100, 10, 5, SNR=2, corr='auto', rho=0.1, signal='het', b=1)$beta

genData(10, 10, 5, family='binomial')$y

genData(1000, 10, rho=0.0, corr='exch')$X |> cor() |> round(digits=2)
genData(1000, 10, rho=0.7, corr='exch')$X |> cor() |> round(digits=2)
genData(1000, 10, rho=0.7, corr='auto')$X |> cor() |> round(digits=2)
genData(1000, 3, 3, rho=0)$X |> cor() |> round(digits=2)
