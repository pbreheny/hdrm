if (interactive()) library(tinytest)

Data <- genDataGrp(100, 10, 5, J1=3, K1=2)
expect_equal(dim(Data$X), c(100, 50))
head(Data$y)
B <- matrix(Data$beta, ncol=10)
expect_false(any(B[1:2, 1:3]==0))
expect_true(all(B[3:5, 1:3]==0))
expect_true(all(B[, 4:10]==0))
expect_equal(Data$group, rep(1:10, each=5))

genDataGrp(100, 3, 3, J1=2, K1=2)$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2)$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8)$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het')$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het', signal.g='het')$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het', b=1)$beta

genDataGrp(1000, 3, 3, rho=0)$X |> cor() |> round(digits=2)
genDataGrp(1000, 3, 3, rho=0.7)$X |> cor() |> round(digits=2)
genDataGrp(1000, 3, 3, rho=0.3, rho.g=0.8)$X |> cor() |> round(digits=2)
