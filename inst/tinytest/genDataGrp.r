Data <- genDataGrp(100, 10, 5, J1=3, K1=2)
dim(Data$X)
head(Data$y)
Data$beta
Data$group

genDataGrp(100, 3, 3, J1=2, K1=2)$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2)$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8)$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het')$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het', signal.g='het')$beta
genDataGrp(100, 3, 3, J1=2, K1=2, SNR=2, rho=0.8, signal='het', b=1)$beta

\dontshow{\dontrun{
  require(magrittr)
  genDataGrp(1000, 3, 3, rho=0)$X %>% cor %>% round(digits=2)
  genDataGrp(1000, 3, 3, rho=0.7)$X %>% cor %>% round(digits=2)
  genDataGrp(1000, 3, 3, rho=0.3, rho.g=0.8)$X %>% cor %>% round(digits=2)
}}
