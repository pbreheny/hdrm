Data <- genDataABN(n=100, p=20, a=2, b=3)
dim(Data$X)
length(Data$y)
with(Data, data.frame(beta, varType))

genDataABN(100, 10, 2, 1)$beta
genDataABN(100, 10, 2, 1, rho=0.9)$beta
genDataABN(100, 10, 2, 1, rho=0.9, rho.noise=0.9)$beta
genDataABN(100, 10, 2, 1, SNR=3)$beta
genDataABN(100, 10, 2, 1, SNR=3, signal='het')$beta
genDataABN(100, 10, 2, 1, beta=3)$beta
genDataABN(100, 10, 2, 1, beta=10:1)$beta

genDataABN(10, 20, 2, 3, family='binomial')$y

\dontshow{\dontrun{
  require(magrittr)
  genDataABN(1000, 10, 2, 2, rho=0.5, rho.noise=0.0, noise='exch')$X %>% cor %>% round(digits=2)
  genDataABN(1000, 10, 2, 2, rho=0.5, rho.noise=0.5, noise='exch')$X %>% cor %>% round(digits=2)
  genDataABN(1000, 10, 2, 2, rho=0.5, rho.noise=0.9, noise='auto')$X %>% cor %>% round(digits=2)
}}
