Ex9.1 <- function(n=100, p=60, n.corr=2, rho=0.5, seed=1) {
  set.seed(seed)
  genDataABN(n=n, p=p, b=n.corr, rho=rho, beta=1)
}
