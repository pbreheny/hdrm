Fig1.1 n=20, p=n-1) {
  set.seed(seed)
  X <- std(matrix(rnorm(n*p),n,p))
  bigVar <- numeric(p)
  for (i in 1:p) {
    bigVar[i] <- max(diag(solve(crossprod(X[,1:i]))))
  }
  plot(1:p, log(20*bigVar, 2), yaxt="n", ylab="Largest variance", xlab="Number of columns included",
       pch=19, bty="n", xlim=c(0,n))
  logAxis(2, base=2)
  invisible(20*bigVar)
}
