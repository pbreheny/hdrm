#' Reproduce Figure 9.1
#'
#' Reproduces Figure 9.1 from the book.  If you specify any options, your results may look different.
#'
#' @param out   Output of Ex9.1()
#' @param N     Number of random splits
#' @param seed  Random number seed for reproducibility
#'
#' @examples
#'
#' Data <- Ex9.1()
#' out <- Fig9.1(Data)
#' Fig9.1(out)

Fig9.1 <- function(out, N=100, seed=1) {
  set.seed(seed)
  pb <- txtProgressBar(1, N, style=3)
  n <- nrow(out$X)
  p <- ncol(out$X)
  P <- matrix(1, N, p, dimnames=list(1:N, colnames(out$X)))
  for (i in 1:N) {
    ind <- as.logical(sample(rep(0:1, each=n/2)))
    cvfit <- cv.glmnet(out$X[ind,], out$y[ind])
    b <- coef(cvfit, s=cvfit$lambda.min)[-1]
    XX <- out$X[!ind,which(b!=0)]
    fit <- lm(out$y[!ind]~XX)
    summ <- summary(fit)$coefficients[-1,]
    var.id <- gsub("XX", "", rownames(summ))
    P[i, var.id] <- summ[,4]
    setTxtProgressBar(pb, i)
  }
  pval <- apply(P, 2, median)
  boxplot(pval[out$varType=="A"],
          pval[out$varType=="B"],
          pval[out$varType=="N"],
          col="gray", frame.plot=FALSE, pch=19,
          names=c("A", "B", "N"), las=1, ylim=c(0,1), ylab="p")
  return(invisible(pval))
}
