#' Reproduce Example 9.2
#'
#' Reproduces Example 9.2 from the book.  If you specify any options, your results may be different.
#'
#' @param N      Number of random splits
#' @param seed   Random seed for reproducibility
#'
#' @examples
#' p <- Ex9.2(N=10)
#' head(sort(p))

Ex9.2 <- function(N=100, seed=1) {
  Data <- readData("bcTCGA")
  X <- Data$X
  y <- Data$y
  p <- ncol(X)
  colnames(X) <- paste0("V", 1:p)
  X.TCGA <- X
  y.TCGA <- y

  pb <- txtProgressBar(0, N, style=3)
  P <- matrix(1, N, ncol(X.TCGA))
  for (i in 1:N) {
    ind <- as.logical(sample(rep(0:1, each=length(y.TCGA)/2)))
    cvfit <- cv.glmnet(X.TCGA[ind,], y.TCGA[ind], nfolds=5)
    b <- coef(cvfit, s=cvfit$lambda.min)[-1]
    XX <- X.TCGA[!ind,which(b!=0)]
    yy <- y.TCGA[!ind]
    fit <- lm(yy~XX)
    summ <- summary(fit)$coefficients[-1,]
    var.id <- as.numeric(gsub("XXV", "", rownames(summ)))
    P[i, var.id] <- summ[,4]
    setTxtProgressBar(pb, i)
  }
  close(pb)
  p <- apply(P, 2, median)
  names(p) <- colnames(Data$X)
  return(p)
}
