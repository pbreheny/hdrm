#' Reproduce Figure 9.2
#'
#' Reproduces Figure 9.2 from the book. If you specify any options, your results may look different.
#'
#' @param out   Output of Ex9.1
#' @param N     Number of simulated realizations
#' @param seed  Random number seed for reproducibility
#'
#' @examples
#' out <- Ex9.1()
#' res <- Fig9.2(out)
#' head(sort(res$Stability[,20], decreasing=TRUE))
#' @export

Fig9.2 <- function(out, N=100, seed=1) {
  X <- out$X
  y <- out$y
  p <- ncol(X)
  n <- nrow(X)
  fit <- glmnet(X, y)
  pb <- txtProgressBar(0, N, style=3)
  SS <- array(NA, dim=c(N, p, length(fit$lambda)), dimnames=list(1:N, colnames(X), fit$lambda))
  Q <- matrix(NA, N, length(fit$lambda))
  set.seed(seed)
  for (i in 1:N) {
    ind <- sample(1:n, replace=TRUE)
    fit.i <- glmnet(X[ind,], y[ind], lambda=fit$lambda)
    SS[i,,] <- as.matrix(coef(fit.i)[-1,]!=0)
    Q[i,] <- sapply(predict(fit.i, type="nonzero"), length)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  S <- apply(SS, 2:3, mean)
  colnames(S) <- lam_names(fit$lambda)
  q <- apply(Q, 2, mean)
  names(q) <- colnames(S)

  l <- fit$lambda
  col <- rep(rgb(0.6, 0.6, 0.6, 0.25), p)
  col[out$varType=="A"] <- "red"
  matplot(l, t(S), type="l", lty=1, xlim=rev(range(l)), col=col, lwd=2, las=1, bty="n",
          xlab=expression(lambda), ylab="Stability")

  return(invisible(list(Selected=q, Stability=S)))
}
