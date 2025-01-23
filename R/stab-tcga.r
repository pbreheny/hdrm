#' Reproduce Figure 9.3
#'
#' Reproduces Figure 9.3 from the book.  If you specify any options, your results may look different.
#'
#' @param N     Number of simulated realizations
#' @param seed  Random number seed for reproducibility
#'
#' @examples
#' res <- Fig9.3(N=10)
#' S <- res$Stability
#' s <- apply(S, 1, max)
#' head(sort(s[s>0.6], decreasing=TRUE), n=20)
#'
#' @export

Fig9.3 <- function(N=100, seed=1) {
  Data <- read_data("brca1")
  X <- Data$X
  y <- Data$y
  p <- ncol(X)
  n <- nrow(X)
  fit <- glmnet(X, y)
  pb <- txtProgressBar(0, N, style=3)
  SS <- array(NA, dim=c(N, p, length(fit$lambda)), dimnames=list(1:N, colnames(X), fit$lambda))
  Q <- matrix(NA, N, length(fit$lambda))
  set.seed(seed)
  for (i in 1:N) {
    ind <- sample(1:n, replace=TRUE)
    #ind <- as.logical(sample(rep(0:1, each=n/2)))
    fit.i <- glmnet(X[ind,], y[ind], lambda=fit$lambda)
    SS[i,,] <- as.matrix(coef(fit.i)[-1,]!=0)
    Q[i,] <- sapply(predict(fit.i, type="nonzero"), length)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  S <- apply(SS, 2:3, mean)
  colnames(S) <- lam_names(fit$lambda)
  q <- apply(Q, 2, mean)
  sMax <- apply(S, 1, max)

  l <- fit$lambda
  col <- rep(rgb(0.6, 0.6, 0.6, 0.25), p)
  col[sMax > 0.6] <- "red"
  matplot(l, t(S), type="l", lty=1, xlim=rev(range(l)), col=col, lwd=2, las=1, bty="n",
          xlab=expression(lambda), ylab="Stability")

  return(invisible(list(Selected=q, Stability=S)))
}
