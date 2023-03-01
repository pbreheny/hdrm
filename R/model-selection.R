#' Reproduce Example 1.1
#'
#' Reproduces Example 1.1 from the book.  If you specify any options, your results may look different.
#'
#' @param N Number of simulated realizations
#' @param n Sample size
#' @param p Number of features
#'
#' @examples Ex1.1(N=10)
#'
#' @export

Ex1.1 <- function(N=100, n=25, p=100) {
  set.seed(1)
  xnam <- paste0("V", 1:p)
  form <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))

  res <- cover <- pred <- NULL
  pb <- txtProgressBar(0, N, style=3)
  for (i in 1:N) {
    X <- matrix(runif(n*p),n,p)
    Data <- as.data.frame(X)
    y <- rnorm(n)
    j <- which.max(abs(crossprod(X, y - mean(y))))
    XX <- matrix(runif(n*p),n,p)
    pData <- as.data.frame(XX)
    yy <- rnorm(n)
    fit0 <- lm(y~1, data=Data)
    fit <- step(fit0, scope=form, direction="forward", trace=0, k=log(n), steps=5)
    res <- rbind(res, summary(fit)$coef[-1,])
    pred <- c(pred, yy - predict(fit, pData))
    cover <- c(cover, apply(confint(fit)[-1,,drop=FALSE], 1, prod) <= 0)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  list(results=res, cover=cover, pred=pred)
}

#' Reproduce Figure 1.2
#'
#' Reproduces Figure 1.2 from the book.  If you specify any options, your results may look different.
#'
#' @param out  Output of Ex1.1()
#'
#' @examples
#' out <- Ex1.1(N=10)
#' Fig1.2(out)
#'
#' @export

Fig1.2 <- function(out) {
  if (missing(out)) stop("You need to run the code in Ex1.1() first and pass it to Fig1.2()")
  b <- out$results[,"Estimate"]
  hist(b, breaks=seq(-4.5, 4.5, .25), freq=FALSE, xlab=expression(hat(beta)), col="gray", border="white", main="", las=1)
}
