#' Reproduce Example 1.1
#'
#' Reproduces Example 1.1 from the book.  If you specify any options, your results may look different.
#'
#' @param N  Number of simulated realizations
#' @param n  Sample size
#' @param p  Number of features
#' @param bar  Show progress bar?
#'
#' @examples
#' Ex1.1(N=10)
#' @export

Ex1.1 <- function(N=100, n=25, p=100, bar=TRUE) {
  set.seed(1)
  xnam <- paste0("V", 1:p)
  form <- as.formula(paste("y ~ ", paste(xnam, collapse= "+")))

  out <- expand.grid(coef=1:5, rep=1:N, estimate=NA_real_, p=NA_real_, cover=NA_integer_, pe=NA_real_, pex=NA_real_)
  pb <- progress::progress_bar$new(total = nrow(out))
  for (i in 1:nrow(out)) {
    if (i == 1 || out$rep[i] != out$rep[i-1]) {
      X <- runif(n*p) |> matrix(n, p)
      y <- rnorm(n)
      train <- as.data.frame(X)
      j <- which.max(abs(crossprod(X, y - mean(y))))
      test <- runif(n*p) |> matrix(n, p) |> as.data.frame()
      yy <- rnorm(n)
      fit0 <- lm(y~1, data=train)
      fit <- step(fit0, scope=form, direction="forward", trace=0, k=log(n), steps=5)
      smry <- summary(fit)$coef
      pe <- crossprod(yy - predict(fit, test))
      pex <- crossprod(yy - predict(fit, train))
      j <- 2
    } else {
      j <- j + 1
    }
    if (j <= length(coef(fit))) {
      out$estimate[i] <- coef(fit)[j]
      out$p[i] <- smry[j,4]
      out$cover[i] <- prod(confint(fit)[j,]) <= 0
      out$pe[i] <- pe
      out$pex[i] <- pex
    }
    if (bar) pb$tick()
  }
  out[!is.na(out$estimate), ]
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
#' @export

Fig1.2 <- function(out) {
  if (missing(out)) stop("You need to run the code in Ex1.1() first and pass it to Fig1.2()")
  hist(out$estimate, breaks=seq(-4.5, 4.5, .25), freq=FALSE, xlab=expression(hat(beta)), col="gray", border="white", main="", las=1)
}
