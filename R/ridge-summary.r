#' Reproduce Table 1.1
#'
#' Reproduces Table 1.1 from the book.  If you specify any options, your results may look different.
#'
#' @examples
#' Tab1.1()
#' @export

Tab1.1 <- function() {
  dat <- read_data("pollution")
  XX <- std(dat$X)
  y <- dat$y
  fit.ridge <- ridge(XX, y)
  t.ridge <- summary(fit.ridge, which=which.min(fit.ridge$loocv))[,4]
  fit.ols <- lm(y ~ ., data=as.data.frame(XX))
  t.ols <- summary(fit.ols)$coef[,3]
  out <- cbind(t.ridge, t.ols)[-1,]
  ord <- order(out[,1], decreasing=TRUE)
  out[ord,]
}
