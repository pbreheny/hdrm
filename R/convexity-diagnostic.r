#' Reproduce Figure 3.6
#'
#' Reproduces Figure 3.6 from the book.  If you specify any options, your results may look different.
#'
#' @param seed      Random seed for reproducibility
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig3.6()
#' @export

Fig3.6 <- function(seed=9, parlist=list(mfrow=c(1,2), mar=c(5,5,0.5,0.5))) {
  op <- par(parlist)
  set.seed(seed)
  Data <- gen_data(20, 50, 4)
  fit <- with(Data, ncvreg(X, y, nlambda=500))
  plot(fit, bty='n')

  # Exploratory
  X <- std(Data$X)
  y <- with(Data, y - mean(y))
  fit <- ncvreg(X, y, nlambda=500)
  ind1 <- fit$convex.min
  ind2 <- fit$convex.min +1
  l1 <- fit$lambda[ind1]
  l2 <- fit$lambda[ind2]
  b1 <- coef(fit, which=ind1)[-1]
  b2 <- coef(fit, which=ind2)[-1]
  n <- length(y)
  Q <- function(b, lam) {
    r <- y - X%*%b
    1/(2*n)*crossprod(r) + sum(MCP(b, lam))
  }
  x <- seq(-0.5,1.5,len=101)
  q1 <- q2 <- q3 <- length(x)
  for (i in 1:length(x)) {
    q1[i] <- Q(x[i]*b2 + (1-x[i])*b1, 1.2*l1)
    q2[i] <- Q(x[i]*b2 + (1-x[i])*b1, l1/2)
    q3[i] <- Q(x[i]*b2 + (1-x[i])*b1, 2*l1)
  }
  mar <- par()$mar
  mar[4] <- 5
  par(mar=mar)
  plot(x, q1, type='l', las=1, ylim=c(0, 1), bty='n', xaxt='n', ylab=expression(Q(beta)), xlab='')
  axis(1, at=c(-0.5, 0, 1, 1.5), labels=expression("", beta[1], beta[2], ""))
  lines(x, q2)
  lines(x, q3)
  text(1.9, tail(q1,1), expression(lambda==0.25), xpd=1)
  text(1.9, tail(q2,1), expression(lambda==0.11), xpd=1)
  text(1.9, tail(q3,1), expression(lambda==0.42), xpd=1)
  par(op)
}
