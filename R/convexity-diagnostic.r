#' Reproduce Figure 3.6
#'
#' Reproduces Figure 3.6 from the book. If you specify any options, your results
#' may look different. For this example in particular, changing the seed is
#' unlikely to result in an interesting plot.
#'
#' @param which     `both`, `left`, or `right`
#' @param seed      Random seed for reproducibility
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' Fig3.6()
#' Fig3.6(which='right')
#' @export

Fig3.6 <- function(which=c('both', 'left', 'right'), seed=9, parlist) {
  which <- match.arg(which)
  if (missing(parlist)) {
    if (which == 'both') {
      parlist <- list(mfrow=c(1,2), mar=c(5,5,0.5,0.5))
    } else {
      parlist <- list(mar=c(5,5,0.5,0.5))
    }
  }
  op <- par(parlist)
  on.exit(par(op))
  original_seed <- .GlobalEnv$.Random.seed
  on.exit(.GlobalEnv$.Random.seed <- original_seed)
  set.seed(seed)

  Data <- gen_data(20, 50, 4)
  X <- ncvreg::std(Data$X)
  y <- with(Data, y - mean(y))
  fit <- ncvreg(X, y, nlambda=500, eps = 1e-10, max.iter = 1e5)

  if (which == 'both' | which == 'left') {
    plot(fit, bty='n')
  }

  if (which == 'both' | which == 'right') {
    ind1 <- fit$convex.min
    ind2 <- fit$convex.min + 1
    l1 <- fit$lambda[ind1]
    l2 <- fit$lambda[ind2]
    b1 <- coef(fit, which=ind1)[-1]
    b2 <- coef(fit, which=ind2)[-1]
    n <- length(y)
    Q <- function(b, lam) {
      r <- y - X%*%b
      1/(2*n)*crossprod(r) + sum(MCP(b, lam))
    }
    x <- seq(-0.5, 1.5, len=101)
    q1 <- q2 <- q3 <- length(x)
    l <- c(0.21, 0.27, 0.35)
    for (i in 1:length(x)) {
      q1[i] <- Q(x[i]*b2 + (1-x[i])*b1, l[1])
      q2[i] <- Q(x[i]*b2 + (1-x[i])*b1, l[2])
      q3[i] <- Q(x[i]*b2 + (1-x[i])*b1, l[3])
    }
    mar <- par()$mar
    mar[4] <- 5
    par(mar=mar)
    plot(x, q1, type='l', las=1, ylim=c(0.5, 1), bty='n', xaxt='n', ylab=expression(Q(beta)), xlab='')
    axis(1, at=c(-0.5, 0, 1, 1.5), labels=expression("", beta[1], beta[2], ""))
    lines(x, q2)
    lines(x, q3)
    text(1.9, tail(q1, 1), bquote(lambda == .(l[1])), xpd=1)
    text(1.9, tail(q2, 1), bquote(lambda == .(l[2])), xpd=1)
    text(1.9, tail(q3, 1), bquote(lambda == .(l[3])), xpd=1)
  }
}
