#' Reproduce Figure 3.1
#'
#' Reproduces Figure 3.1 from the book; if you specify any options, your results may look different.
#'
#' @param range     Range for beta coefficient (vector of length 2)
#' @param col       Lasso/ridge color (vector of length 2)
#' @param which     `left`, `middle`, `right`, or `all`
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples
#' Fig3.1()
#' Fig3.1(which='middle')
#' @export

Fig3.1 <- function(
    range = c(-4, 4),
    col = c("#FF4E37FF", "#00B500FF", "#008DFFFF"),
    which = c('all', 'left', 'middle', 'right', 'all'),
    parlist) {
  res <- 101
  x <- seq(range[1], range[2], len=res)
  xx <- seq(0.0001, range[2], len=res)
  g <- 3
  which <- match.arg(which)
  if (missing(parlist)) {
    if (which == 'all') {
      parlist <- list(mfrow=c(1,3), mar=c(5,5,0.5,0.5), xpd=1)
    } else {
      parlist <- list(mar=c(5,5,0.5,0.5), xpd=1)
    }
  }
  op <- par(parlist)
  on.exit(par(op))

  # Left: Penalty
  if (which == 'left' | which == 'all') {
    Y <- cbind(Lasso(x, 1), MCP(x, 1, g), SCAD(x, 1, g))
    matplot(x, Y, type='l', bty='n', lty=1, lwd=3, col=col, las=1,
            xlab=expression(beta), ylab=expression(P(beta*'|'*lambda,gamma)))
    text(2.3, 3.5, "Lasso")
    text(3.8, 2.4, "SCAD")
    text(3.8, 1.1, "MCP")
  }

  # Middle: Derivative
  if (which == 'middle' | which == 'all') {
    Y <- cbind(dLasso(xx, 1), dMCP(xx, 1, g), dSCAD(xx, 1, g))
    matplot(xx, Y, type='l', bty='n', lty=1, lwd=3, col=col, las=1,
            xlab=expression(beta), ylab=expression(dot(P)(beta*'|'*lambda,gamma)))
    text(3.5, 0.9, "Lasso")
    text(2.25, 0.7, "SCAD")
    text(0.9, 0.48, "MCP")
  }

  # Right: Solution
  if (which == 'right' | which == 'all') {
    Y <- cbind(soft(x,1), firmMCP(x,1,g), firmSCAD(x,1,g))
    matplot(x, Y, type='l', bty='n', lty=1, lwd=3, col=col, las=1,
            xlab="z", ylab=expression(hat(beta)))
    lines(c(-4,4),c(-4,4), col="gray70", lwd=1)
    text(3.7, 1.2, "Lasso")
    text(2.5, 3.5, "SCAD")
    text(1.4, 1.9, "MCP")
  }
}
