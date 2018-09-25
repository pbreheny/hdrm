#' Reproduce Figure 3.3
#'
#' Reproduces Figure 3.3 from the book; if you specify any options, your results may look different.
#'
#' @param N         Number of simulations
#' @param parlist   List of arguments to pass to `par()`
#'
#' @examples Fig3.3()

Fig3.3 <- function(N=100000, parlist=list(mfrow=c(1,2), mar=c(5,5,1.5,0.5), oma=c(0,0,2,0))) {
  # MCP
  gam1 <- seq(1+1e-6, 9, len=99)
  G <- length(gam1)
  Bias <- Var <- numeric(G)
  for (i in 1:G) {
    z <- rnorm(N, mean=1, sd=0.6)
    b <- firmMCP(z, 1, gam1[i])
    Bias[i] <- mean(b) - 1
    Var[i] <- var(b)
  }
  MCP <- cbind(Bias^2, Var, Bias^2 + Var)

  # SCAD
  gam2 <- seq(2+1e-6, 10, len=99)
  G <- length(gam2)
  Bias <- Var <- numeric(G)
  for (i in 1:G) {
    z <- rnorm(N, mean=1, sd=0.6)
    b <- firmSCAD(z, 1, gam2[i])
    Bias[i] <- mean(b) - 1
    Var[i] <- var(b)
  }
  SCAD <- cbind(Bias^2, Var, Bias^2 + Var)

  op <- par(parlist)
  matplot(gam1, MCP, type='l', lwd=3, col=pal(3), lty=1, las=1, ylab='', xlab=expression(gamma), bty='n', xaxt='n')
  axis(1, at=seq(1,9,2))
  mtext('MCP', line=0.5)
  matplot(gam2, SCAD, type='l', lwd=3, col=pal(3), lty=1, las=1, ylab='', xlab=expression(gamma), bty='n')
  mtext('SCAD', line=0.5)
  toplegend(legend=c(expression("Bias"^2), "Var", "MSE"), lwd=3, col=pal(3))
  par(op)
}
