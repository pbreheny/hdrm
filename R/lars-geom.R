#' Reproduce Figure 2.4
#'
#' Reproduces Figure 2.4 from the book.
#'
#' @examples Fig2.4()

Fig2.4 <- function() {
  adj <- 0.07
  plot(0, 0, type="l", xaxt="n", yaxt="n", xlim=c(-0.05, 1.05), ylim=c(-0.07, 0.85), bty="n", xlab="", ylab="")
  arrows(0,0,1,0)
  text(1+adj, -adj, expression(bold(x)[1]), xpd=TRUE)
  arrows(0,0,1/2,sqrt(3)/2)
  text(1/2+adj, sqrt(3)/2-adj, expression(bold(x)[2]), xpd=TRUE)

  points(0, 0, pch=19)
  text(-adj, -adj, expression(bold(y)), xpd=TRUE)
  points(0.8,0,pch=19)
  text(0.8-adj, -adj, expression(bar(bold(y))[1]), xpd=TRUE)

  arrows(0.4, 0, 0.4+sqrt(3)/3, 1/3)
  text(0.4+sqrt(3)/3+adj, 1/3-adj, expression(bar(bold(y))[2]), xpd=TRUE)
  points(0.8,0,pch=19)
  points(0.4+sqrt(3)/3, 1/3, pch=19)

  col <- "#008DFFFF"
  lines(c(0, 0.4), c(0,0), col=col, lwd=3)
  lines(c(0.4, 0.4+sqrt(3)/3), c(0, 1/3), col=col, lwd=3)

  lines(c(0.4, 0.4+1/2), c(0, sqrt(3)/2), col="gray", lty=2, lwd=3)
}
