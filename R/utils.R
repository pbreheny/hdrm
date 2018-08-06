array2df <- function(X,vars=paste("V",1:ncol(df),sep="")) {
  df <- cbind(do.call("expand.grid",dimnames(X)),as.numeric(X))
  names(df) <- vars
  df
}
logAxis <- function(side=1, base, disp=base, n=5, style=NULL, ...) {
  if (missing(base)) base <- exp(1)
  is.x <- side%%2 == 1
  usr <- if(is.x) par("usr")[1:2] else par("usr")[3:4]
  r <- usr*log(base)/log(disp)
  px <- pretty(r, n=n)
  if (r[2] > r[1]) {
    px <- px[px > r[1] & px < r[2]]
  } else {
    px <- px[px < r[1] & px > r[2]]
  }
  if (is.null(style)) style <- if (isTRUE(all.equal(px, as.integer(px)))) "pow" else "dec"
  if (style=="pow") {
    at <- px*log(disp)/log(base)
    lab <- disp^px
  } else {
    s <- seq(usr[1], usr[2], len=n)
    s <- s - s[which.min(abs(s))]
    at <- s/max(range(s)/usr[1:2])
    lab <- formatC(base^at, digits=1, format="f")
  }
  a <- axis(side, at=at, labels=lab, las=1, ...)
}
toplegend <- function(horiz=TRUE, ...) {
  if (par("oma")[3]==0) {
    x <- mean(par("usr")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    y  <- mean(c(yy[2],par("usr")[4]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, horiz=horiz, ...)
  } else {
    g <- par("mfrow")
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform.coord(xx, c(g[2]-1,g[2])/g[2])
    yyy <- transform.coord(yy, c(g[1]-1,g[1])/g[1])
    yyyy <- transform.coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2],yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, horiz=horiz, ...)
  }
}
rightlegend <- function(...) {
  if (par("oma")[3]==0) {
    y <- mean(par("usr")[3:4])
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    x <- mean(c(xx[2],par("usr")[2]))
    legend(x, y, xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)
  } else {
    g <- par("mfrow")
    xx <- transform.coord(par("usr")[1:2], par("plt")[1:2])
    yy <- transform.coord(par("usr")[3:4], par("plt")[3:4])
    xxx <- transform.coord(xx, c(g[2]-1,g[2])/g[2])
    yyy <- transform.coord(yy, c(g[1]-1,g[1])/g[1])
    yyyy <- transform.coord(yyy, par("omd")[3:4])
    legend(mean(xxx), mean(c(yyy[2],yyyy[2])), xpd=NA, bty="n", xjust=0.5, yjust=0.5, ...)
  }
}
transform.coord <- function(x,p) {
  ba <- (x[2]-x[1])/(p[2]-p[1])
  a <- x[1]-p[1]*ba
  b <- a + ba
  c(a,b)
}
pal <- function(n, alpha=1) {
  if (n==2) {
    val <- hcl(seq(15,375,len=4), l=60, c=150, alpha=alpha)[c(1,3)]
  } else val <- hcl(seq(15,375,len=n+1), l=60, c=150, alpha=alpha)[1:n]
  val
}
lfdrPlot <- function(z, pi0=1, delta=0, sigma=1, lfdrReturn=TRUE, ...) {
  # Calculation
  dens <- density(z, bw="nrd")
  f <- approxfun(dens$x, dens$y)
  f0 <- function(z) pi0*dnorm(z, mean=delta, sd=sigma)
  lfdr <- pmin(f0(z)/f(z), 1)

  # Plot
  h <- hist(z, breaks=seq(min(z), max(z), length = 99), plot=FALSE)
  zz <- seq(min(z), max(z), len=299)
  ylim=c(0, max(c(h$density, dens$y, f0(delta))))
  #plot(h, main="", border=FALSE, col="lightgray", freq=FALSE, las=1, ylim=ylim)
  plot(h, main="", border=FALSE, col="lightgray", freq=FALSE, las=1, ylim=ylim)
  lines(dens, col=pal(2)[1], lwd=2)
  lines(zz, f0(zz), col=pal(2)[2], lwd=2)
  fdr.zz <- f0(h$mids)/f(h$mids)
  y <- pmax(h$density * (1 - fdr.zz), 0)
  for (k in 1:length(h$mids)) lines(rep(h$mids[k],2), c(0, y[k]), lwd = 2, col = "red")

  if (lfdrReturn) return(lfdr)
}
formatP <- function(p,digits=2,label=FALSE) {
  val <- formatC(p,digits=digits,format="f")
  for (d in -(digits:4)) val[p < 10^d] <- paste("<",formatC(10^d))
  if (any(p < .01, na.rm=TRUE) & !label) val[substr(val,1,2)=="0."] <- paste("  ",val[substr(val,1,2)=="0."])
  if (label) {
    val[p >= 10^(-digits)] <- paste("p =",val[p >= 10^(-digits)])
    val[p < 10^(-digits)] <- paste("p",val[p < 10^(-digits)])
  }
  val
}
revlevel <- function(x) factor(x, levels=rev(levels(x)))
