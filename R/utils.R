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
CIplot.matrix <- function(obj, labels=rownames(B), sort=TRUE, pxlim, xlim, ylim, sub, diff=(ncol(B)==4), null=0, n.ticks=6, mar, axis=!add, trans, p.label=FALSE, xlab="", ylab="", add=FALSE, setupOnly=FALSE, lwd=2, replaceUnderscore=TRUE, ...) {
  B <- obj
  if (sort) B <- B[order(B[,1], decreasing=TRUE),,drop=FALSE]

  ## Set up margins
  if (missing(mar)) {
    m1 <- 5
    nn <- if (is.null(labels)) 10 else max(nchar(labels))
    m2 <- nn/3+.5
    m3 <- 2
    m4 <- if (diff) 6 else 2
    op <- par(mar=c(m1, m2, m3, m4))
  } else op <- par(mar=mar)
  n <- nrow(B)
  if (!missing(trans)) B[,1:3] <- trans(B[,1:3])

  ## Set up plot structure and add points
  if (missing(pxlim)) {
    pxlim <- if (missing(xlim)) pretty(range(B[,2:3], na.rm=TRUE),n=n.ticks-1) else pretty(xlim, n=n.ticks-1)
  }
  if (missing(ylim)) ylim <- c(0.5,n+0.5)
  if (add) {
    points(B[n:1,1], 1:n, pch=19)
  } else if (setupOnly) {
    plot(B[n:1,1], 1:n, type="n", xlim = range(pxlim), ylim=ylim, ylab=ylab, axes=FALSE, pch=19, xlab=xlab, ...)
    return(invisible(NULL))
  } else {
    plot(B[n:1,1], 1:n, xlim = range(pxlim), ylim=ylim, ylab=ylab, axes=FALSE, pch=19, xlab=xlab, ...)
  }

  ## Add lines, p-values
  for (i in 1:n) {
    dots <- list(...)
    col <- if ("col" %in% names(dots)) rep_len(dots$col[n-i+1], n) else "black"
    lines(c(B[i,2:3]), c(n-i+1,n-i+1), lwd=lwd, col=col)
    if (diff) {
      p <- formatP(B[,4], label=p.label)
      p[is.na(B[,4])] <- ""
      mtext(at=n-i+1,p[i],line=1,side=4,las=1, cex=0.8*par("cex"), adj=0)
    }
  }
  if (axis) axis(1, pxlim)
  if (diff) {
    if (!missing(trans)) null <- trans(null)
    abline(v=null,col="gray")
  }
  if (!missing(sub)) mtext(sub,3,0,cex=0.8)

  ## Add labels
  if (replaceUnderscore) labels <- gsub("_", " ", labels)
  if (!add) {
    ind <- !is.na(B[,1])
    lapply(which(ind), function(l) text(x=par("usr")[1], adj=1, y=(n:1)[l], labels=labels[[l]], xpd=TRUE, cex=.8)) ## List approach is necessary for compatibility with expressions
    if (sum(!ind) > 0) {
      a <- diff(par("usr")[1:2])/diff(par("plt")[1:2])
      b <- par("usr")[1] - a*par("plt")[1]
      text(x=b+a*.01, adj=0, y=(n:1)[!ind], labels=labels[!ind], xpd=TRUE, cex=.8)
    }
  }
  par(op)
  invisible(B)
}
CIplot.lm <- function(obj, intercept=FALSE, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- if (intercept) 1:p else 2:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,4])
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),,drop=FALSE]
  if (plot) CIplot(B,xlab=xlab,...)
  return(invisible(B))
}
CIplot.glm <- function(obj,...) CIplot.lm(obj,...)
CIplot.mer <- function(obj, intercept=FALSE, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, n.sim=10000, ...) {
  fit <- obj
  p <- length(fit@fixef)
  j <- if (intercept) 1:p else 2:p
  B <- cbind(fit@fixef[j], confint(fit, j, n.sim=n.sim))
  if (!missing(tau)) B[,1:3] <- B[,1:3]*tau
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) CIplot(B,xlab=xlab,...)
  return(invisible(B))
}
CIplot.coxph <- function(obj, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- 1:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,5])
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) CIplot(B,xlab=xlab,...)
  return(invisible(B))
}
CIplot <- function(obj,...) UseMethod("CIplot")
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
