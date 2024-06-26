#' Create forest plot of confidence intervals
#'
#' "Forest plot"-style plotting of confidence intervals from a regression model. Basic input is a matrix with columns of estimate/lower/upper, along with an optional 4th column for the p-value. Also works with a variety of models (lm/glm/coxph/etc).
#'
#' @param obj   The object to be plotted; can be a matrix of raw values or a model object
#' @param ...
#'
#' @examples
#' # Supplying a matrix
#' B <- cbind(1:3, 0:2, 2:4)
#' rownames(B) <- LETTERS[1:3]
#' ci_plot(B)
#'
#' # Supplying a fitted model object
#' fit <- lm(Ozone ~ Solar.R + Wind + Temp, airquality)
#' ci_plot(fit)
#' @export

ci_plot <- function(obj, ...) UseMethod("ci_plot")

#' @rdname ci_plot
#'
#' @param labels              Paramater labels
#' @param sort                Sort parameters by estimate? (default: true)
#' @param xlim,pxlim          x axis limits and breakpoints; see `pretty()`
#' @param ylim                y axis limits (default: c(0.5, n+0.5), where n is number of params)
#' @param sub                 Text to be written at top of plot
#' @param diff                Include tests of difference / p-values?
#' @param null                Draw a line representing no effect at this value (default: 0)
#' @param n.ticks             Number of ticks on x-axis
#' @param mar                 As in `par()`
#' @param axis                Create an x axis?
#' @param trans               Function to transform parameter space
#' @param p.label             Label p-values (p=0.02 instead of just 0.02)? (default: FALSE)
#' @param xlab,ylab           As in `plot()`
#' @param add                 Add to existing plot?
#' @param setupOnly           Create a new window for plot, but don't actually plot anything yet
#' @param lwd                 As in `lines()`
#' @param replaceUnderscore   Replace underscore with space in plotting label
#' @param ...                 Additional arguments to `plot()`
#'
#' @export

ci_plot.matrix <- function(
    obj, labels=rownames(B), sort=TRUE, pxlim, xlim, ylim, sub, diff=(ncol(B)==4), null=0, n.ticks=6, mar, axis=!add,
    trans, p.label=FALSE, xlab="", ylab="", add=FALSE, setupOnly=FALSE, lwd=2, replaceUnderscore=TRUE, ...) {
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
      p <- format_p(B[,4], label=p.label)
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
  rownames(B) <- labels
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

  # Fix lwr/upr (if tau is negative)
  for (i in 1:nrow(B)) {
    B[i, 2:3] <- sort(B[i, 2:3])
  }

  invisible(B)
}

#' @export

ci_plot.lm <- function(obj, intercept=FALSE, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- if (intercept) 1:p else 2:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,4])
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),,drop=FALSE]
  if (plot) B <- ci_plot(B, xlab=xlab, ...)
  return(invisible(B))
}

#' @export

ci_plot.glm <- function(obj,...) ci_plot.lm(obj,...)

#' @export

ci_plot.mer <- function(obj, intercept=FALSE, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, n.sim=10000, ...) {
  fit <- obj
  p <- length(fit@fixef)
  j <- if (intercept) 1:p else 2:p
  B <- cbind(fit@fixef[j], confint(fit, j, n.sim=n.sim))
  if (!missing(tau)) B[,1:3] <- B[,1:3]*tau
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) B <- ci_plot(B, xlab=xlab, ...)
  return(invisible(B))
}

#' @export

ci_plot.coxph <- function(obj, xlab="Regression coefficient", exclude=NULL, plot=TRUE, tau, ...) {
  fit <- obj
  p <- length(coef(fit))
  j <- 1:p
  if (missing(tau)) tau <- 1
  B <- cbind(tau*coef(fit)[j],
             tau*confint(fit,j),
             summary(fit)$coef[j,5])
  colnames(B) <- c("Coef","Lower","Upper","p")
  for (i in seq_along(exclude)) B <- B[-grep(exclude[i],rownames(B)),]
  if (plot) B <- ci_plot(B,xlab=xlab,...)
  return(invisible(B))
}

#' @export

ci_plot.data.frame <- function(obj, ...) {
  ci_plot.matrix(as.matrix(obj), ...)
}
