#' Reproduce Figure 6.6
#'
#' Reproduces Figure 6.6 from the book.  If you specify any options, your results may look different.
#'
#' @param fit    \code{ncvreg} fit to the TCGA data; see examples
#'
#' @examples
#' attach_data(brca1)
#'
#' fit <- ncvreg(X, y, penalty="lasso")
#' obj <- mfdr(fit)
#' tail(subset(obj, mFDR < .1))
#' plot(obj)
#'
#' # Lasso figure
#' Fig6.6(fit)
#'
#' # An MCP figure
#' fit <- ncvreg(X, y)
#' Fig6.6(fit)
#'
#' @export

Fig6.6 <- function(fit) {
  obj <- mfdr(fit)
  par(mar=c(5,5,5,0.5), mfrow=c(1,2))
  plot(obj, lwd=3, log.l=TRUE, bty="n")
  plot(obj, type="EF", lwd=3, log.l=TRUE, bty="n")
}
