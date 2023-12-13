#' Reproduce Example 6.2 and Figure 6.3
#'
#' Reproduces Example 6.2 and Figure 6.3 from the book.  If you specify any options, your results may look different.
#'
#' @examples
#' \dontrun{
#' out <- Ex6.2()
#' Fig6.3(out)
#' }
#' @export

Ex6.2 <- function() {
  # ~/res/lassoFDR/uncorr/sim1.R
  stop('Ex6.2 not implemented yet')
}

#' @rdname Ex6.2
#' @param out   Output of `Ex6.2()`
#' @export

Fig6.3 <- function(out) {
  if (missing(out)) stop("You need to run the code in Ex6.2() first and pass it to Fig6.3()")
  # /man/Breheny2019/fig/sim-ind.R
}
