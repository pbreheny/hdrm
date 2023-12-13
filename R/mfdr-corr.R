#' Reproduce Example 6.3 and Figure 6.5
#'
#' Reproduces Example 6.3 and Figure 6.5 from the book.  If you specify any options, your results may look different.
#'
#' @examples
#' \dontrun{
#' out <- Ex6.3()
#' Fig6.5(out)
#' }
#' @export

Ex6.3 <- function() {
  # ~/res/lassoFDR/corr/*
  stop('Ex6.3 not implemented yet')
}

#' @rdname Ex6.3
#' @param out   Output of `Ex6.3()`
#' @export

Fig6.5 <- function(out) {
  if (missing(out)) stop("You need to run the code in Ex6.3() first and pass it to Fig6.5()")
  # /man/Breheny2019/fig/sim-corr.R
}
