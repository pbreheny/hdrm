#' Evaluate expression and cache, or load from cache
#'
#' If the file exists, the expression is not evaluated -- the cached result is
#' returned. If the file path does not exist, the expression is evaluated and
#' stored as a parquet file at the location specified by the path. To trigger a
#' re-run, delete the file.
#'
#' @param expr  An R expression that returns a `data.frame`-compatible object;
#'              see `[arrow::write_parquet()]`
#' @param path  A file path (where to save/load the object)
#'
#' @examples
#' tmp <- tempfile()
#' cache(Ex1.1(N=10), tmp)  # saves to cache (slow)
#' cache(Ex1.1(N=10), tmp)  # loads from cache (fast)
#' @export

cache <- function(expr, path) {
  if (file.exists(path)) {
    arrow::read_parquet(path)
  } else {
    x <- eval(expr)
    arrow::write_parquet(x, sink = path)
    x
  }
}
