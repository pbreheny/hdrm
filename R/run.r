#' Run an example and save the result, or load if already run
#'
#' To trigger a re-run, delete the file. All results are saved as parquet files.
#'
#' @param expr  An R expression that returns a `data.frame`-compatible object;
#'              see `[arrow::write_parquet()]`
#' @param path  A file path (where to save/load the object)
#'
#' @examples
#' run(Ex1.1(N=10), '~/a.pqt')
#' @export

run <- function(expr, path) {
  if (file.exists(path)) {
    arrow::read_parquet(path)
  } else {
    arrow::write_parquet(x = eval(expr), sink = path)
  }
}
