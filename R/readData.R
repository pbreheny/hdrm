#' Download data and read into R
#'
#' These functions download data sets described in the hdrm book either read or attach them.  Data sets in hdrm are lists consisting of `X`, `y`, and possibly other elements.  `readData` returns the list as an object that the user can name and use how they want.  `attachData` assigns the elements of the list into the user's environment.  `attachData` is often more convenient, but offers less control and potentially introduces side effects, such as overwriting an existing object `X` if one already exists.  See the examples for more details.
#'
#' @param name    Name of data set to read/attach
#' @param envir   If attaching, to which environment?
#'
#' @examples
#' Data <- readData(bcTCGA)
#' dim(Data$X)
#' head(Data$y)

readData <- function(name) {
  address <- paste0("http://s3.amazonaws.com/pbreheny-data-sets/", substitute(name), ".rds")
  readRDS(url(address))
}

#' @rdname readData
#'
#' @examples
#'
#' attachData(bcTCGA)
#' dim(X)
#' head(y)

attachData <- function(name, envir=parent.frame()) {
  address <- paste0("http://s3.amazonaws.com/pbreheny-data-sets/", substitute(name), ".rds")
  Data <- readRDS(url(address))
  for (x in names(Data)) {
    assign(x, Data[[x]], envir=envir)
  }
}
