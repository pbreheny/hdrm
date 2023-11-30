#' Download data and read into R
#'
#' These functions are downloading, reading, and attaching data sets described in the hdrm book.
#' To keep the size of the `hdrm` package manageable, these data sets are not included with the package and must be downloaded separately using `downloadData()`; this only needs to be done once.
#'
#' Data sets in hdrm are lists consisting of `X`, `y`, and possibly other elements.
#' `readData` returns the list as an object that the user can name and use how they want.
#' `attachData` assigns the elements of the list into the user's environment.
#' `attachData` is often more convenient, but offers less control and potentially introduces side effects, such as overwriting an existing object `X` if one already exists.  See the examples for more details.
#'
#' @param name    Name of data set to read/attach
#'
#' @examples
#' \dontrun{
#' downloadData()         # Download all data sets
#' downloadData(brca1)    # Download a specific data set}
#' @export

downloadData <- function(name) {
  if (missing(name)) {
    name <- hdrm_data_sets
  } else {
    name <- as.character(substitute(name))
  }
  for (id in name) {
    ext <- if (id == 'glioma') '.txt' else '.rds'
    URL <- paste0(
      'https://github.com/IowaBiostat/data-sets/raw/main/',
      id, '/', id, ext)
    FILE <- paste0(system.file("extdata", package="hdrm"), '/', id, '.rds')
    download.file(URL, FILE, mode='wb')
  }
}

#' @rdname downloadData
#'
#' @param envir   If attaching, to which environment?
#'
#' @examples
#'
#' Data <- readData(brca1)
#' dim(Data$X)
#' head(Data$y)
#'
#' @export

readData <- function(name) {
  name <- as.character(substitute(name))
  if (!(name %in% hdrm_data_sets)) stop(paste0(name, ' is not an hdrm data set'))
  FILE <- paste0(system.file("extdata", package="hdrm"), '/', name, '.rds')
  if (!file.exists(FILE)) stop("You have to run downloadData() first; see ?downloadData", call.=FALSE)
  readRDS(FILE)
}

#' @rdname downloadData
#'
#' @examples
#'
#' attachData(bcTCGA)
#' dim(X)
#' head(y)
#'
#' @export

attachData <- function(name, envir=parent.frame()) {
  name <- as.character(substitute(name))
  if (!(name %in% hdrm_data_sets)) stop(paste0(name, ' is not an hdrm data set'))
  FILE <- paste0(system.file("extdata", package="hdrm"), '/', name, '.rds')
  if (!file.exists(FILE)) stop("You have to run downloadData() first; see ?downloadData", call.=FALSE)
  Data <- readRDS(FILE)
  if (!is.list(Data)) {
    stop(paste0("attachData() does not work for ", name, "; use readData() instead."))
  }
  for (x in names(Data)) {
    assign(x, Data[[x]], envir=envir)
  }
}
