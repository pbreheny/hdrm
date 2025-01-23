#' Download data and read into R
#'
#' These functions are downloading, reading, and attaching data sets described in the hdrm book.
#' To keep the size of the `hdrm` package manageable, these data sets are not included with the package and must be downloaded separately using `downloadData()`; this only needs to be done once.
#'
#' Data sets in hdrm are lists consisting of `X`, `y`, and possibly other elements.
#' `read_data` returns the list as an object that the user can name and use how they want.
#' `attach_data` assigns the elements of the list into the user's environment.
#' `attach_data` is often more convenient, but offers less control and potentially introduces side effects, such as overwriting an existing object `X` if one already exists.  See the examples for more details.
#'
#' @param name    Name of data set to read/attach
#'
#' @examples
#' \dontrun{
#' download_data()         # Download all data sets
#' download_data(brca1)    # Download a specific data set}
#' @export

download_data <- function(name) {
  if (missing(name)) {
    name <- hdrm_data_sets
  }
  for (id in name) {
    ext <- if (id == 'glioma') '.txt' else '.rds'
    URL <- paste0(
      'https://github.com/IowaBiostat/data-sets/raw/main/',
      id, '/', id, ext)
    FILE <- paste0(system.file("extdata", package="hdrm"), '/', id, '.rds')
    try(suppressMessages(suppressWarnings(download.file(URL, FILE, mode = 'wb'))), silent = TRUE)
    if (!file.exists(FILE)) {
      stop(paste0("Unable to download data set '", name, "'"), call. = FALSE)
    }
  }
}

#' @rdname download_data
#'
#' @param envir   If attaching, to which environment?
#'
#' @examples
#' Data <- read_data(brca1)
#' dim(Data$X)
#' head(Data$y)
#' @export

read_data <- function(name) {
  name <- as.character(substitute(name))
  if (!(name %in% hdrm_data_sets)) stop(paste0(name, ' is not an hdrm data set'))
  FILE <- paste0(system.file("extdata", package="hdrm"), '/', name, '.rds')
  if (!file.exists(FILE)) download_data(name)
  readRDS(FILE)
}

#' @rdname download_data
#'
#' @examples
#' attach_data(brca1)
#' dim(X)
#' head(y)
#' @export

attach_data <- function(name, envir=parent.frame()) {
  name <- as.character(substitute(name))
  if (!(name %in% hdrm_data_sets)) stop(paste0(name, ' is not an hdrm data set'))
  FILE <- paste0(system.file("extdata", package="hdrm"), '/', name, '.rds')
  if (!file.exists(FILE)) download_data(name)
  Data <- readRDS(FILE)
  if (!is.list(Data)) {
    stop(paste0("attach_data() does not work for ", name, "; use read_data() instead."))
  }
  for (x in names(Data)) {
    assign(x, Data[[x]], envir=envir)
  }
}
