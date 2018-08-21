loadData <- function(name, envir=parent.frame()) {
  address <- paste0("http://s3.amazonaws.com/pbreheny-data-sets/", name, ".RData")
  load(url(address), envir=envir)
}
