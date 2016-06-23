loadData <- function(name, envir=parent.frame()) {
  address <- paste0("http://myweb.uiowa.edu/pbreheny/data/", name, ".RData")
  load(url(address), envir=envir)
}
