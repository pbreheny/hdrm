array2df <- function(X,vars=paste("V",1:ncol(df),sep=""))
  {
    df <- cbind(do.call("expand.grid",dimnames(X)),as.numeric(X))
    names(df) <- vars
    return(df)
  }
