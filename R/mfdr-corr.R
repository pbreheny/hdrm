#' @export

Ex6.3 <- function() {
  # NEED TO ADD
}

#' @export

Fig6.5 <- function(out) {
  requireNamespace(ggplot2)
  requireNamespace(gridExtra)

  ## Read in data
  df <- array2df(apply(out[,,-1], 2:3, sum), vars=c("lambda", "Type", "Avg"))
  S <- apply(out[,,1], 2, sum)
  df$mFDR <- df$Avg/S
  df$Avg <- df$Avg/dim(out)[1]
  df$lam <- factor2num(df$lambda)
  df$ll <- log(df$lam)

  xlim <- rev(range(df$ll))
  p1 <- qplot(ll, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=xlim) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + theme(legend.position="top")
  p2 <- qplot(ll, mFDR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=xlim) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + theme(legend.position="top")

  #subset(df, ll == ll[14])
  gridExtra::grid.arrange(p1, p2, widths=c(1,1))
}
