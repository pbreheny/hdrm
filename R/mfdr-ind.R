#' Reproduce Example 6.2 and Figure 6.3
#'
#' Reproduces Example 6.2 and Figure 6.3 from the book.  If you specify any options, your results may look different.
#'
#' @examples
#' \dontrun{
#' out <- Ex6.2()
#' Fig6.3(out)
#' }
#' @export

Ex6.2 <- function() {
  stop('Ex6.2 not implemented yet')
  #subset(df, loglambda == loglambda[20])
}

#' @rdname Ex6.2
#'
#' @param out   Output of `Ex6.2()`
#'
#' @export

Fig6.3 <- function(out) {
  if (missing(out)) stop("You need to run the code in Ex6.2() first and pass it to Fig6.3()")

  out[,,1,4] <- out[,,1,4]*42/60
  out[,,2,4] <- out[,,2,4]*540/600
  dimnames(out)[[4]][3:4] <- c("Actual", "Estimated")
  df <- data.frame(array2df(apply(out[,,,3:4],2:4,mean), vars=c("lambda","p","Type", "Avg")))
  df$lambda <- factor2num(df$lambda)
  df$loglambda <- log(df$lambda)
  df$S <- as.numeric(apply(apply(out[,,,1:3], 1:3, sum), 2:3, mean))
  df$FDR <- df$Avg/df$S

  p1 <- qplot(loglambda, Avg, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="False inclusions", xlim=c(0.4, -4.2)) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="top") + theme(panel.background=element_rect(fill = "gray90"))
  p2 <- qplot(loglambda, FDR, data=df, color=Type, geom="line", xlab=expression(log(lambda)), ylab="FIR", xlim=c(0.4, -4.2)) +
    geom_line(size=2) + scale_color_manual(values=pal(2, alpha=0.5)) + facet_grid(~p, labeller=label_both) + theme(legend.position="none") + theme(panel.background=element_rect(fill = "gray90"))

  gridExtra::grid.arrange(p1, p2, heights=c(1.2, 1))
}
