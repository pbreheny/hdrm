#' @export

Fig6.4 <- function(out) {
  dimnames(out)[[3]][1:2] <- c("Univariate", "LassoFIR")
  dimnames(out)[[4]] <- c("Causative (A)", "Correlated (B)", "Noise (C)")
  df <- array2df(apply(out, 2:4, mean, na.rm=TRUE), vars=c("p","Method", "Group", "Avg"))

  # Subsets / reordering factors
  ggdf1 <- subset(df, p==60 & Method != "LassoBIC")
  #ggdf2 <- subset(df, p==600 & !(Method %in% c("LassoBIC", "LassoCV")))
  ggdf2 <- subset(df, p==600 & Method != "LassoBIC")
  ggdf1$Method <- revlevel(relevel(droplevels(ggdf1$Method), 'LassoFIR'))
  ggdf2$Method <- revlevel(relevel(droplevels(ggdf2$Method), 'LassoFIR'))

  # Plot
  p1 <- ggplot(data=ggdf1) + aes(Method, Avg, fill=Group) + geom_bar(stat='identity') +
    scale_fill_grey(start=0,end=1) + coord_flip() + theme(panel.background=element_rect(fill = "gray90")) +
    facet_grid(p~., labeller=label_both) + theme(legend.position="top", legend.background=element_rect(fill = "gray90"))
  p2 <- ggplot(data=ggdf2) + aes(Method, Avg, fill=Group) + geom_bar(stat='identity') +
    scale_fill_grey(start=0,end=1) + coord_flip() + theme(panel.background=element_rect(fill = "gray90")) +
    facet_grid(p~., labeller=label_both) + theme(legend.position="none")

  gridExtra::grid.arrange(p1, p2, heights=c(1.3, 1))

  # Summaries
#   T1 <- apply(out[,1,,],2:3,mean)
#   T1[,3]/apply(T1,1,sum)
#   (T1[,2]+T1[,3])/apply(T1,1,sum)
#   T2 <- apply(out[,2,,],2:3,mean)
#   T2[,3]/apply(T2,1,sum)
#   (T2[,2]+T2[,3])/apply(T2,1,sum)

}
