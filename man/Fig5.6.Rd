\name{Fig5.6}
\alias{Fig5.6}
\title{Reproduce Figure 5.6}
\usage{
Fig5.6(fit)
}
\description{
\code{Fig5.6()} will recreate Figure 5.6 from the book.  If you specify any
options, your results may look different.  See the code for details.
}
\arguments{
  item{fit}{ncvreg fit to the TCGA data; see examples}
}
\examples{
loadData("bcTCGA")

# Example code
fit <- ncvreg(X, y, penalty="lasso")
obj <- fir(fit)
tail(subset(obj, FIR < .1))
plot(obj)

# Lasso figure
Fig5.6(fit)

# An MCP figure
fit <- ncvreg(X, y)
Fig5.6(fit)
}
