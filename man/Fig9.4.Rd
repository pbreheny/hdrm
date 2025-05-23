% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot-toy.r
\name{Fig9.4}
\alias{Fig9.4}
\title{Reproduce Figure 9.4}
\usage{
Fig9.4(out, B = 100, seed = 2)
}
\arguments{
\item{out}{Output of Ex9.1}

\item{B}{Number of bootstrap replications}

\item{seed}{Random number seed for reproducibility}
}
\description{
Reproduces Figure 9.4 from the book.  If you specify any options, your results may look different.
}
\examples{
Data <- Ex9.1()
res <- Fig9.4(Data)
covered <- Data$beta >= res$Lower & Data$beta <= res$Upper
mean(covered)
table(Data$varType, covered)
}
