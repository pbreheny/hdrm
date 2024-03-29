% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gam-snr.R
\name{Fig3.5}
\alias{Fig3.5}
\title{Reproduce Figure 3.5}
\usage{
Fig3.5(
  out,
  parlist = list(mfrow = c(1, 2), mar = c(5, 5, 1, 0.5), oma = c(0, 0, 3, 0))
)
}
\arguments{
\item{out}{Output of Ex3.1()}

\item{parlist}{List of arguments to pass to \code{par()}}
}
\description{
Reproduces Figure 3.5 from the book; if you specify any options, your results may look different.
}
\examples{
out <- Ex3.1(N=3)
Fig3.5(out)
}
