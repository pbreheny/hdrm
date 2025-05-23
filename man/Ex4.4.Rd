% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mnet-sim-cor.r
\name{Ex4.4}
\alias{Ex4.4}
\alias{Fig4.3}
\title{Reproduce Example 4.4 and Figure 4.3}
\usage{
Ex4.4(
  N = 100,
  s = seq(0.1, 1.1, 0.2),
  n = 100,
  p = 500,
  p1 = 12,
  rho = 0.7,
  seed = 1
)

Fig4.3(res, parlist = list(mfrow = c(1, 2), mar = c(4, 4, 2, 0.5)))
}
\arguments{
\item{N}{Number of simulated realizations}

\item{s}{Signal strength (coefficient value for the non-null features)}

\item{n}{Sample size}

\item{p}{Number of features}

\item{p1}{Number of non-null features}

\item{rho}{Correlation between features (compound symmetric)}

\item{seed}{Seed for reproducibility}

\item{res}{Output from Ex4.4}

\item{parlist}{List of arguments to pass to \code{par()}}
}
\description{
Reproduces Example 4.4 and Figure 4.3 from the book.  If you specify any options, your results may look different.  Note that this simulation in particular is very time-consuming if run with the full N=100 replications.
}
\examples{
res <- Ex4.4(N=2, s=c(0.3, 1))
Fig4.3(res)
}
