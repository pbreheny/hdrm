% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bc-tcga.R
\name{Fig2.10}
\alias{Fig2.10}
\title{Reproduce Figure 2.10}
\usage{
Fig2.10(cvfit, parlist = list(mfrow = c(1, 2), mar = c(5, 5, 5, 0.5)))
}
\arguments{
\item{cvfit}{\code{glmnet()} fit to the TCGA data; see examples}

\item{parlist}{List of arguments to pass to \code{par()}}
}
\description{
Reproduces Figure 2.10 from the book.  If you specify any options, your results may look different.
}
\examples{
attachData(bcTCGA)
cvfit <- cv.glmnet(X, y)
Fig2.10(cvfit)
}
