% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ridge.r
\name{plot.ridge}
\alias{plot.ridge}
\title{Plot a ridge regression coefficient path}
\usage{
\method{plot}{ridge}(
  x,
  xaxis = c("loglam", "df", "both"),
  standardize = FALSE,
  xlab,
  ylab,
  ...
)
}
\arguments{
\item{x}{An object of class \code{"ridge"}, as returned by \code{ridge()}}

\item{xaxis}{One of \code{"loglam"}, \code{"df"}, or \code{"both"}. If \code{"both"}, the
bottom axis is lambda (log scale), and the top is df.}

\item{standardize}{As in \code{coef()} (default: FALSE)}

\item{xlab, ylab}{As in \code{plot()}}

\item{...}{Additional arguments to \code{matplot()}}
}
\description{
Plot a ridge regression coefficient path
}
