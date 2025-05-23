% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lasso-path.r
\name{Fig2.5}
\alias{Fig2.5}
\title{Reproduce Figure 2.5}
\usage{
Fig2.5(cv = TRUE, mar = rep(5, 4))
}
\arguments{
\item{cv}{Carry out cross-validation?  Default: true}

\item{mar}{Margins, passed to \code{par()}}
}
\description{
Reproduces Figure 2.5 from the book; if you specify any options, your results may look different.
}
\examples{
Fig2.5()

# default glmnet plot
attach_data(pollution)
fit <- glmnet(std(X), y)   # Standardize design matrix
plot(fit, label=TRUE)

# Nonzero coefficients at CV-selected lambda
cvfit <- cv.glmnet(X, y, nfolds=length(y))
colnames(X)[unlist(predict(cvfit, type="nonzero"))]

}
