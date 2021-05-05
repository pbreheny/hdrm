\name{Yeoh2002}
\docType{data}
\alias{Yeoh2002}

\title{Classification of acute lymphoblastic leukemia
subtypes}
\description{
There are six known categories of acute lymphoblastic leukemia (ALL).
Unfortunately, the accurate assignment of patients to these subtypes is
a difficult and expensive process, requiring intensive laboratory
studies and the collective expertise of a number of professionals
(usually only available at major medical centers).

In this study, bone marrow samples were obtained from pediatric
patients, and gene expression measurements were taken. The goal is to
determine ALL subtype from the gene expression data alone -- if this can
be done accurately, it would make it possible to diagnose ALL subtype at
rural hospitals, in developing countries, etc.

The data has been preprocessed using \code{vsn} on the probe level and
the probes have been summed up using the \emph{median polish} technique.

}
\section{Dimensions}{
\itemize{
\item
  n = 248 observations
\item
  p = 12,625 features
\item
  K = 6 outcome categories
}

}
\section{Annotation}{
\itemize{
\item
  The object \code{fData} contains the associated gene names, gene
  symbols, and chromosome locations for the (mapped) probes in
  \code{X}. Rows of \code{fData} correspond to columns of
  \code{X}, and are named accordingly.
}

}
\references{
I obtained this data set from the \code{stjudem} package (on
Bioconductor). The original reference is:


Yeoh et al.~(2002). Classification, subtype discovery, and prediction of
outcome in pediatric acute lymphoblastic leukemia by gene expression
profiling. \emph{Cancer Cell}, \bold{1}: 133-143.

}
