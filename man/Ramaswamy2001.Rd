\name{Ramaswamy2001}
\docType{data}
\alias{Ramaswamy2001}

\title{Multiclass cancer prediction involving 14 tumor
types}
\description{
The optimal treatment of cancer patients depends on establishing
accurate diagnoses using clinical and histopathologic data. In some
instances, particularly involving metastatic tumors, this is difficult
to atypical clinical presentation or histopathology. The purpose of this
study was to determine whether the diagnosis of common adult
malignancies could be achieved purely by molecular classification. The
data consists of 218 tumor samples, spanning 14 common tumor types. For
each tumor, the expression of 16,063 genes were measured.

}
\section{Dimensions}{
\itemize{
\item
  n = 144 (train), 54 (test)
\item
  p = 16,063
}

}
\format{
\itemize{
\item
  \code{y}: A 14-level factor indicating the type of tumor
\item
  \code{X}: Gene expression measurement
\item
  \code{y.test}: Same as above, but for independent testing
\item
  \code{X.test}: Same as above, but for independent testing
}

}
\section{Annotation}{
\itemize{
\item
  The object \code{fData} contains the associated gene names and gene
  symbols for the (mapped) probes in \code{X}. Rows of \code{fData}
  correspond to columns of \code{X}, and are named accordingly.
}

}
\references{
I obtained the raw data from
\href{https://web.stanford.edu/~hastie/glmnet/glmnetData}{Trevor
Hastie}; I am not sure what sort of preprocessing/normalization was done
to it. I added the annotation, both with respect to the tumor types and
the feature data.

Original citation:


Ramaswamy S, Tamayo P, Rifkin R, Mukherjee S, Yeang C, Angelo M, Ladd C,
Reich M, Latulippe E, Mesirov J and others (2001). Multiclass cancer
diagnosis using tumor gene expression signatures. \emph{Proceedings of
the National Academy of Sciences}, \bold{98}: 15149-15154.

}