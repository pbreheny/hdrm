\name{Scheetz2006}
\docType{data}
\alias{Scheetz2006}

\title{Gene expression in the mammalian
eye}
\description{
In this study, laboratory rats (Rattus norvegicus) were studied to learn
about gene expression and regulation in the mammalian eye. Inbred rat
strains were crossed and tissue extracted from the eyes of 120 animals
from the F2 generation. Microarrays were used to measure levels of RNA
expression in the isolated eye tissues of each subject.

Of the 31,000 different probes, 18,976 were detected at a sufficient
level to be considered ``expressed'' in the mammalian eye. For the
purposes of this analysis, we treat one of those genes, Trim32, as the
outcome. Trim32 is known to be linked with a genetic disorder called
Bardet-Biedl Syndrome (BBS): the mutation (P130S) in Trim32 gives rise
to BBS. For this reason, Trim32 is also sometimes called Bbs11.

}
\section{Dimensions}{
\itemize{
\item
  n = 120
\item
  p = 18,975
}

}
\format{
\itemize{
\item
  \code{y}: Gene expression measurement for Trim32
\item
  \code{X}: Gene expression measurements for remaining genes
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

Scheetz TE, Kim K-YA, Swiderski RE, Philp AR, Braun TA, Knudtson KL,
Dorrance AM, DiBona GF, Huang J, Casavant TL, Sheffield VC and Stone EM
(2006). Regulation of gene expression in the mammalian eye and its
relevance to eye disease. \emph{Proceedings of the National Academy of
Sciences}, \bold{103}: 14429-14434.

}