\name{Shedden2008}
\docType{data}
\alias{Shedden2008}

\title{Survival analysis of lung cancer
patients}
\description{
Retrospective data was collected across multiple sites for 442 patients
with adenocarcinoma of the lung including their survival times, some
additional clinical and demographic data, and expression levels of
22,283 genes taken from the tumor sample.

}
\section{Dimensions}{
\itemize{
\item
  n = 442
\item
  p = 22,283
}

}
\format{
\itemize{
\item
  \code{S}: A two-column matrix containing the survival data. First
  column is time on study (in months), second column is the failure
  indication (1=died)
\item
  \code{X}: Gene expression measurements
\item
  \code{Z}: Clinical covariates, which include \code{Sex},
  \code{Age}, \code{Race}, \code{AdjChemo} (whether the patient
  received adjuvant chemotherapy), \code{SmHist} (smoking history),
  \code{Margin}, and \code{Grade}
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

Shedden K, Taylor JMG, Enkemann SA, Tsao M-S, Yeatman TJ, Gerald WL,
Eschrich S, Jurisica I, Giordano TJ, Misek DE, Chang AC, Zhu CQ, Strumpf
D, Hanash S, Shepherd FA, Ding K, Seymour L, Naoki K, Pennell N, Weir B,
Verhaak R, Ladd-Acosta C, Golub T, Gruidl M, Sharma A, Szoke J, Zakowski
M, Rusch V, Kris M, Viale A, Motoi N, Travis W, Conley B, Seshan VE,
Meyerson M, Kuick R, Dobbin KK, Lively T, Jacobson JW and Beer DG
(2008). Gene expression-based survival prediction in lung
adenocarcinoma: a multi-site, blinded validation study. \emph{Nature
Medicine}, \bold{14}: 822-827.

}