#' Classification of acute lymphoblastic leukemia subtypes
#' 
#' @description
#' 
#' There are six known categories of acute lymphoblastic leukemia
#' (ALL). Unfortunately, the accurate assignment of patients to these subtypes
#' is a difficult and expensive process, requiring intensive laboratory studies
#' and the collective expertise of a number of professionals (usually only
#' available at major medical centers).
#' 
#' In this study, bone marrow samples were obtained from pediatric patients, and
#' gene expression measurements were taken. The goal is to determine ALL subtype
#' from the gene expression data alone -- if this can be done accurately, it would
#' make it possible to diagnose ALL subtype at rural hospitals, in developing
#' countries, etc.
#' 
#' The data has been preprocessed using `vsn` on the probe level and the probes
#' have been summed up using the *median polish* technique.
#' 
#' Note: This data set also contains some additional (simulated) gene expression
#' measurements for the purposes of prediction. This is intended as a homework
#' exercise; for this reason the outcomes are not publicly available. Please
#' contact `patrick-breheny@uiowa.edu` if you would like them.
#' 
#' @format
#' 
#' * `y`: ALL subtype (six categories: BCR/E2A/Hyperdip/MLL/T/TEL)
#' 
#' * `X`: A matrix with 248 rows and 12625 columns of gene expression
#'   measurements
#' 
#' * `fData`: Additional information about the features: gene names, gene symbols,
#'   and chromosome locations for the (mapped) probes in `X`. Rows of `fData`
#'   correspond to columns of `X`, and are named accordingly.
#' 
#' * `Xnew`: 100 additional (simulated) gene expression measurements for purposes
#'   of prediction.
#' 
#' @source
#' 
#' I obtained this data set from the `stjudem` package (on Bioconductor). The
#' original reference is:
#' 
#' Yeoh et al. (2002). Classification, subtype discovery, and prediction of
#' outcome in pediatric acute lymphoblastic leukemia by gene expression
#' profiling. *Cancer Cell*, **1**: 133-143.
#' 
#' @name Yeoh2002
NULL
