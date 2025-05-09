#' Array CGH data of a glioblastoma tumor
#' 
#' @description
#' 
#' Broadly speaking, humans have two copies of their genome.  Occasionally however, a region of the genome is duplicated or destroyed; this is known as copy number variation.  All humans have some degree of copy number variation; many cancers, however, display much more extreme variation.  Comparative genomic hybridization (CGH) is one method for measuring the number of copies present at a genome-wide scale.
#' 
#' This data set consists of CGH data from glioblastoma tumors.  In the data set, the data from two tumors (chromosome 7 in one patient, chromosome 13 in another) are spliced together in order to create a challenging data set for CNV detection consisting of both gains and losses over both short and large scales.  It has become a popular benchmark data set for CNV studies.
#' 
#' @format
#' 
#' A vector of length 990 (since there is only 1 instance, the data is simply
#' stored a numeric vector.
#' 
#' The numbers in the vector are the log (base 2) ratio of the number of DNA copies
#' of the tumor relative to a normal reference. Values above 0 represent copy
#' number gains, while values less than 0 represent copy number losses.
#' 
#' @source
#' 
#' The original data appear in:
#' 
#' Bredel M, Bredel C, Juric D, Harsh GR, Vogel H, Recht LD, and Sikic
#' BI. (2005). High-Resolution Genome-Wide Mapping of Genetic Alterations in
#' Human Glial Brain Tumors. *Cancer Research*, **65**: 4088-4096.
#' 
#' The pseudo-chromosome data set was introduced by:
#' 
#' Tibshirani R, and Wang P. (2008). Spatial smoothing and hot spot detection for
#' CGH data using the fused lasso. *Biostatistics*, **9**: 18-29.
#' 
#' The data used to be available in the `cghFLasso` package, but this package
#' appears to be no longer available.
#' 
#' @name glioma
NULL
