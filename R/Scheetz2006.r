#' Gene expression in the mammalian eye
#' 
#' @description
#' 
#' In this study, laboratory rats (Rattus norvegicus) were studied to learn about
#' gene expression and regulation in the mammalian eye.  Inbred rat strains were
#' crossed and tissue extracted from the eyes of 120 animals from the F<sub>2</sub>
#' generation.  Microarrays were used to measure levels of RNA expression in the
#' isolated eye tissues of each subject.
#' 
#' Of the 31,000 different probes, 18,976 were detected at a sufficient level to be
#' considered "expressed" in the mammalian eye.  For the purposes of this analysis,
#' we treat one of those genes, Trim32, as the outcome.  Trim32 is known to be
#' linked with a genetic disorder called Bardet-Biedl Syndrome (BBS): the mutation
#' (P130S) in Trim32 gives rise to BBS.  For this reason, Trim32 is also sometimes
#' called Bbs11.
#' 
#' @format
#' 
#' * `y`: Gene expression measurement for Trim32
#' 
#' * `X`: A matrix with 120 rows and 18975 columns of gene expression
#'   measurements for remaining genes
#' 
#' ### Annotation
#' 
#' * The object `fData` contains the associated gene names, gene symbols, and
#'   chromosome locations for the (mapped) probes in `X`.  Rows of `fData`
#'   correspond to columns of `X`, and are named accordingly.
#' 
#' @source
#' 
#' Scheetz TE, Kim K-YA, Swiderski RE, Philp AR, Braun TA, Knudtson KL, Dorrance
#' AM, DiBona GF, Huang J, Casavant TL, Sheffield VC and Stone EM
#' (2006). Regulation of gene expression in the mammalian eye and its relevance
#' to eye disease. *Proceedings of the National Academy of Sciences*, **103**:
#' 14429-14434.
#' 
#' @name Scheetz2006
NULL
