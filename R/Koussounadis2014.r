#' Carbotax study of ovarian tumor growth
#' 
#' @description
#' 
#' The data presented here come from a study of gene expression changes in
#' ovarian cancer.  The current standard treatment for ovarian cancer consists
#' of surgery, followed by either carboplatin and paclitaxel or carboplatin
#' alone.  This approach, however, is not effective for all patients.  The goal
#' of this study was to identify genes and pathways associated with drug
#' response.  To identify such genes, the investigators implanted ovarian cell
#' lines into adult mice and allowed the tumors to grow for 2 months, at which
#' point one of three treatments (carboplatin, carboplatin + paclitaxel, or
#' control) was administered to each mouse.  At various time points ranging from
#' 0 to 14 days following the initiation of treatment, the mice were sacrificed,
#' at which point the investigators measured the size of the tumor as well as
#' gene expression in the cancerous tissue.
#' 
#' Our analysis here concentrates on relative tumor volume (RTV) as the outcome
#' variable.
#' 
#' For this study, there were 34,694 features with expression data and a sample
#' size of 101 mice.
#' 
#' @format
#' 
#' * `y`: Relative tumor volume.  Measurements are on the log2 scale so that y=1
#'   means that the tumor has doubled in size since baseline.  By definition,
#'   y=0 for all samples taken at day 0.
#'
#' * `X`: #' A matrix with 101 rows and 34694 columns of gene expression
#'   measurements
#' 
#' * `sData`: Data frame containing additional information about the
#'   experimental conditions for each sample.
#' 
#'   * `Treatment`: Treatment received (Carbo, CarboTax, Control)
#'   * `Day`: Days since beginning of treatment
#' 
#' ### Annotation
#' 
#' * Rows of X and elements of y are given matching, arbitrary labels.
#' * Columns of X are labeled with the Gene Symbol.
#' * In addition, a data frame `fData` is provided that contains additional annotation information for each feature.
#' 
#' @source
#' 
#' Koussounadis A, Langdon SP, Harrison DJ and Smith VA (2014). Chemotherapy-induced dynamic gene expression changes in vivo are prognostic in ovarian cancer. *British Journal of Cancer*, **110**: 2975-2984.
#' 
#' @name Koussounadis2014
NULL
