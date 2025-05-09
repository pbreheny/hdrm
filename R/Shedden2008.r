#' Survival analysis of lung cancer patients
#' 
#' @description
#' 
#' Retrospective data was collected across multiple sites for 442 patients with
#' adenocarcinoma of the lung including their survival times, some additional
#' clinical and demographic data, and expression levels of 22,283 genes taken from
#' the tumor sample.
#' 
#' @format
#' 
#' * `S`: A two-column matrix containing the survival data. First column is time on
#'   study (in months), second column is the failure indication (1=died). One would
#'   presume that time is measured as months since surgery, however this is never
#'   explicitly stated in the article.
#' 
#' * `X`: A matrix with 442 rows and 22283 columns of gene expression
#'   measurements
#' 
#' * `Z`: Clinical covariates, which include `Sex`, `Age`, `Race`, `AdjChemo`
#'   (whether the patient received adjuvant chemotherapy), `SmHist` (smoking
#'   history), `Margin`, and `Grade`
#' 
#' ### Annotation
#' 
#' * The object `fData` contains the associated gene names and gene symbols for the
#'   (mapped) probes in `X`. Rows of `fData` correspond to columns of `X`, and are
#'   named accordingly.
#' 
#' @source
#' 
#' Shedden K, Taylor JMG, Enkemann SA, Tsao M-S, Yeatman TJ, Gerald WL, Eschrich
#' S, Jurisica I, Giordano TJ, Misek DE, Chang AC, Zhu CQ, Strumpf D, Hanash S,
#' Shepherd FA, Ding K, Seymour L, Naoki K, Pennell N, Weir B, Verhaak R,
#' Ladd-Acosta C, Golub T, Gruidl M, Sharma A, Szoke J, Zakowski M, Rusch V, Kris
#' M, Viale A, Motoi N, Travis W, Conley B, Seshan VE, Meyerson M, Kuick R,
#' Dobbin KK, Lively T, Jacobson JW and Beer DG (2008). Gene expression-based
#' survival prediction in lung adenocarcinoma: a multi-site, blinded validation
#' study. *Nature Medicine*, **14**: 822-827.
#' 
#' @name Shedden2008
NULL
