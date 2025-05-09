#' Case-control study of prostate cancer
#' 
#' @description
#' 
#' Prostate tumors are among the most heterogeneous of cancers, both histologically
#' and clinically. Gene expression data was collected on samples from 102 patients
#' who underwent a prostatectomy. Of the samples, 52 are from tumors and 50 are
#' from healthy tissue.
#' 
#' @format
#' 
#' * `y`: Classification of sample (normal, tumor)
#' 
#' * `X`: A matrix with 102 rows and 12600 columns of gene expression
#'   measurements, normalized and on the log2 scale
#' 
#' ### Annotation
#' 
#' * Rows of X, and elements of y, are annotated with matching sample labels.
#' * Columns of X are labeled with a GenBank ID. To see what gene a given ID
#'   belongs to, visit
#'   [http://www.ncbi.nlm.nih.gov/genbank](http://www.ncbi.nlm.nih.gov/genbank).
#' 
#' @source
#' 
#' I obtained the data from the authors at
#' [https://portals.broadinstitute.org/cgi-bin/cancer/publications/view/75](http://www-genome.wi.mit.edu/MPR/prostate)
#' and used the [vsn](http://bioconductor.org/packages/vsn) package for
#' normalization. The original reference is:
#' 
#' Singh D, Febbo PG, Ross K, Jackson DG, Manola J, Ladd C, Tamayo P, Renshaw AA,
#' D'Amico AV, Richie JP, Lander ES, Loda M, Kantoff PW, Golub TR and Sellers WR
#' (2002). Gene expression correlates of clinical prostate cancer
#' behavior. *Cancer Cell*, **1**: 203-209.
#' 
#' @name Singh2002
NULL
