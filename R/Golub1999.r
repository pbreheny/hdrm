#' Gene expression in leukemia patients
#' 
#' @description
#' 
#' This data set consists of 47 patients with acute lymphoblastic leukemia (ALL)
#' and 25 patients with acute myeloid leukemia (AML). Each of the 72 patients had
#' bone marrow samples obtained at the time of diagnosis.  Gene expression
#' measurements for the samples were taken using Affymetrix Hgu6800 chips,
#' resulting in 7129 measurements per patient.
#' 
#' Of the two diseases, AML has a considerably worse prognosis: only 26% survive at
#' least 5 years following diagnosis, compared to 68% for ALL
#' ([source](http://seer.cancer.gov)).
#' 
#' @format
#' 
#' * `y`: Type of leukemia (ALL, AML)
#' * `X`: A matrix with 72 rows and 7129 columns containing gene expression
#'   measurements, normalized and on the log2 scale
#' 
#' ### Annotation
#' 
#' * Rows of X, and elements of y, are annotated with arbitrary, matching sample
#'   labels.
#' * Columns of X are labeled with the Gene Symbol.  In some cases, the probe could
#'   not be matched to a gene symbol; in those cases, the column is labeled with
#'   the Affymetrix probe label instead.  This happens for artificial spike-in
#'   probes and probes where it is uncertain which, if any gene, they measure.
#' 
#' @source
#' 
#' I obtained the data from the
#' [golubEsets](https://www.bioconductor.org/packages/golubEsets) package, used the
#' [vsn](http://bioconductor.org/packages/vsn) package for normalization, and the
#' [hu6800.db](https://bioconductor.org/packages/hu6800.db/) package for
#' annotation.
#' 
#' Golub TR, Slonim DK, Tamayo P, Huard C, Gaasenbeek M, Mesirov JP, Coller H,
#' Loh ML, Downing JR, Caligiuri MA, Bloomfield CD, and Lander ES (1999).
#' Molecular classification of cancer: class discovery and class prediction by
#' gene expression monitoring.  *Science*, **286**:531-537.
#' 
#' @name Golub1999
NULL
