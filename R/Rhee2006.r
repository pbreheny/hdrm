#' HIV Drug Resistance Mutations
#' 
#' @description
#' 
#' Although there are many drugs that have been approved for treating the Human
#' Immunodeficiency Virus (HIV), one of the hallmarks of the virus is its
#' ability to rapidly mutate and gain resistance to these drugs.  In this study,
#' isolates of HIV were extracted from infected individuals and sequenced.
#' These isolates were also tested for their resistance to various drugs used in
#' HIV therapy.  The scientific goal of the project is to determine which
#' mutations are associated with drug resistance, thereby helping to develop new
#' antiretroviral drugs and to optimize the use of existing drugs.
#' 
#' The full study examined many drugs and is available at the [Stanford HIV Drug
#' Resistance
#' Database](https://hivdb.stanford.edu/pages/published_analysis/genophenoPNAS2006).
#' This data set contains the results for one specific drug, Nelfinavir, a
#' protease inhibitor, and the presence of mutations in the protease gene, which
#' potentially confer resistance to the drug.
#' 
#' @format
#' 
#' * `y`: Outcome of the drug susceptibility assay.  Higher numbers indicate greater resistance to the drug.
#' 
#' * `X`: A matrix with 842 rows and 361 columns of 1/0 indicators for
#'   presence/absence of a mutation at a given position. Column names indicate
#'   the position and mutation. For example, the protease gene is 99 amino
#'   acids long; for any isolates with a `1` in column `P13.V`, this indicates
#'   that at position 13 in the amino acid sequence, the isolate has the amino
#'   acid (V)aline instead of the usual amino acid found at position 13.
#' 
#' @source
#' 
#' The data come from the Stanford HIV Drug Resistance Database and were originally described in:
#' 
#' Rhee S-Y, Taylor J, Wadhera G, Ben-Hur A, Brutlag DL, and Shafer RW (2006). Genotypic predictors of human immunodeficiency virus type 1 drug resistance. *Proceedings of the National Academy of Sciences*, **103**: 17355-17360.
#' 
#' @name Rhee2006
NULL
