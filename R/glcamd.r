#' Genetic association study of glaucoma and macular degeneration
#' 
#' @description
#' 
#' This data set comes from a study carried out at the University of Iowa
#' investigating the genetic causes of primary open-angle glaucoma (GLC) and
#' age-related macular degeneration (AMD). In the study, 400 AMD patients and 400
#' GLC patients were recruited, and their genotype determined at roughly 500,000
#' genetic loci. The idea here is that each group could serve as the control group
#' group for the other disease.
#' 
#' This is a subset of the data; it does not consider all 500,000 genetic loci, but
#' rather a subset of 497 SNPs from 30 genes that have been previously suggested
#' may play a role in AMD.
#' 
#' @format
#' 
#' * `y`: Disease status: either `'glc'` or `'amd'`
#' 
#' * `x`: A matrix with 800 rows and 699 columns of SNP genotypes (0/1/2)
#' 
#' * `group`: For each SNP, this is the nearest gene. There are 30 genes
#'   altogether.
#' 
#' @source
#' 
#' Scheetz TE, Fingert JH, Wang K, Kuehn MH, Knudtson KL, Alward WLM, Boldt HC,
#' Russell SR, Folk JC, Casavant TL Braun TA, Clark AF, Stone EM, and Sheffield
#' VC (2013). A Genome-Wide Association Study for Primary Open Angle Glaucoma and
#' Macular Degeneration Reveals Novel Loci. *PLOS ONE* **8**:
#' e58657. <https://doi.org/10.1371/journal.pone.0058657>
#' 
#' @name glcamd
NULL
