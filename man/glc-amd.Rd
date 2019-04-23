\name{glc-amd}
\docType{data}
\alias{glc-amd}

\title{Genetic association study of age-related macular
degeneration}
\description{
Genetic association studies are an important tool for detecting links
between genetic markers and diseases. Glaucoma (GLC) and age-related
macular degeneration (AMD) are the two leading causes of vision loss in
the United States. The design of this experiment was to carry out a
joint genome-wide association study for both primary open angle glaucoma
and AMD, with a total of 400 patients in each arm.

The original experiment was a genome-wide association study
interrogating the entire genome; the data here are confined to 30 genes
that previous biological studies suggested may be related to the
disease. The genes contained 497 markers with acceptably low rates of
missing data (<20\%) and high minor allele frequency
(>10\%).

}
\section{Dimensions}{
\itemize{
\item
  800 observations
\item
  497 variables
}

}
\format{
\itemize{
\item
  \code{y}: Type of eye disease (GLC, AMD)
\item
  \code{X}: Genotype at a given locus, in terms of number of minor
  alleles present (0/1/2)
\item
  \code{group}: Character vector denoting the nearest gene of
  interest. Note that this does not necessarily mean that the given
  locus is \emph{in} the gene, just that the gene is nearby.
}

}
\section{Annotation}{
\itemize{
\item
  Rows of X, and elements of y, are annotated with arbitrary, matching
  sample labels.
\item
  Columns of X are labeled with
  \href{https://www.ncbi.nlm.nih.gov/snp/docs/RefSNP_about}{dbSNP
  reference (rs) numbers}. A few SNPs did not have rs numbers at the
  time of data collection and are labeled with a SNP probe ID number; if
  you would like to know the genomic position of such a column, e-mail
  me.
}

}
\references{

Scheetz TE, Fingert JH, Wang K, Kuehn MH, Knudtson KL, Alward WLM, Boldt
HC, Russell SR, Folk JC, Casavant TL, Braun TA, Clark AF, Stone EM, and
Sheffield VC. (2013). A Genome-Wide Association Study for Primary Open
Angle Glaucoma and Macular Degeneration Reveals Novel Loci. PLOS ONE, 8:
1-10.

}