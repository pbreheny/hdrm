\name{whoari}
\docType{data}
\alias{whoari}

\title{World Health Organization study on acute respiratory
illnesses}
\description{
The presentation of an acutely ill young infant presents health workers,
especially those in developing countries, with a very difficult problem.
Serious infections are the main cause of morbidity and mortality in
infants under 3 months of age in these countries, and diagnosing the
severing of the illness is rather difficult.

To study this problem, the World Health Organization (WHO) collected
data on a number of readily accessible variables such as vital signs,
family history, and clinical observations resulting from physical
examination. The patients' disease status was later determined based on
the course of the disease and various laboratory tests. The goal of the
study was to develop a early prediction rule for grading the severity of
the disease so that timely treatment could be delivered (and costly but
unnecessary treatments avoided).

The WHO study looked at several acute respiratory illnesses in several
countries. This data set contains data on pneumonia from the country
Ethiopia.

}
\section{Dimensions}{
\itemize{
\item
  n = 816
\item
  p = 66
}

}
\format{
\itemize{
\item
  \code{y}: A pneumonia score, measured on the following scale: 1=No
  disease, 2=Cold/cough, 3=Pneumonia, 4=Severe pneumonia,
  5=Life-threatening illness.
\item
  \code{X}: Each row of \code{X} represents an individual.

  \itemize{
    \item
    The following columns are objective clinical measurements:

    \itemize{
        \item
      \code{biwt}: Birth weight
    \item
      \code{hcir}: Head circumference
    \item
      \code{wght}: Weight (in grams)
    \item
      \code{lgth}: Length (in centimeters)
    \item
      \code{temp}: Temperature (in Celsius)
    \item
      \code{hrat}: Heart rate
    \item
      \code{age}: Age (in days)
    \item
      \code{rr}: Adjusted respiratory rate
    \item
      \code{waz}: Weight-for-age (z-score)
    }
  \item
    There are also a number of clinical observations, judged on a
    severity scale with higher numbers denoting increased severity. For
    example, 0-3 would represent absent/mild/moderate/severe. Below are
    some of those variables:

    \itemize{
        \item
      \code{hvo}: Vomit more (0-1)
    \item
      \code{bat}: Birth preterm (0-1)
    \item
      \code{hbr}: Engorged breasts (0-1)
    \item
      \code{inc}: Respiratory distress (0-2)
    \item
      \code{sr1}: Respiratory state (0-3)
    \item
      \code{sr2}: Respiratory state (0-3)
    \item
      \code{str}: Stridor (0-2)
    \item
      \code{att}: Attentive (0-2)
    \item
      \code{skr}: Skin rash (0-2)
    \item
      \code{hyp}: Hypotonia (0-3)
    \item
      \code{nut}: Nutritional status score (0-3)
    \item
      \code{oab}: Sclerema (0-1)
    }
  \item
    \href{http://myweb.uiowa.edu/pbreheny/data/whoari.pdf}{The rest of
    the variables are described in this table}. In the table, hx stands
    for history, ausc for auscultation, and hxprob for history of
    problems. The author made an effort to cluster relevant variables
    together; some variables are listed twice as a result.
  }
}

}
\references{
I obtained the data from
\url{http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets}. The original
reference is:


Harrell F, Margolis P, Gove S, Mason K, Mulholland E, Lehmann D, Muhe L,
Gatchalian S and Eichenwald H (1998). Development of a clinical
prediction model for an ordinal outcome: the World Health Organization
multicentre study of clinical signs and etiological agents of pneumonia,
sepsis and meningitis in young infants. \emph{Statistics in Medicine},
\bold{17}: 909-944.

}