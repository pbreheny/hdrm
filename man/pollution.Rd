\name{pollution}
\docType{data}
\alias{pollution}

\title{Air pollution and mortality in US metropolitan
areas}
\description{
Excessive air pollution is thought to have a negative long-term effect
on health. In this study, the ``pollution potential'' (the product of
the tons emitted per day per square kilometer of a pollutant and a
dispersion factor which accounts for mixing height, wind speed, number
of episode days, and dimension of the area over which measurements were
taken) of three pollutants (hydrocarbons, nitrogen oxides, and sulfur
dioxide) was determined for 60 metropolitan statistical areas in the
United States. The outcome for this analysis is the total age-adjusted
mortality rate in the metropolitan area. To account for potentially
confounding effects, a number of demographic variables were included in
the analysis as well.

}
\section{Dimensions}{
\itemize{
\item
  n = 60
\item
  p = 15
}

}
\format{
\itemize{
\item
  \code{y}: Total age-adjusted mortality from all causes (Annual
  deaths per 100,000 people)
\item
  \code{X}: Rows of \code{X} are labeled with the name of the
  metropolitan area; columns are as follows:

  \itemize{
    \item
    \code{Precip}: Mean annual precipitation (inches)
  \item
    \code{Humidity}: Percent relative humidity (annual average at
    1:00pm)
  \item
    \code{JanTemp}: Mean January temperature (degrees F)
  \item
    \code{JulyTemp}: Mean July temperature (degrees F)
  \item
    \code{Over65}: Percentage of the population aged 65 years or over
  \item
    \code{House}: Population per household
  \item
    \code{Educ}: Median number of school years completed for persons
    25 years or older
  \item
    \code{Sound}: Percentage of the housing that is sound with all
    facilities
  \item
    \code{Density}: Population density (in persons per square mile of
    urbanized area)
  \item
    \code{NonWhite}: Percentage of population that is nonwhite
  \item
    \code{WhiteCol}: Percentage of employment in white collar
    occupations
  \item
    \code{Poor}: Percentage of households with annual income under
    $3,000 in 1960
  \item
    \code{HC}: Pollution potential of hydrocarbons
  \item
    \code{NOX}: Pollution potential of oxides of nitrogen
  \item
    \code{SO2}: Pollution potential of sulfur dioxide
  }
}

}
\references{
I obtained the data from the
\href{https://cran.r-project.org/web/packages/Sleuth3/index.html}{Sleuth3}
package. The original reference is:


McDonald GC and Schwing RC (1973). Instabilities of Regression Estimates
Relating Air Pollution to Mortality. \emph{Technometrics}, \bold{15}:
463-481.

}
