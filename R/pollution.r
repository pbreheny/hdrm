#' Air pollution and mortality in US metropolitan areas
#' 
#' @description
#' 
#' Excessive air pollution is thought to have a negative long-term effect on
#' health.  In this study, the "pollution potential" (the product of the tons
#' emitted per day per square kilometer of a pollutant and a dispersion factor
#' which accounts for mixing height, wind speed, number of episode days, and
#' dimension of the area over which measurements were taken) of three pollutants
#' (hydrocarbons, nitrogen oxides, and sulfur dioxide) was determined for 60
#' metropolitan statistical areas in the United States.  The outcome for this
#' analysis is the total age-adjusted mortality rate in the metropolitan area.  To
#' account for potentially confounding effects, a number of demographic variables
#' were included in the analysis as well.
#' 
#' @format
#' 
#' * `y`: Total age-adjusted mortality from all causes (Annual deaths per 100,000
#'   people)
#' 
#' * `X`: A matrix with 60 rows and 15 columns. Rows of `X` are labeled with the
#'   name of the metropolitan area. Columns are as follows:
#'   * `Precip`: Mean annual precipitation (inches)
#'   * `Humidity`: Percent relative humidity (annual average at 1:00pm)
#'   * `JanTemp`: Mean January temperature (degrees F)
#'   * `JulyTemp`: Mean July temperature (degrees F)
#'   * `Over65`: Percentage of the population aged 65 years or over
#'   * `House`: Population per household
#'   * `Educ`: Median number of school years completed for persons 25 years or older
#'   * `Sound`: Percentage of the housing that is sound with all facilities
#'   * `Density`: Population density (in persons per square mile of urbanized area)
#'   * `NonWhite`: Percentage of population that is nonwhite
#'   * `WhiteCol`: Percentage of employment in white collar occupations
#'   * `Poor`: Percentage of households with annual income under $3,000 in 1960
#'   * `HC`: Pollution potential of hydrocarbons
#'   * `NOX`: Pollution potential of oxides of nitrogen
#'   * `SO2`: Pollution potential of sulfur dioxide
#' 
#' @source
#' 
#' I obtained the data from the
#' [Sleuth3](https://cran.r-project.org/web/packages/Sleuth3/index.html) package.
#' The original reference is:
#' 
#' McDonald GC and Schwing RC (1973). Instabilities of Regression Estimates
#' Relating Air Pollution to Mortality. *Technometrics*, **15**: 463-481.
#' 
#' @name pollution
NULL
