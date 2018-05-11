
#' Example Dataset: Southern Ontario Census Division Polygons
#'
#' An object of class 'SpatialPolygonsDataFrame' created from subsetting
#' a shapefile of Canadian census division boundaries. There are 39 census divisions.
#'
#' @format The format is formal class 'SpatialPolygonsDataFrame' [package "sp"].
#' @source Statistics Canada (\url{http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcd_000b06a_e.zip})
#'
"cd_southon"


#' Example Dataset: Canadian Demographic Statistics
#'
#' A dataset comparing demographic statistics of 288 Canadian census divisions
#' from the 2006 Census.
#'
#' @format A data frame with 288 rows and the following 11 columns:
#' \describe{
#'   \item{id}{Census division id.}
#'   \item{population}{Population.}
#'   \item{pop_density}{Population density.}
#'   \item{pct_immigrants}{Immigrant population percentage.}
#'   \item{bachelor_above}{Percentage of population with Bachelor's degree education or above.}
#'   \item{median_earnings}{Median annual earnings.}
#'   \item{employment}{Number of employed individuals.}
#'   \item{employ_rate_15plus}{Employment rate, those aged 15 or older.}
#'   \item{employ_rate_25_54}{Employment rate, those aged between 25 and 54.}
#'   \item{pop_density_cuberoot}{Cube root of population density.}
#'   \item{population_cuberoot}{Cube root of population.}
#' }
#' @source Chen, Xin. 2011. “Visual Analysis of Immigration in Canada Based on Conditioned Choropleth Maps.” December. University of Waterloo.
#' @note This is just an example data set.
#'
"canada_demographics"


#' Example Dataset: U.S. States Polygons
#'
#' An object of class 'SpatialPolygonsDataFrame' that was created from a
#' shapefile of U.S. state borders. Washington DC is treated as a separate area/polygon.
#'
#' @format The format is formal class 'SpatialPolygonsDataFrame' [package "sp"].
#' @source micromap R package
#' @note This is just an example data set.
#'
"USstates"


#' Example Dataset: U.S. Education and Poverty Levels
#'
#' A dataset comparing education and poverty levels among the states. Washington
#' DC is treated as a separate data point.
#'
#' @format A data frame with 51 rows and the following 5 columns:
#' \describe{
#'   \item{state}{Full state name.}
#'   \item{ed}{Education rate.}
#'   \item{pov}{Poverty rate.}
#'   \item{region}{Major U.S. region.}
#'   \item{StateAb}{Abbreviated state name.}
#' }
#' @source micromap R package
#' @note This is just an example data set.
#'
"edPov"


#' Example Dataset: U.S. Lung and Bronchus Cancer Mortality
#'
#' A dataset comparing the lung and bronchus cancer mortality rates with
#' fine particulate matter concentration levels and income per capita, 2009-2011 average.
#' Washington DC is treated as a separate data point.
#'
#' @format A data frame with 51 rows and the following 5 columns:
#' \describe{
#'   \item{state}{Full state name.}
#'   \item{state.abbr}{Abbreviated state name.}
#'   \item{lung_bronch_death}{Lung and bronchus cancer mortality rate.}
#'   \item{pm25}{Outdoor fine particulate matter (PM2.5) level.}
#'   \item{income}{Annual income per capita.}
#' }
#' @source CDC U.S. Cancer Statistics (\url{https://nccd.cdc.gov/uscs/cancersbystateandregion.aspx}),
#'   CDC WONDER Portal (\url{https://wonder.cdc.gov/NASA-PM.html}), and
#'   U.S. Bureau of Economic Analysis (\url{https://www.bea.gov/iTable/index_regional.cfm})
#' @note This is just an example data set.
#'
"us_lungmort"
