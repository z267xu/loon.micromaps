

#' Example Dataset: Canadian Census Division Polygons, 2006 Census
#'
#' An object of class 'SpatialPolygonsDataFrame' created from subsetting
#' a shapefile of Canadian census division boundaries. There are 288 census divisions.
#'
#' @format The format is formal class 'SpatialPolygonsDataFrame' [package "sp"].
#' @source Statistics Canada (\url{http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcd_000b06a_e.zip})
#'
"cd_Canada_2006"


#' Example Dataset: South-Western Ontario Census Division Polygons
#'
#' An object of class 'SpatialPolygonsDataFrame' created from subsetting
#' a shapefile of Canadian census division boundaries. There are 39 census divisions.
#'
#' @format The format is formal class 'SpatialPolygonsDataFrame' [package "sp"].
#' @source Statistics Canada (\url{http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcd_000b06a_e.zip})
#'
"cd_SWOntario_2006"


#' Example Dataset: Canadian Demographic Statistics, 2006 Census
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
#'   \item{employ_rate_15plus}{Employment rate, those aged 15 and older.}
#'   \item{employ_rate_25_54}{Employment rate, those aged between 25 and 54.}
#'   \item{pop_density_cuberoot}{Cube root of population density.}
#'   \item{population_cuberoot}{Cube root of population.}
#' }
#' @source Chen, Xin. 2011. “Visual Analysis of Immigration in Canada Based on Conditioned Choropleth Maps.” December. University of Waterloo.\cr
#' Originally from Statistics Canada (\url{http://www12.statcan.gc.ca/census-recensement/2006/dp-pd/hlt/index-eng.cfm}).
#' @note This is just an example data set.
#'
"canada_demographics_2006"


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
"us_lungmort_2009_2011"
