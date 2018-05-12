
library(rgdal)
library(rgeos)
library(dplyr)

# Canadian census division spatial data -----
cd_Canada_2006 <- readOGR(dsn = "data-raw", layer = 'gcd_000b06a_e', stringsAsFactors = F)
# Source: http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcd_000b06a_e.zip

format(object.size(cd_Canada_2006), units = 'Mb')
# 79.7 Mb

lapply(cd_Canada_2006@polygons, function(x) {
  lapply(x@Polygons, function(y) y@hole)
}) %>% unlist %>% sum()
# 30 holes


# Simplify geometry while preserving topology -----
data_2006 <- cd_Canada_2006@data
cd_Canada_2006 <- gSimplify(cd_Canada_2006, tol = 0.05, topologyPreserve = TRUE) %>%
  SpatialPolygonsDataFrame(., data_2006)

format(object.size(cd_Canada_2006), units = 'Mb')
# 26.9 Mb

lapply(cd_Canada_2006@polygons, function(x) {
  lapply(x@Polygons, function(y) y@hole)
}) %>% unlist %>% sum()
# 30 holes preserved


# Remove holes -----
holes_coords <-lapply(cd_Canada_2006@polygons, function(x) {

  Map(function(y) {

    if (y@hole) {
      y@coords
    }

  }, x@Polygons)

}) %>% unlist(recursive = F) %>%
  Filter(Negate(is.null), .)
# The 30 holes all have fairly small areas --> remove since loon.micromaps
# cannot handle holes

# Source: https://github.com/cran/wild1/blob/master/R/remove.holes.r
remove.holes <- function(Poly){

  require(sp)

  is.hole <- lapply(Poly@Polygons, function(P) P@hole)
  is.hole <- unlist(is.hole)

  polys <- Poly@Polygons[!is.hole]
  Poly <- Polygons(polys, ID = Poly@ID)

  Poly

}

new_polygons <- lapply(cd_Canada_2006@polygons, function(x) {

  remove.holes(x)

})

cd_Canada_2006@polygons <- new_polygons


# Subset to Southern Ontario census divisions -----
SWOntario_cds <- c("Stormont", "Dundas and Glengarry", "Prescott and Russell", "Ottawa",
                   "Leeds and Grenville", "Lanark", "Frontenac", "Lennox and Addington",
                   "Hastings", "Prince Edward", "Northumberland", "Peterborough", "Kawartha Lakes",
                   "Durham", "York", "Toronto", "Peel", "Dufferin", "Wellington",
                   "Halton", "Hamilton", "Niagara", "Haldimand-Norfolk", "Brant",
                   "Waterloo", "Perth", "Oxford", "Elgin", "Chatham-Kent", "Essex",
                   "Lambton", "Middlesex", "Huron", "Bruce", "Grey", "Simcoe", "Muskoka",
                   "Haliburton", "Renfrew", "Nipissing", "Parry Sound")

cd_Canada_2006@data$CDUID <- as.integer(cd_Canada_2006@data$CDUID)
cd_Canada_2006@data$ind <- 1:nrow(cd_Canada_2006@data)

SWOntario_ids <- cd_Canada_2006@data %>%
  filter(PRNAME == 'Ontario' & CDNAME %in% SWOntario_cds) %>%
  select(ind) %>%
  unlist() %>%
  as.character() %>% as.numeric()

cd_SWOntario_2006 <- cd_Canada_2006[cd_Canada_2006@data$ind %in% SWOntario_ids, ]


# Canadian demographics data -----
canada_demographics_2006 <- read.csv('data-raw/canada_cd_2006.csv', stringsAsFactors = F)
# Source: Chen, Xin. 2011. “Visual Analysis of Immigration in Canada Based on Conditioned Choropleth Maps.” December. University of Waterloo


# Save data to package -----
devtools::use_data(cd_Canada_2006, overwrite = TRUE)
devtools::use_data(cd_SWOntario_2006, overwrite = TRUE)
devtools::use_data(canada_demographics_2006, overwrite = TRUE)

