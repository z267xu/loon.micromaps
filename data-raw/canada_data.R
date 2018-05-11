
library(rgdal)
library(rgeos)
library(dplyr)

# Reading in data
cd_Canada_2006 <- readOGR(dsn = "data-raw", layer = 'gcd_000b06a_e', stringsAsFactors = F)
# Source: http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gcd_000b06a_e.zip

canada_demographics_2006 <- read.csv('data-raw/canada_cd_2006.csv', stringsAsFactors = F)
# Source: Chen, Xin. 2011. “Visual Analysis of Immigration in Canada Based on Conditioned Choropleth Maps.” December. University of Waterloo


# Subset to Southern Ontario census divisions
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
data_2006 <- cd_SWOntario_2006@data
cd_SWOntario_2006 <- gSimplify(cd_SWOntario_2006, tol = 0.05) %>%
  SpatialPolygonsDataFrame(., data_2006)


# Save data to package
devtools::use_data(cd_Canada_2006, overwrite = TRUE)
devtools::use_data(cd_SWOntario_2006, overwrite = TRUE)
devtools::use_data(canada_demographics_2006, overwrite = TRUE)

