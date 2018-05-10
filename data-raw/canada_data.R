
library(rgdal)
library(rgeos)
library(dplyr)

# Reading in data
cd <- readOGR(dsn = "data-raw", layer = 'gcd_000b06a_e', stringsAsFactors = F)
canada_demographics <- read.csv('data-raw/canada_cd_2006.csv', stringsAsFactors = F)


# Subset to Southern Ontario census divisions
southon_cds <- c("Stormont", "Dundas and Glengarry", "Prescott and Russell", "Ottawa",
                 "Leeds and Grenville", "Lanark", "Frontenac", "Lennox and Addington",
                 "Hastings", "Prince Edward", "Northumberland", "Peterborough", "Kawartha Lakes",
                 "Durham", "York", "Toronto", "Peel", "Dufferin", "Wellington",
                 "Halton", "Hamilton", "Niagara", "Haldimand-Norfolk", "Brant",
                 "Waterloo", "Perth", "Oxford", "Elgin", "Chatham-Kent", "Essex",
                 "Lambton", "Middlesex", "Huron", "Bruce", "Grey", "Simcoe", "Muskoka",
                 "Haliburton", "Renfrew", "Nipissing", "Parry Sound")

cd@data$CDUID <- as.integer(cd@data$CDUID)
cd@data$ind <- 1:nrow(cd@data)

southon_ids <- cd@data %>%
  filter(PRNAME == 'Ontario' & CDNAME %in% southon_cds) %>%
  select(ind) %>%
  unlist() %>%
  as.character() %>% as.numeric()

cd_southon <- cd[cd@data$ind %in% southon_ids, ]
data <- cd_southon@data
cd_southon <- gSimplify(cd_southon, tol = 0.05) %>%
  SpatialPolygonsDataFrame(., data)


# Save data to package
devtools::use_data(cd_southon, overwrite = TRUE)
devtools::use_data(canada_demographics, overwrite = TRUE)

