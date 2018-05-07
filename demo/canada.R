
library(loon.micromaps)
library(rgdal)
library(rgeos)
library(dplyr)

# Reading in data -----
cd <- readOGR(dsn = "data/canada_cd", layer = 'gcd_000b06a_e', stringsAsFactors = F)
canada_data <- read.csv('data/canada_cd/canada_cd_2006.csv', stringsAsFactors = F)

cd@data$CDUID <- as.integer(cd@data$CDUID)
cd@data$ind <- 1:nrow(cd@data)


# Subset to Southern Ontario census divisions -----
southon_cds <- c("Stormont", "Dundas and Glengarry", "Prescott and Russell", "Ottawa",
                 "Leeds and Grenville", "Lanark", "Frontenac", "Lennox and Addington",
                 "Hastings", "Prince Edward", "Northumberland", "Peterborough", "Kawartha Lakes",
                 "Durham", "York", "Toronto", "Peel", "Dufferin", "Wellington",
                 "Halton", "Hamilton", "Niagara", "Haldimand-Norfolk", "Brant",
                 "Waterloo", "Perth", "Oxford", "Elgin", "Chatham-Kent", "Essex",
                 "Lambton", "Middlesex", "Huron", "Bruce", "Grey", "Simcoe", "Muskoka",
                 "Haliburton", "Renfrew", "Nipissing", "Parry Sound")


southon_ids <- cd@data %>%
  filter(PRNAME == 'Ontario' & CDNAME %in% southon_cds) %>%
  select(ind) %>%
  unlist() %>%
  as.character() %>% as.numeric()

cd_southon <- cd[cd@data$ind %in% southon_ids, ]
data <- cd_southon@data
cd_southon <- gSimplify(cd_southon, tol = 0.05) %>%
  SpatialPolygonsDataFrame(., data)

cd_southon@data <- merge(cd_southon@data, canada_data,
                         all.x = T, all.y = F, by.y = 'id', by.x = 'CDUID')


# Draw micromaps -----
mm <- l_micromaps(lab.label = 'Census Divisions',
                  spdf = cd_southon,
                  variables = list(id.var = 'CDNAME',
                                   grouping.var = list(name = 'pct_immigrants',
                                                       xlab = NULL,
                                                       label = '% Immigrant Population'),
                                   var2 = list(name = 'pop_density', xlab = 'Per KM^2')),
                  spacing = 'equal', n_groups = 5,
                  linkingGroup = 'Southern_ON', sync = 'push',
                  showItemLabels = TRUE, itemLabel = cd_southon@data$CDNAME)


# Draw CCmaps -----
cc <- l_ccmaps(spdf = cd_southon,
               respvar = 'pct_immigrants', respvar.lab = '% Immigrant Population',
               cond1var = 'bachelor_above', cond1var.lab = '% with Bachelor Education',
               cond2var = 'pop_density', cond2var.lab = 'Population Density',
               seg1col = 'yellow', seg3col = 'orange',
               respscale = 'percent', cond1scale = 'actual', cond2scale = 'log',
               optimize = TRUE)

