
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
southon_ids <- cd@data %>%
  filter(PRNAME == 'Ontario', CDTYPE != 'DIS', CDNAME != 'Greater Sudbury / Grand Sudbury') %>%
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
l_micromaps(spdf = cd_southon,
            lab.label = 'Census Divisions',
            variables = list(id.var = 'CDNAME',
                             grouping.var = list(name = 'pct_immigrants',
                                                 xlab = NULL,
                                                 label = '% Immigrant Population'),
                             var3 = list(name = 'pop_density', xlab = 'Per KM^2')),
            glyph = 'square', linkingGroup = 'Southern_ON', sync = 'push')


# Draw CCmaps -----
l_ccmaps(spdf = cd_southon,
         respvar = 'pct_immigrants', respvar.lab = '% Immigrant Population',
         cond1var = 'employ_rate_25_54', cond1var.lab = 'Employment Rate, Age 25-54',
         cond2var = 'pop_density', cond2var.lab = 'Population Density',
         seg1col = 'yellow', seg3col = 'orange')

