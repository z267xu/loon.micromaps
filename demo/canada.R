
library(rgdal)
library(rgeos)
library(dplyr)

# Read in data
cd <- readOGR(dsn = "data/canada_cd", layer = 'gcd_000b06a_e', stringsAsFactors = F)
canada_data <- read.csv('data/canada_cd/canada_cd_2006.csv', stringsAsFactors = F)

cd@data$CDUID <- as.integer(cd@data$CDUID)
cd@data$ind <- 1:nrow(cd@data)

# Subset to Southern Ontario census divisions
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

# Set parameters for micromaps
grouping <- 7

ord.var <- 'pct_immigrants'
ord.var.axis <- 'Percent'
ord.var.label <- '% Immigrant Population'

var2 <- 'employ_rate_15plus'
var2.axis <- 'Percent'
var2.label <- 'Employment Rate, 15 Years and Older'

lab.label <- 'Census Divisions'

cds <- cd_southon@data$CDNAME


mm <- l_micromaps(spdf = cd_southon, labels = cds, grouping = grouping,
                  ord.var = ord.var, ord.var.axis = ord.var.axis, ord.var.label = ord.var.label,
                  var2 = var2, var2.axis = var2.axis, var2.label = var2.label,
                  lab.label = lab.label)

mm2 <- l_micromaps(spdf = cd_southon, labels = cds, grouping = grouping,
                   ord.var = ord.var, ord.var.axis = ord.var.axis, ord.var.label = ord.var.label,
                   lab.label = lab.label)

