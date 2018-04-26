
library(loon.micromaps)
library(micromap)


# Read in data -----
data("USstates")
statedata <- read.csv("data/usa_states/statedata.csv")

USstates@data <- merge(USstates@data, statedata, by.x = "ST", by.y = "state.abbr")


# Draw micromaps -----
mm <- l_micromaps(spdf = USstates,
                  lab.label = 'States',
                  variables = list(id.var = 'ST_NAME',
                                   grouping.var = list(name = 'lung_bronch_death'),
                                   var2 = list(name = 'pm25', label = 'Fine Particulate Matter Level'),
                                   var3 = list(name = 'income', label = 'Income per Capita')),
                  linkingGroup = 'States', sync = 'push',
                  spacing = 'max',
                  showItemLabels = TRUE, itemLabel = as.character(USstates@data$ST_NAME))


# Draw CCmaps -----
cc <- l_ccmaps(spdf = USstates,
               respvar = 'lung_bronch_death', respvar.lab = 'Lung & Bronchus Cancer Death Rate',
               cond1var = 'pm25', cond1var.lab = 'Fine Particulate Matter Level',
               cond2var = 'income', cond2var.lab = 'Income per Capita',
               optimize = TRUE, otry = 10,
               title = 'CCmaps')
# Takes ~40s to run. For faster results without optimizing R^2, set optimize=FALSE

