
# Linked micromaps and conditioned choropleth maps display using data
# related to U.S. lung and bronchus mortality rates

local({

  data("USstates", package = "micromap")
  data("us_lungmort_2009_2011", package = "loon.micromaps")

  USstates@data <- merge(USstates@data, us_lungmort_2009_2011,
                         by.x = "ST", by.y = "state.abbr")


  readline("Create linked micrmoaps display. Press the <enter> key to continue: ")

  mm <- l_micromaps(lab.label = 'States',
                    spdf = USstates,
                    variables = list(id.var = 'ST_NAME',
                                     grouping.var = list(name = 'lung_bronch_death'),
                                     var2 = list(name = 'pm25', label = 'Fine Particulate Matter Level'),
                                     var3 = list(name = 'income', label = 'Income per Capita')),
                    linkingGroup = 'States', sync = 'push',
                    spacing = 'equal',
                    showItemLabels = TRUE, itemLabel = as.character(USstates@data$ST_NAME))


  readline("Create conditioned choropleth maps display. Press the <enter> key to continue: ")

  print('Optimizing for R^2; this can take a while.')

  cc <- l_ccmaps(spdf = USstates, title = 'CCmaps',
                 respvar = 'lung_bronch_death', respvar.lab = 'Lung & Bronchus Cancer Death Rate',
                 cond1var = 'pm25', cond1var.lab = 'Fine Particulate Matter Level',
                 cond2var = 'income', cond2var.lab = 'Income per Capita',
                 respscale = 'actual', cond1scale = 'actual', cond2scale = 'actual',
                 optimize = TRUE)

})

