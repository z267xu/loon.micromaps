
# Linked micromaps and conditioned choropleth map displays using data
# on Southern Ontario demographic statistics

local({

  data("cd_southon")
  data("canada_demographics")

  cd_southon@data <- merge(cd_southon@data, canada_demographics,
                           all.x = T, all.y = F, by.y = 'id', by.x = 'CDUID')


  readline("Create linked micrmoaps display. Press the <enter> key to continue: ")

  mm <- l_micromaps(lab.label = 'Census Divisions',
                    spdf = cd_southon,
                    variables = list(id.var = 'CDNAME',
                                     grouping.var = list(name = 'pct_immigrants', xlab = 'Percent'),
                                     var2 = list(name = 'pop_density', xlab = 'Per KM^2'),
                                     var3 = list(name = 'bachelor_above', xlab = 'Percent')),
                    spacing = 'equal', n_groups = 5,
                    linkingGroup = 'Southern_ON', sync = 'push',
                    showItemLabels = TRUE, itemLabel = cd_southon@data$CDNAME)


  readline("Create conditioned choropleth maps display. Press the <enter> key to continue: ")

  print('Optimizing for R^2; this can take a while.')

  cc <- l_ccmaps(spdf = cd_southon,
                 respvar = 'pct_immigrants', respvar.lab = '% Immigrant Population',
                 cond1var = 'bachelor_above', cond1var.lab = '% with Bachelor Education',
                 cond2var = 'pop_density', cond2var.lab = 'Population Density',
                 seg1col = 'yellow', seg3col = 'orange',
                 respscale = 'log', cond1scale = 'actual', cond2scale = 'log',
                 optimize = TRUE)

})

