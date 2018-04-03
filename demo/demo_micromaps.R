

mm <- l_micromaps(spdf = USstates, labels = states, grouping = grouping,
                  ord.var = ord.var, ord.var.axis = ord.var.axis, ord.var.label = ord.var.label,
                  var2 = var2, var2.axis = var2.axis, var2.label = var2.label,
                  lab.label = lab.label)

mm2 <- l_micromaps(spdf = USstates, labels = states, grouping = grouping,
                   ord.var = 'ed', ord.var.axis = ord.var.axis,
                   lab.label = lab.label)
