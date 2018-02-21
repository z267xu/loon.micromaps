
# Data
library(micromap)
library(dplyr)

data("USstates")
data("edPov")

# Pre-processing
grouping <- 8
ord.by <- 'pov'

n <- nrow(edPov)
n_groups <- ceiling(nrow(edPov)/grouping)
edPov <- edPov[order(edPov$pov, decreasing = T), ]
edPov$group <- rep(1:n_groups, each = grouping)[1:n]

USstates@data <- merge(USstates@data, edPov, by.x = 'ST', by.y = 'StateAb', sort = F)
USstates@data <- USstates@data[USstates@plotOrder, ]

df <- USstates@data

# Set up loon
tt <- tktoplevel()
tktitle(tt) <- 'Micromaps'

rng <- range(df$pov)
palette <- loon_palette(grouping)
bg_col <- 'white'

p_stat_base <- vector(length = n_groups)
p_stat <- vector(length = n_groups)

p_map_base <- vector(length = n)
p_map_group <- vector(length = n)
p_map <- vector("list",length = n)

p_label_base <- vector(length = n_groups)
p_label <- vector(length = n_groups)
p_label2 <- vector(length = n_groups)


# Create plots
for (i in 1:n_groups) {

  data <- df[df$group == i, ] %>% arrange(-pov)

  data$colors <- palette[1:nrow(data)]

  p_stat_base[i] <- l_plot(parent = tt,
                           x = seq(from = rng[1], to = rng[2], length.out = grouping),
                           y = seq(grouping, 1),
                           color = bg_col,
                           background = bg_col,
                           showLabels = F,
                           showGuides = F)

  p_stat[i] <- l_layer_points(p_stat_base[i],
                              x = data$pov,
                              y = seq(grouping, grouping - nrow(data) + 1),
                              color = palette[1:nrow(data)],
                              size = 8)


  p_map_base[i] <- l_plot(parent = tt)

  p_map_group[i] <- l_layer_group(p_map_base[i])

  p_map[[i]] <- l_layer(p_map_base[i], parent = p_map_group[i],
                        USstates,
                        color = 'cornsilk',
                        asSingleLayer = FALSE)

  l_scaleto_world(p_map_base[i])


  cols <- data.frame(ST = df$ST, colors = NA, stringsAsFactors = F)

  cols$colors <- data$colors[match(cols$ST, data$ST)]
  cols$colors[is.na(cols$colors)] <- 'cornsilk'


  for (j in 1:length(slot(USstates, "polygons"))) {
    for (k in 1:length(slot(USstates@polygons[[USstates@plotOrder[j]]], "Polygons"))) {

      l_configure(c(p_map_base[i], p_map[[i]][[j]][k]),
                  color = cols$colors[j])

    }
  }


  p_label_base[i] <- l_plot(parent = tt,
                            x = 1:grouping,
                            y = seq(grouping, 1),
                            color = bg_col,
                            background = bg_col,
                            showLabels = F,
                            showGuides = F)

  p_label[i] <- l_layer_points(p_label_base[i],
                               x = rep(2, nrow(data)),
                               y = seq(grouping, grouping - nrow(data) + 1),
                               color = palette[1:nrow(data)],
                               size = 8)

  p_label2[i] <- l_layer_texts(p_label_base[i],
                               x = rep(3, nrow(data)),
                               y = seq(grouping, grouping - nrow(data) + 1),
                               text = as.character(data$ST_NAME),
                               anchor = 'w')

}


# Layout
label_lab <- tcl('label', l_subwin(tt,'label_for_labels'), text = 'States')
stat_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = 'Percent Families Living Below Poverty Level')
map_lab <- tcl('label', l_subwin(tt,'label_for_maps'), text = 'Map')


tkgrid(label_lab, row = 0, column = 0, sticky = "nesw")
tkgrid(stat_lab, row = 0, column = 1, sticky = "nesw")
tkgrid(map_lab, row = 0, column = 2, sticky = "nesw")

tkgrid.rowconfigure(tt, 0, weight = 1)


for (ii in 1:n_groups) {

  tkgrid(p_label_base[ii], row = ii, column = 0, sticky = 'nesw')
  tkgrid(p_stat_base[ii], row = ii, column = 1, sticky = 'nesw')
  tkgrid(p_map_base[ii], row = ii, column = 2, sticky = 'nesw')

  tkgrid.rowconfigure(tt, ii, weight = 5)

}

for (jj in 0:2) {

  tkgrid.columnconfigure(tt, jj, weight = 5)

}
