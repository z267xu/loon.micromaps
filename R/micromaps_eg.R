
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
# USstates@data <- USstates@data[USstates@plotOrder, ]

USstates@data$NAME <- USstates@data$ST_NAME

df <- USstates@data
df$id <- 1:nrow(df)


# Some regions have subpolygons - this provides a key to map between the polygons and regions
plotorder_f <- Map(function(x) {

  n <- length(USstates@polygons[[x]]@Polygons)
  rep(x, times = n)

}, USstates@plotOrder) %>% unlist()


mapping <- data.frame(sort_key = plotorder_f)


# Set up loon
tt <- tktoplevel()
tktitle(tt) <- 'Micromaps'

rng <- range(df$pov)
palette <- loon_palette(grouping)


p_stat <- vector(length = n_groups)

p_map_base <- vector(length = n_groups)
p_map <- vector('list', length = n_groups)


p_label <- vector(length = n_groups)
p_label_txt <- vector(length = n_groups)


mapping <- vector('list', length = n_groups)
mapping_scat2map <- vector('list', length = n_groups)
mapping_map2scat <- vector('list', length = n_groups)
b <- vector(length = n_groups)
b2 <- vector(length = n_groups)
b3 <- vector(length = n_groups)

# Create plots
for (i in 1:n_groups) {

  data <- df[df$group == i, ] %>% arrange(-pov)

  data$colors <- palette[1:nrow(data)]


  # Scatterplot
  p_stat[i] <- l_plot(parent = tt,
                      x = data$pov,
                      y = seq(grouping, grouping - nrow(data) + 1),
                      color = palette[1:nrow(data)],
                      size = 6)

  l_configure(p_stat[i],
              panX = rng[1] - 1, zoomX = 1, deltaX = diff(rng)*1.1,
              panY = 0, zoomY = 1, deltaY = grouping + 1,
              xlabel = '', ylabel = '')


  # Map
  p_map_base[i] <- l_plot(parent = tt)

  p_map[[i]] <- l_layer(p_map_base[i],
                      USstates,
                      color = 'cornsilk',
                      asSingleLayer = TRUE,
                      label = 'map')

  l_scaleto_world(p_map_base[i])


  cols <- data.frame(ST = df$ST, key = 1:nrow(df), colors = NA, stringsAsFactors = F)

  cols$colors <- data$colors[match(cols$ST, data$ST)]
  cols$colors[is.na(cols$colors)] <- 'cornsilk'

  mapping[[i]] <- data.frame(sort_key = plotorder_f,
                           colors = cols$colors[match(plotorder_f, cols$key)],
                           stringsAsFactors = F)

  l_configure(c(p_map_base[i], p_map[[i]]), color = mapping[[i]]$colors)


  # for (j in 1:length(slot(USstates, "polygons"))) {
  #   for (k in 1:length(slot(USstates@polygons[[USstates@plotOrder[j]]], "Polygons"))) {
  #
  #     l_configure(c(p_map_base[i], p_map[i][[j]][k]),
  #                 color = cols$colors[j])
  #
  #   }
  # }


  # Labels
  p_label[i] <- l_plot(parent = tt,
                       x = rep(1, nrow(data)),
                       y = seq(grouping, grouping - nrow(data) + 1),
                       color = palette[1:nrow(data)],
                       size = 6)

  l_configure(p_label[i], panX = 0, zoomX = 1, deltaX = 6,
              panY = 0, zoomY = 1, deltaY = grouping + 1,
              xlabel = '', ylabel = '')

  p_label_txt[i] <- l_layer_texts(p_label[i],
                                  x = rep(2, nrow(data)),
                                  y = seq(grouping, grouping - nrow(data) + 1),
                                  text = as.character(data$ST_NAME),
                                  anchor = 'w',
                                  size = 6,
                                  col = 'black')


  mapping_scat2map[[i]] <- lapply(data$NAME, function(x) which(attr(p_map[[i]], 'NAME') == x))
  names(mapping_scat2map[[i]]) <- data$NAME

  # mapping_map2scat <- sapply(attr(p_map, 'NAME'), function(x) which(data$NAME == x))
  # mapping_map2scat <- Filter(function(x) length(x) > 0, mapping_map2scat)

  mapping_map2scat[[i]] <- match(attr(p_map[[i]], 'NAME'), data$NAME)

  updateMap_sp <- function(i) {

    sel <- l_cget(p_stat[i], 'selected')

    j <- unlist(mapping_scat2map[[i]][sel])

    orig_cols <- mapping[[i]]$colors
    new_cols <- orig_cols
    new_cols[j] <- 'magenta'

    l_configure(p_map[[i]], color = new_cols)

  }

  assign_b <- function(i) {

    force(i)

    l_bind_state(p_stat[i], 'selected', function() updateMap_sp(i))

  }

  b[i] <- assign_b(i)


  updatePlot_sp <- function(add, i) {

    j <- l_currentindex(p_map_base[i])

    if(j !=-1 && !is.na(mapping_map2scat[[i]][j])) {

      if (add) {

        l_configure(p_stat[i], selected = TRUE, which = mapping_map2scat[[i]][j])

      } else {

        sel <- rep(FALSE, l_cget(p_stat[i], 'n'))
        sel[mapping_map2scat[[i]][j]] <- TRUE
        l_configure(p_stat[i], selected = sel)

      }
    } else {

      l_configure(p_stat[i], selected = FALSE)

    }

  }

  assign_b2 <- function(i) {

    force(i)

    l_bind_item(p_map_base[i], paste0(p_map[[i]]), '<ButtonPress-1>',
                function(){updatePlot_sp(FALSE, i)})

  }

  b2[i] <- assign_b2(i)

  assign_b3 <- function(i) {

    force(i)

    l_bind_item(p_map_base[i], paste0(p_map[[i]]), '<Shift-ButtonPress-1>',
                function(){updatePlot_sp(TRUE, i)})

  }

  b3[i] <- assign_b3(i)

}


# Layout
label_lab <- tcl('label', l_subwin(tt,'label_for_labels'), text = 'States')
stat_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = 'Percent Families Living Below Poverty Level')
map_lab <- tcl('label', l_subwin(tt,'label_for_maps'), text = 'Map')


tkgrid(label_lab, row = 0, column = 0, sticky = "nesw")
tkgrid(stat_lab, row = 0, column = 1, sticky = "nesw")
tkgrid(map_lab, row = 0, column = 2, sticky = "nesw")


for (ii in 1:n_groups) {

  tkgrid(p_label[ii], row = ii, column = 0, sticky = 'nesw')
  tkgrid(p_stat[ii], row = ii, column = 1, sticky = 'nesw')
  tkgrid(p_map_base[ii], row = ii, column = 2, sticky = 'nesw')

  tkgrid.rowconfigure(tt, ii, weight = 4)

}

for (jj in 0:2) {

  tkgrid.columnconfigure(tt, jj, weight = 5)

}

