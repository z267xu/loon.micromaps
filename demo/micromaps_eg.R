
# Data
library(micromap)
library(dplyr)

data("USstates")
data("edPov")

# Pre-processing
grouping <- 8
ord.by <- 'pov'
var2 <- 'ed'


USstates@data <- merge(USstates@data, edPov, by.x = 'ST', by.y = 'StateAb', sort = F)
USstates@data$NAME <- USstates@data$ST_NAME


n <- nrow(edPov)
n_groups <- ceiling(nrow(edPov)/grouping)

USstates@data$id <- 1:nrow(USstates@data)


sort_df <- USstates@data[, c('id', ord.by)]
sort_df <- sort_df %>%
  arrange(-pov) %>%
  mutate(linkingKey = 0:(n-1)) %>%
  mutate(grouping = linkingKey %/% grouping + 1) %>%
  select(-pov)

USstates@data <- merge(USstates@data, sort_df, by = 'id', sort = F)

df <- USstates@data


# Some regions have subpolygons - this provides a key to map between the polygons and regions
plotorder_f <- Map(function(x) {

  n <- length(USstates@polygons[[x]]@Polygons)
  rep(x, times = n)

}, USstates@plotOrder) %>% unlist()


# Set up loon
tt <- tktoplevel()
tktitle(tt) <- 'Micromaps'

rng1 <- range(df$pov)
rng2 <- range(df$ed)
palette <- loon_palette(grouping)

p_stat1 <- vector(length = n_groups)
p_stat2 <- vector(length = n_groups)

p_map_base <- vector(length = n_groups)
p_map <- vector('list', length = n_groups)

p_label <- vector(length = n_groups)
p_label_txt <- vector(length = n_groups)

mapping <- vector('list', length = n_groups)
mapping_scat2map <- vector('list', length = n_groups)
mapping_map2scat <- vector('list', length = n_groups)
b_scat2map <- vector(length = n_groups)
b_map2scat <- vector(length = n_groups)
b_map2scat_add <- vector(length = n_groups)

# Create plots
for (i in 1:n_groups) {

  data <- df[df$group == i, ] %>% arrange(-pov)

  data$colors <- palette[1:nrow(data)]


  # Scatterplot
  p_stat1[i] <- l_plot(parent = tt,
                       x = data$pov,
                       y = seq(grouping, grouping - nrow(data) + 1),
                       color = palette[1:nrow(data)],
                       size = 6,
                       linkingKey = data$linkingKey,
                       linkingGroup = 'micromap')

  l_configure(p_stat1[i],
              panX = rng1[1]*0.9, zoomX = 1, deltaX = diff(rng1)*1.1,
              panY = 0, zoomY = 1, deltaY = grouping + 1,
              xlabel = '', ylabel = '')


  p_stat2[i] <- l_plot(parent = tt,
                       x = data$ed,
                       y = seq(grouping, grouping - nrow(data) + 1),
                       color = palette[1:nrow(data)],
                       size = 6,
                       linkingKey = data$linkingKey,
                       linkingGroup = 'micromap')

  l_configure(p_stat2[i],
              panX = rng2[1]*0.9, zoomX = 1, deltaX = diff(rng2)*1.1,
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


  cols <- data.frame(ST = df$ST, key = df$id, colors = NA, stringsAsFactors = F)

  cols$colors <- data$colors[match(cols$ST, data$ST)]
  cols$colors[is.na(cols$colors)] <- 'cornsilk'

  mapping[[i]] <- data.frame(sort_key = plotorder_f,
                             colors = cols$colors[match(plotorder_f, cols$key)],
                             stringsAsFactors = F)

  l_configure(c(p_map_base[i], p_map[[i]]), color = mapping[[i]]$colors)


  # Labels
  p_label[i] <- l_plot(parent = tt,
                       x = rep(1, nrow(data)),
                       y = seq(grouping, grouping - nrow(data) + 1),
                       color = palette[1:nrow(data)],
                       size = 6,
                       linkingKey = data$linkingKey,
                       linkingGroup = 'micromap')

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


  mapping_map2scat[[i]] <- match(attr(p_map[[i]], 'NAME'), data$NAME)

}


b_xmove1 <- do.call('bind_zoompandelta', c(direction = 'x', as.list(p_stat1)))
b_xmove2 <- do.call('bind_zoompandelta', c(direction = 'x', as.list(p_stat2)))


b_scat2map <- lapply(1:n_groups, function(x) {
  bind_scat2map(s = p_stat1[x], m = p_map[[x]],
                mapping = mapping_scat2map[[x]], orig_colors = mapping[[x]]$color)
})

b_map2scat <- lapply(1:n_groups, function(x) {
  bind_map2scat(s = p_stat1[x], m = p_map[[x]], m_base = p_map_base[x],
                mapping = mapping_map2scat[[x]])
})


b_map2scat_add <- lapply(1:n_groups, function(x) {
  bind_map2scat_add(s = p_stat1[x], m = p_map[[x]], m_base = p_map_base[x],
                    mapping = mapping_map2scat[[x]])
})



# Label axis at the bottom
xs <- c(seq(0.2, 0.8, by = 0.2), 0.5)
xs1_t <- round(xs*(diff(rng1)*1.1) + rng1[1]*0.9, 1)

p_scale1 <- l_plot(parent = tt,
                   x = xs1_t,
                   y = c(rep(0.8, 4), 0.4), color = 'black',
                   showScales = F, showLabels = F)

l_configure(p_scale1,
            panX = rng1[1]*0.9, zoomX = 1, deltaX = diff(rng1)*1.1,
            panY = 0, zoomY = 1, deltaY = 1,
            background = 'gray95')


p_glyph1 <- l_glyph_add_text(p_scale1, text = as.character(c(xs1_t[1:4], 'Percent')))
p_scale1['glyph'] <- p_glyph1



xs2_t <- round(xs*(diff(rng2)*1.1) + rng2[1]*0.9, 1)


p_scale2 <- l_plot(parent = tt,
                   x = xs2_t,
                   y = c(rep(0.8, 4), 0.4), color = 'black',
                   showScales = F, showLabels = F)

l_configure(p_scale2,
            panX = rng2[1]*0.9, zoomX = 1, deltaX = diff(rng2)*1.1,
            panY = 0, zoomY = 1, deltaY = 1,
            background = 'gray95')


p_glyph2 <- l_glyph_add_text(p_scale2, text = as.character(c(xs2_t[1:4], 'Percent')))
p_scale2['glyph'] <- p_glyph2





# Layout
label_lab <- tcl('label', l_subwin(tt,'label_for_labels'), text = 'States')
stat1_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = 'Poverty Rate')
stat2_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = 'Education Rate')
map_lab <- tcl('label', l_subwin(tt,'label_for_maps'), text = 'Map')


tkgrid(label_lab, row = 0, column = 0, sticky = "nesw")
tkgrid(stat1_lab, row = 0, column = 1, sticky = "nesw")
tkgrid(stat2_lab, row = 0, column = 2, sticky = "nesw")
tkgrid(map_lab, row = 0, column = 3, sticky = "nesw")


for (ii in 1:n_groups) {

  tkgrid(p_label[ii], row = ii, column = 0, sticky = 'nesw')
  tkgrid(p_stat1[ii], row = ii, column = 1, sticky = 'nesw')
  tkgrid(p_stat2[ii], row = ii, column = 2, sticky = 'nesw')
  tkgrid(p_map_base[ii], row = ii, column = 3, sticky = 'nesw')

  tkgrid.rowconfigure(tt, ii, weight = 2)

}

for (jj in 0:3) {

  tkgrid.columnconfigure(tt, jj, weight = 2)

}

tkgrid(p_scale1, row = n_groups + 1, column = 1, sticky = 'news')
tkgrid.rowconfigure(tt, n_groups + 1, weight = 3, minsize = 30)

tkgrid(p_scale2, row = n_groups + 1, column = 2, sticky = 'news')
tkgrid.rowconfigure(tt, n_groups + 1, weight = 3, minsize = 30)

l_resize(tt, 1000, 800)

