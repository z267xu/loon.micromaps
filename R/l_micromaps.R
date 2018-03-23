
#' Micromaps in loon
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange_ select_ mutate
#'
#' @examples
#' \dontrun{
#'
#' library(micromap)
#'
#' data("USstates")
#' data("edPov")
#'
#' USstates@data <- merge(USstates@data, edPov, by.x = 'ST', by.y = 'StateAb', sort = F)
#'
#' grouping <- 8
#'
#' ord.var <- 'pov'
#' ord.var.axis <- 'Percent'
#' ord.var.label <- 'Poverty Rate'
#'
#' var2 <- 'ed'
#' var2.axis <- 'Percent'
#' var2.label <- 'Education Rate'
#'
#' lab.label <- 'States'
#'
#' states <- USstates@data$ST_NAME
#'
#' l_micromaps(spdf = USstates, labels = states, grouping = grouping,
#'                ord.var = ord.var, ord.var.axis = ord.var.axis, ord.var.label = ord.var.label,
#'                var2 = var2, var2.axis = var2.axis, var2.label = var2.label,
#'                lab.label = lab.label)
#'
#' }
l_micromaps <- function(spdf, labels, grouping = 5,
                        ord.var, ord.var.axis, ord.var.label,
                        var2 = NULL, var2.axis = NULL, var2.label = NULL,
                        map.label = 'Map', lab.label = 'Labels', title = 'Micromaps') {

  # Input checks
  if (class(spdf) != 'SpatialPolygonsDataFrame') stop('spdf must be of class SpatialPolygonsDataFrame')
  # ...


  # Data pre-processing
  fontsize <- 6
  dotsize <- 4
  palette <- loon_palette(grouping)

  n <- nrow(spdf@data)
  n_groups <- ceiling(n/grouping)

  spdf@data$id <- 1:n
  spdf@data$NAME <- labels

  gr_df <- spdf@data[, c('id', ord.var)]
  gr_df <- gr_df %>%
    arrange_(paste0('-', ord.var)) %>%
    mutate(linkingKey = 0:(n-1)) %>%
    mutate(group = linkingKey %/% grouping + 1) %>%
    select_(paste0('-', ord.var))

  spdf@data <- merge(spdf@data, gr_df, by = 'id', sort = F)

  plotorder_exp <- Map(function(x) {

    n <- length(spdf@polygons[[x]]@Polygons)
    rep(x, times = n)

  }, spdf@plotOrder) %>% unlist()


  rng1 <- range(spdf@data[[ord.var]])

  if (!is.null(var2)) {
    rng2 <- range(spdf@data[[var2]])
  }


  # Initialize loon
  tt <- tktoplevel()
  tktitle(tt) <- title

  p_stat1 <- vector(length = n_groups)

  if (!is.null(var2)) {
    p_stat2 <- vector(length = n_groups)
  }

  p_map_base <- vector(length = n_groups)
  p_map <- vector('list', length = n_groups)

  p_label <- vector(length = n_groups)
  p_label_txt <- vector(length = n_groups)

  color_df <- vector('list', length = n_groups)

  mapping_scat2map <- vector('list', length = n_groups)
  mapping_map2scat <- vector('list', length = n_groups)



  # Create plots
  # Create plots
  for (i in 1:n_groups) {

    data <- spdf@data[spdf@data$group == i, ] %>%
      arrange_(paste0('-', ord.var))

    data$colors <- palette[1:nrow(data)]


    # Scatterplot(s)
    p_stat1[i] <- l_plot(parent = tt,
                         x = data[[ord.var]],
                         y = seq(grouping, grouping - nrow(data) + 1),
                         color = palette[1:nrow(data)],
                         size = dotsize,
                         linkingKey = data$linkingKey,
                         linkingGroup = 'micromap')

    l_configure(p_stat1[i],
                panX = rng1[1] * 0.9, zoomX = 1, deltaX = diff(rng1) * 1.1,
                panY = 0, zoomY = 1, deltaY = grouping + 1,
                showLabels = T, xlabel = '', ylabel = '')


    if (!is.null(var2)) {

      p_stat2[i] <- l_plot(parent = tt,
                           x = data[[var2]],
                           y = seq(grouping, grouping - nrow(data) + 1),
                           color = palette[1:nrow(data)],
                           size = dotsize,
                           linkingKey = data$linkingKey,
                           linkingGroup = 'micromap')

      l_configure(p_stat2[i],
                  panX = rng2[1] * 0.9, zoomX = 1, deltaX = diff(rng2) * 1.1,
                  panY = 0, zoomY = 1, deltaY = grouping + 1,
                  showLabels = T, xlabel = '', ylabel = '')

    }


    # Maps
    p_map_base[i] <- l_plot(parent = tt)

    p_map[[i]] <- l_layer(p_map_base[i],
                          spdf,
                          color = 'cornsilk',
                          asSingleLayer = TRUE,
                          label = 'map')

    l_scaleto_world(p_map_base[i])



    color_df[[i]] <- data.frame(key = plotorder_exp,
                                color = data$colors[match(plotorder_exp, data$id)],
                                stringsAsFactors = F)
    color_df[[i]]$color[is.na(color_df[[i]]$color)] <- 'cornsilk'

    l_configure(c(p_map_base[i], p_map[[i]]), color = color_df[[i]]$color)


    # Labels
    p_label[i] <- l_plot(parent = tt,
                         x = rep(1, nrow(data)),
                         y = seq(grouping, grouping - nrow(data) + 1),
                         color = palette[1:nrow(data)],
                         size = dotsize,
                         linkingKey = data$linkingKey,
                         linkingGroup = 'micromap')

    l_configure(p_label[i], panX = 0, zoomX = 1, deltaX = 6,
                panY = 0, zoomY = 1, deltaY = grouping + 1,
                xlabel = '', ylabel = '')

    p_label_txt[i] <- l_layer_texts(p_label[i],
                                    x = rep(2, nrow(data)),
                                    y = seq(grouping, grouping - nrow(data) + 1),
                                    text = as.character(data$NAME),
                                    anchor = 'w',
                                    size = fontsize,
                                    col = 'black')


    mapping_scat2map[[i]] <- lapply(data$NAME, function(x) which(attr(p_map[[i]], 'NAME') == x))
    names(mapping_scat2map[[i]]) <- data$NAME

    mapping_map2scat[[i]] <- match(attr(p_map[[i]], 'NAME'), data$NAME)

  }


  # Bindings

  ## Zoom/pan/delta of scatterplots
  b_xmove1 <- do.call('bind_zoompandelta', c(direction = 'x', as.list(p_stat1)))

  if (!is.null(var2)) {
    b_xmove2 <- do.call('bind_zoompandelta', c(direction = 'x', as.list(p_stat2)))
  }

  ## Connecting scatterplot points to polygons in maps
  b_scat2map <- lapply(1:n_groups, function(x) {
    bind_scat2map(s = p_stat1[x], m = p_map[[x]],
                  mapping = mapping_scat2map[[x]], orig_colors = color_df[[x]]$color)
  })

  b_map2scat <- lapply(1:n_groups, function(x) {
    bind_map2scat(s = p_stat1[x], m = p_map[[x]], m_base = p_map_base[x],
                  mapping = mapping_map2scat[[x]])
  })

  b_map2scat_add <- lapply(1:n_groups, function(x) {
    bind_map2scat_add(s = p_stat1[x], m = p_map[[x]], m_base = p_map_base[x],
                      mapping = mapping_map2scat[[x]])
  })



  # Label axis for scatterplots
  xs <- c(seq(0.2, 0.8, by = 0.2), 0.5)
  xs1_t <- round(xs * (diff(rng1) * 1.1) + rng1[1] * 0.9, 1)
  axis_txt1 <- sprintf('%.1f', xs1_t[1:4])

  p_scale1 <- l_plot(parent = tt,
                     x = xs1_t,
                     y = c(rep(0.8, 4), 0.4), color = 'black')

  l_configure(p_scale1,
              panX = rng1[1] * 0.9, zoomX = 1, deltaX = diff(rng1) * 1.1,
              panY = 0, zoomY = 1, deltaY = 1,
              background = 'gray95', foreground = 'gray95', # Ensures same color as background
              showLabels = T, xlabel = '', ylabel = '',
              minimumMargins = c(0, 20, 0, 20), showScales = F)


  p_glyph1 <- l_glyph_add_text(p_scale1, text = c(axis_txt1, ord.var.axis))
  p_scale1['glyph'] <- p_glyph1


  if (!is.null(var2)) {

    xs2_t <- round(xs * (diff(rng2) * 1.1) + rng2[1] * 0.9, 1)
    axis_txt2 <- sprintf('%.1f', xs2_t[1:4])

    p_scale2 <- l_plot(parent = tt,
                       x = xs2_t,
                       y = c(rep(0.8, 4), 0.4), color = 'black')

    l_configure(p_scale2,
                panX = rng2[1] * 0.9, zoomX = 1, deltaX = diff(rng2) * 1.1,
                panY = 0, zoomY = 1, deltaY = 1,
                background = 'gray95', foreground = 'gray95',
                showLabels = T, xlabel = '', ylabel = '',
                minimumMargins = c(0, 20, 0, 20), showScales = F)

    p_glyph2 <- l_glyph_add_text(p_scale2, text = c(axis_txt2, var2.axis))
    p_scale2['glyph'] <- p_glyph2

  }


  # Packing
  # Layout
  label_lab <- tcl('label', l_subwin(tt,'label_for_labels'), text = lab.label)

  stat1_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = ord.var.label)

  if (!is.null(var2)) {
    stat2_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = var2.label)
  }

  map_lab <- tcl('label', l_subwin(tt,'label_for_maps'), text = map.label)


  tkgrid(label_lab, row = 0, column = 0, sticky = "nesw")
  tkgrid(stat1_lab, row = 0, column = 1, sticky = "nesw")

  if (!is.null(var2)) {
    tkgrid(stat2_lab, row = 0, column = 2, sticky = "nesw")
    map_col <- 3
  } else {
    map_col <- 2
  }

  tkgrid(map_lab, row = 0, column = map_col, sticky = "nesw")


  for (ii in 1:n_groups) {

    tkgrid(p_label[ii], row = ii, column = 0, sticky = 'nesw')
    tkgrid(p_stat1[ii], row = ii, column = 1, sticky = 'nesw')

    if (!is.null(var2)) {
      tkgrid(p_stat2[ii], row = ii, column = 2, sticky = 'nesw')
    }

    tkgrid(p_map_base[ii], row = ii, column = map_col, sticky = 'nesw')

    tkgrid.rowconfigure(tt, ii, weight = 2)

  }


  for (jj in 0:map_col) {

    tkgrid.columnconfigure(tt, jj, weight = 2)

  }


  tkgrid(p_scale1, row = n_groups + 1, column = 1, sticky = 'news')
  tkgrid.rowconfigure(tt, n_groups + 1, weight = 3, minsize = 30)

  if (!is.null(var2)) {

    tkgrid(p_scale2, row = n_groups + 1, column = 2, sticky = 'news')
    tkgrid.rowconfigure(tt, n_groups + 1, weight = 3, minsize = 30)

  }

  l_resize(tt, 1000, 800)

}
