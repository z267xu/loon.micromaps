
#' Micromaps in loon
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange_ select_ mutate
#' @importFrom purrr walk
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
#' mm <- l_micromaps(spdf = USstates, labels = states, grouping = grouping,
#'                   ord.var = ord.var, ord.var.axis = ord.var.axis, ord.var.label = ord.var.label,
#'                   var2 = var2, var2.axis = var2.axis, var2.label = var2.label,
#'                   lab.label = lab.label)
#'
#' }
l_micromaps <- function(tt = tktoplevel(), var_inspector = TRUE,
                        spdf, labels, grouping = 5,
                        ord.var, ord.var.axis, ord.var.label = NULL,
                        var2 = NULL, var2.axis = NULL, var2.label = NULL,
                        map.label = 'Map', lab.label = 'Labels', title = 'Micromaps') {

  .Tcl('set ::loon::Options(printInConfigurationWarning) FALSE')

  # Input checks
  if (class(spdf) != 'SpatialPolygonsDataFrame') stop('spdf must be of class SpatialPolygonsDataFrame')
  # ...


  if (is.null(ord.var.label) | identical(ord.var.label, '')) ord.var.label <- ord.var
  if ((is.null(var2.label) | identical(var2.label, '')) & !is.null(var2)) var2.label <- var2



  delta <- function(rng, zoom = 1, scale) {

    ((rng[2] - scale*rng[1])/(diff(rng)*zoom)) * 1.01

  }


  # Data pre-processing
  fontsize <- 6
  dotsize <- 4
  palette <- loon_palette(grouping)

  n <- nrow(spdf@data)
  n_groups <- ceiling(n/grouping)

  spdf@data$id <- 1:n
  spdf@data$NAME <- labels

  gr_df <- spdf@data[, c('id', ord.var, 'NAME')]
  gr_df <- gr_df %>%
    arrange_(.dots = c(paste0('-', ord.var), 'NAME')) %>%
    mutate(linkingKey = 0:(n-1)) %>%
    mutate(group = linkingKey %/% grouping + 1) %>%
    select_(.dots = c(paste0('-', ord.var), paste0('-', 'NAME')))

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
  # tt <- tktoplevel()
  tktitle(tt) <- title

  p_scat1 <- vector(length = n_groups)

  if (!is.null(var2)) {
    p_scat2 <- vector(length = n_groups)
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
      arrange_(.dots = c(paste0('-', ord.var), 'NAME'))

    data$colors <- palette[1:nrow(data)]


    # Scatterplot(s)
    p_scat1[i] <- l_plot(parent = tt,
                         x = data[[ord.var]],
                         y = seq(grouping, grouping - nrow(data) + 1),
                         color = palette[1:nrow(data)],
                         size = dotsize,
                         linkingKey = data$linkingKey,
                         linkingGroup = 'micromap')

    l_configure(p_scat1[i],
                panX = rng1[1] * 0.98, zoomX = 1, deltaX = diff(rng1)*delta(rng1, scale = 0.98),
                panY = 0, zoomY = 1, deltaY = grouping + 1,
                showLabels = T, xlabel = '', ylabel = '')


    if (!is.null(var2)) {

      p_scat2[i] <- l_plot(parent = tt,
                           x = data[[var2]],
                           y = seq(grouping, grouping - nrow(data) + 1),
                           color = palette[1:nrow(data)],
                           size = dotsize,
                           linkingKey = data$linkingKey,
                           linkingGroup = 'micromap')

      l_configure(p_scat2[i],
                  panX = rng2[1] * 0.98, zoomX = 1, deltaX = diff(rng2)*delta(rng2, scale = 0.98),
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


  # Label axis for scatterplots
  xs <- seq(0.2, 0.8, by = 0.2)
  xs1_t <- round(xs * (diff(rng1) * 1.01) + rng1[1] * 0.98, 1)
  xlim1 <- c(0, 1) * (diff(rng1) * 1.01) + rng1[1] * 0.98
  axis_txt1 <- sprintf('%.1f', xs1_t)

  p_scale1 <- l_plot(parent = tt,
                     x = xs1_t, y = rep(0.3, 4),
                     color = 'black')

  l_configure(p_scale1,
              panX = rng1[1] * 0.98, zoomX = 1, deltaX = diff(rng1)*delta(rng1, scale = 0.98),
              panY = 0, zoomY = 1, deltaY = 1,
              background = 'gray95', foreground = 'gray95', # Ensures same color as background
              showLabels = T, xlabel = '', ylabel = '',
              minimumMargins = c(0, 20, 0, 20), showScales = F)

  p_scale1_l1 <- l_layer_line(p_scale1,
                              x = xlim1, y = c(0.7, 0.7))
  p_scale1_l2 <- l_layer_line(p_scale1,
                              x = c(xs1_t[1], xs1_t[1]), y = c(0.6, 0.8))
  p_scale1_l3 <- l_layer_line(p_scale1,
                              x = c(xs1_t[2], xs1_t[2]), y = c(0.6, 0.8))
  p_scale1_l4 <- l_layer_line(p_scale1,
                              x = c(xs1_t[3], xs1_t[3]), y = c(0.6, 0.8))
  p_scale1_l5 <- l_layer_line(p_scale1,
                              x = c(xs1_t[4], xs1_t[4]), y = c(0.6, 0.8))


  p_glyph1 <- l_glyph_add_text(p_scale1, text = axis_txt1)
  p_scale1['glyph'] <- p_glyph1



  if (!is.null(var2)) {

    xs2_t <- round(xs * (diff(rng2) * 1.01) + rng2[1] * 0.98, 1)
    xlim2 <- c(0, 1) * (diff(rng2) * 1.01) + rng2[1] * 0.98
    axis_txt2 <- sprintf('%.1f', xs2_t)

    p_scale2 <- l_plot(parent = tt,
                       x = xs2_t, y = rep(0.3, 4),
                       color = 'black')

    l_configure(p_scale2,
                panX = rng2[1] * 0.98, zoomX = 1, deltaX = diff(rng2)*delta(rng2, scale = 0.98),
                panY = 0, zoomY = 1, deltaY = 1,
                background = 'gray95', foreground = 'gray95',
                showLabels = T, xlabel = '', ylabel = '',
                minimumMargins = c(0, 20, 0, 20), showScales = F)


    p_scale2_l1 <- l_layer_line(p_scale2,
                                x = xlim2, y = c(0.7, 0.7))
    p_scale2_l2 <- l_layer_line(p_scale2,
                                x = c(xs2_t[1], xs2_t[1]), y = c(0.6, 0.8))
    p_scale2_l3 <- l_layer_line(p_scale2,
                                x = c(xs2_t[2], xs2_t[2]), y = c(0.6, 0.8))
    p_scale2_l4 <- l_layer_line(p_scale2,
                                x = c(xs2_t[3], xs2_t[3]), y = c(0.6, 0.8))
    p_scale2_l5 <- l_layer_line(p_scale2,
                                x = c(xs2_t[4], xs2_t[4]), y = c(0.6, 0.8))


    p_glyph2 <- l_glyph_add_text(p_scale2, text = axis_txt2)
    p_scale2['glyph'] <- p_glyph2

  }



  # Bindings

  ## Zoom/pan/delta of scatterplots
  b_xmove1 <- do.call('bind_zoompandelta', c(direction = 'x', as.list(c(p_scat1, p_scale1))))

  if (!is.null(var2)) {
    b_xmove2 <- do.call('bind_zoompandelta', c(direction = 'x', as.list(c(p_scat2, p_scale2))))
  }

  ## Connecting scatterplot points to polygons in maps
  b_scat2map <- lapply(1:n_groups, function(x) {
    bind_scat2map(s = p_scat1[x], m = p_map[[x]],
                  mapping = mapping_scat2map[[x]], orig_colors = color_df[[x]]$color)
  })

  b_map2scat <- lapply(1:n_groups, function(x) {
    bind_map2scat(s = p_scat1[x], m = p_map[[x]], m_base = p_map_base[x],
                  mapping = mapping_map2scat[[x]])
  })

  b_map2scat_add <- lapply(1:n_groups, function(x) {
    bind_map2scat_add(s = p_scat1[x], m = p_map[[x]], m_base = p_map_base[x],
                      mapping = mapping_map2scat[[x]])
  })

  b_disable <- lapply(1:n_groups, function(x) {
    disable_zoompandelta(direction = 'xy', p_label[x])
  })



  old <- as.character(tkgrid.slaves(tt))
  walk(old, function(x) tkgrid.forget(x))


  # Packing

  # Layout
  label_lab <- tcl('label', l_subwin(tt,'label_for_labels'), text = lab.label)

  stat1_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = ord.var.label)
  axis1_lab <- tcl('label', l_subwin(tt,'label_for_axis1'), text = ord.var.axis)

  if (!is.null(var2)) {
    stat2_lab <- tcl('label', l_subwin(tt,'label_for_stat_plots'), text = var2.label)
    axis2_lab <- tcl('label', l_subwin(tt,'label_for_axis2'), text = var2.axis)
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
    tkgrid(p_scat1[ii], row = ii, column = 1, sticky = 'nesw')

    if (!is.null(var2)) {
      tkgrid(p_scat2[ii], row = ii, column = 2, sticky = 'nesw')
    }

    tkgrid(p_map_base[ii], row = ii, column = map_col, sticky = 'nesw')

    tkgrid.rowconfigure(tt, ii, weight = 2)

  }


  for (jj in 0:map_col) {

    tkgrid.columnconfigure(tt, jj, weight = 2)

  }


  tkgrid(p_scale1, row = n_groups + 1, column = 1, sticky = 'news')
  tkgrid.rowconfigure(tt, n_groups + 1, weight = 3, minsize = 20)
  tkgrid(axis1_lab, row = n_groups + 2, column = 1, sticky = 'news')


  if (!is.null(var2)) {

    tkgrid(p_scale2, row = n_groups + 1, column = 2, sticky = 'news')
    tkgrid.rowconfigure(tt, n_groups + 1, weight = 3, minsize = 20)
    tkgrid(axis2_lab, row = n_groups + 2, column = 2, sticky = 'news')

  }

  if (!is.null(var2)) {
    l_resize(tt, 1000, 800)
  } else {
    l_resize(tt, 750, 800)
  }



  # Return plot layers
  if ((!is.null(var2))) {
    ret <- list(top = tt,
                labels = list(base = p_label, text = p_label_txt),
                scatterplots.1 = p_scat1,
                scatterplots.2 = p_scat2,
                maps = list(base = p_map_base, polygons = p_map))
  } else {
    ret <- list(top = tt,
                labels = list(base = p_label, text = p_label_txt),
                scatterplots.1 = p_scat1,
                maps = list(base = p_map_base, polygons = p_map))
  }



  # Inspector - variable selector
  mmInspector <- function(w) {

    tt_inspector <- tktoplevel()
    tktitle(tt_inspector) <- 'Micromaps Variable Inspector'


    vars <- setdiff(names(spdf@data)[sapply(spdf@data, is.numeric)],
                    c('id', 'name', 'NAME', 'group', 'linkingKey'))


    ord.var_i <- tclVar(ord.var)
    box.ord.var <- ttkcombobox(tt_inspector, values = vars,
                               textvariable = ord.var_i,
                               state = 'readonly')
    ord.var.label_i <- tclVar(ord.var.label)
    entry.ord.var.label <- tkentry(tt_inspector, textvariable = ord.var.label_i, width = 20)
    ord.var.axis_i <- tclVar(ord.var.axis)
    entry.ord.var.axis <- tkentry(tt_inspector, textvariable = ord.var.axis_i, width = 20)



    var2_i <- tclVar(ifelse(is.null(var2), 'NA', var2))
    box.var2 <- ttkcombobox(tt_inspector, values = c('NA', vars),
                            textvariable = var2_i,
                            state = 'readonly')
    var2.label_i <- tclVar(ifelse(is.null(var2.label), '', var2.label))
    entry.var2.label <- tkentry(tt_inspector, textvariable = var2.label_i, width = 20)
    var2.axis_i <- tclVar(ifelse(is.null(var2.axis), '', var2.axis))
    entry.var2.axis <- tkentry(tt_inspector, textvariable = var2.axis_i, width = 20)


    submit <- tkbutton(tt_inspector, text = 'Submit', command = function() updatemm())


    tkgrid(tklabel(tt_inspector, text = 'Ordering variable: '), box.ord.var,
           tklabel(tt_inspector, text = 'Plot label: '), entry.ord.var.label,
           tklabel(tt_inspector, text = 'Axis label: '), entry.ord.var.axis,
           sticky = 'sw', padx = 5, pady = 5)

    tkgrid(tklabel(tt_inspector, text = 'Second variable (optional): '), box.var2,
           tklabel(tt_inspector, text = 'Plot label: '), entry.var2.label,
           tklabel(tt_inspector, text = 'Axis label: '), entry.var2.axis,
           sticky = 'sw', padx = 5, pady = 5)


    tkgrid(submit, sticky = 'ew', padx = 5, pady = 5, column = 5)


    updatemm <- function() {

      ord.var_new <- tclvalue(ord.var_i)
      ord.var.axis_new <- tclvalue(ord.var.axis_i)
      ord.var.label_new <- tclvalue(ord.var.label_i)

      var2_new <- tclvalue(var2_i)

      if (var2_new == 'NA') var2_new <- NULL

      var2.axis_new <- tclvalue(var2.axis_i)
      var2.label_new <- tclvalue(var2.label_i)


      spdf@data <- spdf@data[, !(names(spdf@data) %in% c('linkingKey', 'group'))]

      l_micromaps(tt = w$top, var_inspector = FALSE,
                  spdf = spdf, labels = labels, grouping = grouping,
                  ord.var = ord.var_new, ord.var.axis = ord.var.axis_new, ord.var.label = ord.var.label_new,
                  var2 = var2_new, var2.axis = var2.axis_new, var2.label = var2.label_new,
                  map.label = map.label, lab.label = lab.label, title = title)


    }

    tt_inspector

  }

  if (var_inspector) mmInspector(ret)


  return(invisible(ret))

}
