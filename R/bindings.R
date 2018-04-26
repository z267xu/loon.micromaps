
# Bind zoom, pan and delta along x and/or y direction for a list of widgets
bind_zoompandelta <- function(direction = c('x', 'y', 'xy'), ...) {

  p <- list(...)

  if (grepl('x', direction)) {

    lapply(p, function(x) {

      l_bind_state(x, c('zoomX', 'panX', 'deltaX'), function(W) updateZoomPanDelta(W, direction = 'x'))

    })

  }


  if (grepl('y', direction)) {

    lapply(p, function(y) {

      l_bind_state(y, c('zoomY', 'panY', 'deltaY'), function(W) updateZoomPanDelta(W, direction = 'y'))

    })

  }


  updateZoomPanDelta <- function(widget, direction = c('x', 'y')) {

    d_upper <- toupper(direction)

    zoom_val <- l_cget(widget, paste0('zoom', d_upper))
    pan_val <- l_cget(widget, paste0('pan', d_upper))
    delta_val <- l_cget(widget, paste0('delta', d_upper))


    if (direction == 'x') {

      for (w in p) {
        l_configure(w, zoomX = zoom_val, panX = pan_val, deltaX = delta_val)
      }

    } else {

      for (w in p) {
        l_configure(w, zoomY = zoom_val, panY = pan_val, deltaY = delta_val)
      }

    }

  }

}



# Disable zooming, panning and delta in the x and/or y direction for a list of widgets
disable_zoompandelta <- function(direction = c('x', 'y', 'xy'), ...) {

  p <- list(...)


  if (grepl('x', direction)) {

    lapply(p, function(x) {

      zoomX_val <- l_cget(x, 'zoomX')
      panX_val <- l_cget(x, 'panX')
      deltaX_val <- l_cget(x, 'deltaX')

      l_bind_state(x, c('zoomX', 'panX', 'deltaX'), function(W) l_configure(W, zoomX = zoomX_val,
                                                                            panX = panX_val, deltaX = deltaX_val))

    })

  }


  if (grepl('y', direction)) {

    lapply(p, function(x) {

      zoomY_val <- l_cget(x, 'zoomY')
      panY_val <- l_cget(x, 'panY')
      deltaY_val <- l_cget(x, 'deltaY')

      l_bind_state(x, c('zoomY', 'panY', 'deltaY'), function(W) l_configure(W, zoomY = zoomY_val,
                                                                            panY = panY_val, deltaY = deltaY_val))

    })

  }

}



# Bind scatterplot point 'selected' state to polygon map layer
bind_scat2map_sel <- function(s, m, mapping, plotorder_exp, ids) {

  l_bind_state(s, 'selected', function() updateMap_sp_sel())

  updateMap_sp_sel <- function() {

    sel <- l_cget(s, 'selected')
    j <- unlist(mapping[sel])

    new_colors <- l_cget(s, 'color')

    new_colors_m <- new_colors[match(plotorder_exp, ids)]
    new_colors_m[is.na(new_colors_m)] <- 'cornsilk'
    new_colors_m[j] <- 'magenta'

    l_configure(m, color = new_colors_m)

  }

}



# Bind scatterplot point 'color' state to polygon map layer
bind_scat2map_col <- function(s, m, plotorder_exp, ids) {

  l_bind_state(s, 'color', function() updateMap_sp_col())

  updateMap_sp_col <- function() {

    new_colors <- l_cget(s, 'color')

    new_colors_m <- new_colors[match(plotorder_exp, ids)]
    new_colors_m[is.na(new_colors_m)] <- 'cornsilk'

    l_configure(m, color = new_colors_m)

  }

}


# Bind polygon layer selection to scatterplot points
# Selection on maps is done using left mouse click
bind_map2scat <- function(s, m, m_base, mapping) {

  l_bind_item(m_base, paste0(m), '<ButtonPress-1>', function() updatePlot_sp())

  updatePlot_sp <- function() {

    j <- l_currentindex(m_base)

    if (j != -1 && !is.na(mapping[j])) {

      sel <- rep(FALSE, l_cget(s, 'n'))
      sel[mapping[j]] <- TRUE
      l_configure(s, selected = sel)

    } else {

      l_configure(s, selected = FALSE)

    }

  }

}



# Bind multiple polygon layer selections to scatterplot points
# Addition to selection on maps is done using the Shift + left mouse click
bind_map2scat_add <- function(s, m, m_base, mapping) {

  l_bind_item(m_base, paste0(m), '<Shift-ButtonPress-1>', function() updatePlot_sp_add())

  updatePlot_sp_add <- function() {

    j <- l_currentindex(m_base)

    if (j != -1 && !is.na(mapping[j])) {

      l_configure(s, selected = TRUE, which = mapping[j])

    } else {

      l_configure(s, selected = FALSE)

    }

  }

}


