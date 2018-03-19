
#' Binding zoom, pan and delta along x and/or y direction for a list of widgets
#'
#' @export
#'
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



#' Bind scatterplot points selection to polygon map layer
#' @export
#'
bind_scat2map <- function(s, m, mapping, orig_colors) {

  l_bind_state(s, 'selected', function() updateMap_sp())

  updateMap_sp <- function() {

    sel <- l_cget(s, 'selected')

    j <- unlist(mapping[sel])

    new_colors <- orig_colors
    new_colors[j] <- 'magenta'

    l_configure(m, color = new_colors)

  }

}



#' Bind polygon layer selection to scatterplot points
#' @export
#'
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



#' Bind polygon layer selections to scatterplot points
#'   (allows for multiple polygons to be selected)
#'
#' @export
#'
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


# disable zooming/panning/delta


