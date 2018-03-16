
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



# bind_pan <- function(direction = c('x', 'y', 'xy'), ...) {
#
#   p <- list(...)
#
#   if (grepl('x', direction)) {
#
#     lapply(p, function(x) {
#
#       l_bind_state(x, 'panX', function(W) updatePan(W, direction = 'x'))
#
#     })
#
#   }
#
#
#   if (grepl('y', direction)) {
#
#     lapply(p, function(y) {
#
#       l_bind_state(y, 'panY', function(W) updatePan(W, direction = 'y'))
#
#     })
#
#   }
#
#
#   updatePan <- function(widget, direction = c('x', 'y')) {
#
#     pan_state <- ifelse(direction == 'x', 'panX', 'panY')
#
#     pan_val <- l_cget(widget, pan_state)
#
#     for (w in p) {
#
#       w[pan_state] <- pan_val
#     }
#
#   }
#
# }

