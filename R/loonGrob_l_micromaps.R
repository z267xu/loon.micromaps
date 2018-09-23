#' @export
loonGrob_layoutType.l_micromaps <- function(target) {
  "arrangeGrobArgs"
}

#' @export
l_get_arrangeGrobArgs.l_micromaps <- function(target){

  titles <- target$titles

  ncol  <-  length(target$scatterplots) + 2
  nrow <- length(target$labels$base)

  # xlims
  xlimsGrob <- lapply(1:ncol,
                      function(i){
                        if(i == 1) rectGrob(gp = gpar(fill = "grey94", col = "NA"))
                        else if(i == ncol) rectGrob(gp = gpar(fill = "grey94", col = "NA"))
                        else {
                          p <- l_create_handle(target$p_scale_base[i - 1])
                          loonGrob(p)
                        }
                      }
  )

  aGrobs <- arrangeGrob(
    grobs = c(
      lapply(target$labels$base,
             function(plot) {
               p <- l_create_handle(plot)
               loonGrob(p)
             }
      ),
      do.call(
        c,
        lapply(target$scatterplots,
               function(plots) {
                 lapply(plots,
                        function(plot) {
                          p <- l_create_handle(plot)
                          loonGrob(p)
                        }
                 )
               }

        )
      ),
      lapply(target$maps$base,
             function(plot) {
               p <- l_create_handle(plot)
               loonGrob(p)
             }
      ),
      xlimsGrob
    ),
    layout_matrix = rbind(
        matrix(rep(1: (ncol * nrow), each = 5),
               ncol = ncol, byrow = FALSE),
        ((ncol * nrow) + 1) : ((nrow + 1) * ncol)
    ),
    name = "scatterplots"
  )

  # pack titles
  ## if titles length is not equal to the row numbers,  titles won't be packed and give warnings
  tt <- gridExtra::ttheme_minimal(
    base_size = 8,
    core=list(bg_params = list(fill = 'grey94', col=NA))
  )

  if(length(titles)  == ncol) {
    aGrobs <- rbind(
      tableGrob(matrix(titles,
                       ncol = ncol),
                theme = tt
      ),
      aGrobs,
      size = "last"
    )
  } else {
    warning("the length of titles is not equal to the column of the scatterplots")
  }

  # pack xlabels
  aGrobs <- rbind(
    aGrobs,
    tableGrob(matrix(c("", target$xlabels, ""),
                     ncol = ncol),
              theme = tt
    ),
    size = "first"
  )

  list(
    grobs = gList(
      aGrobs
    ),
    layout_matrix = matrix(1),
    name = "l_micromaps"
  )
}


#' @export
l_getPlots.l_micromaps <- function(target){
  c(
    lapply(target$labels$base,
           function(plot) {
             l_create_handle(plot)
           }
    ),
    do.call(
      c,
      lapply(target$scatterplots,
             function(plots) {
               lapply(plots,
                      function(plot) {
                        l_create_handle(plot)
                      }
               )
             }

      )
    ),
    lapply(target$maps$base,
           function(plot) {
             l_create_handle(plot)
           }
    )
  )
}

#' @export
l_getLocations.l_micromaps <-  function(target){
  ncol <-  length(target$scatterplots) + 2
  nrow <- length(target$labels$base)
  matrix(1: (ncol * nrow),
         ncol = ncol, byrow = FALSE)
}
