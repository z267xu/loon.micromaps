#' @export
l_getPlots.l_ccmaps <- function(target){
    lapply(target$maps$base,
           function(plot) {
             l_create_handle(plot)
           }
    )
}

#' @export
l_getLocations.l_ccmaps <-  function(target){
  n <- 9
  matrix(1:n,
         nrow = 3)
}
