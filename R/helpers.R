

variable_check <- function(spdf, nm, variable) {

  if (!('name' %in% names(variable))) {
    stop('Must specify name of each variable as list(<varname> = list(name = ...))')
  }

  name <- variable$name

  if (!(name %in% names(spdf@data))) {
    stop(paste0(name, ' variable does not exist in spdf data'))
  }

  if (is.null(variable$xlab) | identical(variable$xlab, '')) {
    xlab <- ''
  } else {
    if (length(variable$xlab) > 1 | !is.character(variable$xlab)) {
      stop(paste0(nm, ' xlab, if specified, must be a single character string'))
    }
    xlab <- variable$xlab
  }

  if (is.null(variable$label) | identical(variable$label, '')) {
    label <- name
  } else {
    if (length(variable$label) > 1 | !is.character(variable$label)) {
      stop(paste0(nm, ' label, if specified, must be a single character string'))
    }
    label <- variable$label
  }

  return(list(name = name, xlab = xlab, label = label))

}
