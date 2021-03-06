
#' @title Conditioned Choropleth Maps (Conditioned Micromaps) in loon
#'
#' @description 2-way panel of maps for visualizing multivariate data
#'
#' @param tt Tk top level window. Defaults to a new window
#' @param cc_inspector Whether to draw the custom inspector for CCmaps, which
#'   allows for variable selection, variable label update, font size adjustment
#'   and option to optimize \eqn{R^2}. Defaults to TRUE. Once created, the inspector
#'   can only be closed when the main display window is closed
#' @param title Title of the map. Appears in the title bar of the toplevel window.
#'   Defaults to "CCmaps"
#' @param spdf \code{SpatialPolygonsDataFrame} object to hold polygon coordinates
#'   and attributes. It should contain all variables used in analysis
#' @param respvar Name of the response value variable
#' @param respvar.lab Label for response value variable for slider. Defaults to NULL,
#'   in which case \code{respvar} is used
#' @param cond1var Name of the first conditioning variable (controls
#'   panel assignment in the vertical direction)
#' @param cond1var.lab Label for first conditional variable for slider. Defaults to NULL,
#'   in which case \code{cond1} is used
#' @param cond2var Name of the second conditioning variable (controls
#'   panel assignment in the horizontal direction)
#' @param cond2var.lab Label for second conditional variable for slider. Defaults to NULL,
#'   in which case \code{cond2} is used
#' @param respbreaks Determines how the response data is divided into three groups
#'   for coloring scheme. It can either be the integer 2 or a numeric vector of
#'   two middle break points. Defaults to 2, in which case the response values are divided
#'   into tertiles
#' @param cond1breaks Similar to \code{respbreaks}; determines how the first conditioning
#'   variable values are divided into three groups for panel membership
#' @param cond2breaks Similar to \code{respbreaks}; determines how the second
#'   conditioning variable values are divided into three groups for panel membership
#' @param respscale What scale to use for drawing the response variable slider. Must be
#'   one of three values - actual (unchanged), percent (quantiles) and log
#' @param cond1scale What scale to use for drawing the first conditioning variable slider.
#'   Must be one of three values - actual (unchanged), percent (quantiles) and log
#' @param cond2scale What scale to use for drawing the second conditioning variable slider.
#'   Must be one of three values - actual (unchanged), percent (quantiles) and log
#' @param size Font size for model value labels and\eqn{R^2}. Defaults to size 10
#' @param seg1col Color of first interval of points by \code{respvar} value.
#'   Cannot be 'cornsilk'. Defaults to 'blue'
#' @param seg2col Color of second interval of points by \code{respvar} value.
#'   Cannot be 'cornsilk'. Defaults to 'darkgrey'
#' @param seg3col Color of third interval of points by \code{respvar} value.
#'   Cannot be 'cornsilk'. Defaults to 'red'
#' @param optimize Logical value indicating whether panel assignment should be
#'   optimized for \eqn{R^2}. Requires a long time to compute. Defaults to FALSE
#' @param otry Integer (greater than 0) indicating number of values to try
#'   for optimization (see above). Required if \code{optimize = TRUE}.
#'   Defaults to 20. A higher \code{otry} value leads to more precise estimates
#'   at the cost of longer computation time
#'
#' @import loon
#' @import tcltk
#' @import sp
#' @import grDevices
#' @import stats
#' @import utils
#' @import methods
#' @importFrom dplyr mutate mutate_at rowwise funs group_by ungroup summarise select transmute mutate_all sample_n desc
#' @importFrom magrittr %>%
#'
#' @return An object of classes \code{l_ccmaps} and \code{loon}, containing the
#'   Tk toplevel window, \code{respvar} value, \code{cond1var} value,
#'   \code{cond2var} value, and the handles for the \code{loon} map plot objects
#'   in list form
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' ## Get data
#' library(rgdal)
#' library(maptools)
#' columbus <- readOGR(system.file("shapes/columbus.shp", package = "maptools")[1], verbose = F)
#'
#' ## Plot
#' cc <- l_ccmaps(title = "Columbus Residential Burglaries and Vehicle Thefts",
#'                spdf = columbus,
#'                respvar = "CRIME", cond1var = "PLUMB", cond2var = "HOVAL",
#'                respscale = "actual", cond1scale = "actual", cond2scale = "actual",
#'                optimize = FALSE)
#'
#'}
#'
l_ccmaps <- function(tt = tktoplevel(), cc_inspector = TRUE,
                     title = "CCmaps", spdf,
                     respvar, respvar.lab = NULL,
                     cond1var, cond1var.lab = NULL,
                     cond2var, cond2var.lab = NULL,
                     respbreaks = 2, cond1breaks = 2, cond2breaks = 2,
                     respscale = c('actual', 'percent', 'log'),
                     cond1scale = c('actual', 'percent', 'log'),
                     cond2scale = c('actual', 'percent', 'log'),
                     size = 10, seg1col = 'blue', seg2col = 'darkgrey', seg3col = 'red',
                     optimize = FALSE, otry = 20) {


  # Input checks -----
  if (!is(tt, 'tkwin'))
    stop('tt should be a Tk toplevel window generated by tktoplevel()')

  if (!is.logical(cc_inspector)) {
    stop("cc_inspector must be TRUE or FALSE only")
  }

  if (!is(spdf, "SpatialPolygonsDataFrame")) {
    stop("spdf has to be of SpatialPolygonsDataFrame class")}


  if (length(unique(spdf@plotOrder)) != nrow(spdf@data)) {
    stop('plotOrder for spdf does not correspond to the number of data points')
  }


  var_len <- sapply(c('respvar', 'cond1var', 'cond2var', 'cc_inspector',
                      'optimize', 'otry', 'title', 'seg1col', 'seg2col', 'seg3col'),
                    function(x) length(get(x)))

  if (any(var_len) > 1) {
    stop(paste(paste0(names(var_len)[var_len > 1], collapse = ', '),
               "should be of length one"))
  }


  if (!is.character(respvar)) {
    stop("respvar (response variable name) should be of type character")
  }

  if (!(respvar %in% names(spdf))) {
    stop("respvar (response variable name) does not exist in the data supplied")
  }

  if (!is.null(respvar.lab) & (!is.character(respvar.lab) | length(respvar.lab) > 1)) {
    stop('Label for respvar should be a single string or NULL')
  }


  if (!is.character(cond1var)) {
    stop("cond1var (name of first conditioned variable) should be of type character")
  }

  if (!(cond1var %in% names(spdf))) {
    stop("cond1var (first conditioned variable) does not exist in the data supplied")
  }

  if (!is.null(cond1var.lab) & (!is.character(cond1var.lab) | length(cond1var.lab) > 1)) {
    stop('Label for cond1var should be a single string or NULL')
  }

  if (!is.character(cond2var)) {
    stop("cond2var (name of second conditioned variable) should be of type character")
  }

  if (!(cond2var %in% names(spdf))) {
    stop("cond2var (second conditioned variable) does not exist in the data supplied")
  }

  if (!is.null(cond1var.lab) & (!is.character(cond1var.lab) | length(cond1var.lab) > 1)) {
    stop('Label for cond2var should be a single string or NULL')
  }


  for (ii in c('respbreaks', 'cond1breaks', 'cond2breaks')) {

    errmsg <- paste0(ii, ' must be a numeric vector of four break points or the integer 3')

    if (!is.numeric(get(ii))) {
      stop(errmsg)
    }

    if (length(get(ii)) == 1) {
      if (get(ii) != 2) stop(errmsg)
    } else if (length(get(ii)) != 2) {
      stop(errmsg)
    }

  }


  if (!is.character(seg1col)) {
    stop('seg1col must be of type character')
  }

  if (!is.character(seg2col)) {
    stop('seg2col must be of type character')
  }

  if (!is.character(seg3col)) {
    stop('seg3col must be of type character')
  }

  if (any(c('cornsilk', 'CORNSILK', '#fff8dc', '#FFF8DC', '#FFFFF8F8DCDC', '#fffff8f8dcdc') %in% c(seg1col, seg2col, seg3col))) {
    stop('seg1col, seg2col and seg3col cannot be cornsilk, which is reserved for l_ccmaps')
  }


  if (!is.logical(optimize) | length(optimize) > 1) {
    stop("optimize must be TRUE or FALSE only")
  }


  if (optimize && !is.numeric(otry)) {
    stop("otry must be an integer")
  }

  if (optimize && otry %% 1 != 0) {
    stop('otry must be an integer')
  }

  if (optimize && otry < 1) {
    stop("otry must be an integer greater or equal to 1")
  }


  if (!is.character(title)) {
    stop("title must be a character string")
  }


  tt$env$respvar <- respvar
  tt$env$cond1var <- cond1var
  tt$env$cond2var <- cond2var


  if (respscale == 'log') {
    if (any(spdf@data[[respvar]] <= 0)) stop('Cannot use log scale when respvar has zero or negative values')
  }

  if (cond1scale == 'log') {
    if (any(spdf@data[[cond1var]] <= 0)) stop('Cannot use log scale when cond1var has zero or negative values')
  }

  if (cond2scale == 'log') {
    if (any(spdf@data[[cond2var]] <= 0)) stop('Cannot use log scale when cond2var has zero or negative values')
  }


  tt$env$respscale <- respscale
  tt$env$cond1scale <- cond1scale
  tt$env$cond2scale <- cond2scale


  # Data prep -----
  n <- 9 # 9 plots from 3x3 panel

  # Group names, used for determining which regions are assigned to each plot. First number
  # refers to the horizontal conditioning variable category, and second number refers to the
  # vertical conditioning variable
  group <- c(t(outer(1:3, 1:3, FUN = 'paste0')))


  spdf@data$id <- 1:nrow(spdf@data)

  plotorder_exp <- Map(function(x) {

    n <- length(spdf@polygons[[x]]@Polygons)
    rep(x, times = n)

  }, spdf@plotOrder) %>% unlist()



  resp <- spdf@data[[respvar]]
  cond1 <- spdf@data[[cond1var]]
  cond2 <- spdf@data[[cond2var]]

  if (!is.numeric(resp)) {
    stop("Study variable must be numeric")
  }

  if (!is.numeric(cond1) | !is.numeric(cond2)) {
    stop("Conditioning variables must be numeric")
  }



  # Setting up plots -----
  tktitle(tt) <- title


  tt$env$p_base <- vector(length = n)
  tt$env$p <- vector("list",length = n)

  for (i in 1:n) {

    tt$env$p_base[i] <- l_plot(parent = tt)

    tt$env$p[[i]] <- l_layer(tt$env$p_base[i], spdf,
                             color = "cornsilk", asSingleLayer = TRUE)

    l_scaleto_world(tt$env$p_base[i])

  }


  # Determine panel membership of polygons and colors -----

  # If break points not given, use default of equal quantiles. Otherwise, use user-defined
  # break points
  if (identical(respbreaks, 2)) {
    respcuts <- quantile(resp, probs = c(1/3, 2/3))
  } else {
    respcuts <- respbreaks
  }


  # Categorical variable corresponding to the intervals in study variable
  spdf@data$resp_cat <- cut_interval(resp, respcuts)

  attr(spdf@data$resp_cat, 'cuts') <- respcuts
  attr(spdf@data$resp_cat, 'colors') <- assign_colors(resp, respcuts, seg1col, seg2col, seg3col)



  # If optimize=TRUE, break points for conditioning variables are obtained from the optimize()
  # function. Otherwise, use equal quantiles
  if (optimize) {

    results <- r2_optimize(otry, resp, cond1, cond2)

    cond1cuts <- c(results$cond1cut1[1], results$cond1cut2[1])
    cond2cuts <- c(results$cond2cut1[1], results$cond2cut2[1])

  } else {

    if (identical(cond1breaks, 2)) {
      cond1cuts <- quantile(cond1, probs = c(1/3, 2/3))
    } else {
      cond1cuts <- cond1breaks
    }

    if (identical(cond2breaks, 2)) {
      cond2cuts <- quantile(cond2, probs = c(1/3, 2/3))
    } else {
      cond2cuts <- cond2breaks
    }

  }

  # Categorical variables corresponding to the levels in conditioning variables
  # For the vertical conditioning variable, need in reverse order (plotting top to bottom, but
  # the bottom rows represent lower levels)
  spdf@data$cond1_cat <- cut_interval(-cond1, -cond1cuts)
  spdf@data$cond1_cat <- assign_levels(spdf@data$cond1_cat)

  attr(spdf@data$cond1_cat, "cuts") <- cond1cuts

  spdf@data$cond2_cat <- cut_interval(cond2, cond2cuts)
  spdf@data$cond2_cat <- assign_levels(spdf@data$cond2_cat)

  attr(spdf@data$cond2_cat, "cuts") <- cond2cuts


  data_wcol <- function(resp, resp_cat, cond1_cat, cond2_cat) {

    pal <- attr(resp_cat, 'colors')

    dat <- data.frame(id = spdf@data$id,
                      resp = resp,
                      resp_cat = resp_cat,
                      group = factor(paste0(cond1_cat, cond2_cat), levels = group))

    data_exp <- data.frame(id = dat$id[match(plotorder_exp, dat$id)],
                           resp = dat$resp[match(plotorder_exp, dat$id)],
                           resp_cat = dat$resp_cat[match(plotorder_exp, dat$id)],
                           group = as.factor(dat$group[match(plotorder_exp, dat$id)]),
                           stringsAsFactors = F) %>%
      mutate(colors = as.character(factor(resp_cat, labels = pal)))

    data_exp <- cbind(data_exp, model.matrix(~ group - 1, data_exp))

    data_exp %>% rowwise() %>%
      mutate_at(.vars = paste0('group', group),
                .funs = funs(ifelse(. == 0, 'cornsilk', colors)))

  }


  update_colors <- function(data) {

    for (k in 1:n) {

      colname <- paste0('group', group[k])

      l_configure(c(tt$env$p_base[k], tt$env$p[[k]]), color = data[[colname]])

    }
  }


  update_labels <- function(data) {

    data <- data[!duplicated(data$id), ]

    r2 <- r2_calc(data$resp, data$group)
    tcl(tt$env$r2label, 'configure', text = paste0("R^2: ", round(r2, 2)))


    model_values <- attr(r2, 'model_values')


    for (i in 1:n) {

      match <- model_values[model_values$group == group[i], ]

      if (nrow(match) > 0) {

        fitted_r <- round(match[['fitted_value']][1], 2)

        l_configure(c(tt$env$p_base[i], tt$env$plabel[i]), text = fitted_r)

      } else {

        l_configure(c(tt$env$p_base[i], tt$env$plabel[i]), text = "NA")

      }
    }

  }


  df <- data_wcol(resp = resp, resp_cat = spdf@data$resp_cat,
                  cond1_cat = spdf@data$cond1_cat, cond2_cat = spdf@data$cond2_cat)

  update_colors(df)


  # R2 -----
  # Model values and R^2 label
  r2 <- r2_calc(df$resp[!duplicated(df$id)], df$group[!duplicated(df$id)])

  if ('r2label' %in% names(tt$env)) {
    tcl(tt$env$r2label, 'configure', text = paste0("R^2: ", round(r2, 2)))
  } else {
    tt$env$r2label <- tcl('label', l_subwin(tt,'r2label'), text = paste0("R^2: ", round(r2, 2)),
                          font = tkfont.create(size = size))
  }

  model_values <- attr(r2, 'model_values')

  xcoord <- spdf@bbox[1,1] - 0.15
  ycoord <- spdf@bbox[2,1]

  tt$env$plabel <- vector(length = n)

  for (i in 1:n) {

    match <- model_values[model_values$group == group[i], ]

    if (nrow(match) > 0) {

      fitted_r <- round(match[['fitted_value']][1], 2)

      tt$env$plabel[i] <- l_layer_text(tt$env$p_base[i], x = xcoord, y = ycoord,
                                       text = fitted_r,
                                       size = size, index = "end", color = "grey")

    } else {

      tt$env$plabel[i] <- l_layer_text(tt$env$p_base[i], x = xcoord, y = ycoord,
                                       text = "NA",
                                       size = size, index = "end", color = "grey")

    }

    l_scaleto_world(tt$env$p_base[i])

  }


  # Update function to be used on sliders
  updateGraph <- function() {

    respcuts <- c(convert_scale2act(respscale, as.numeric(tkcget(tt$env$scaletop, "-min")), resp),
                  convert_scale2act(respscale, as.numeric(tkcget(tt$env$scaletop, "-max")), resp))

    cond1cuts <- c(convert_scale2act(cond1scale, as.numeric(tkcget(tt$env$scaleright, "-min")), cond1),
                   convert_scale2act(cond1scale, as.numeric(tkcget(tt$env$scaleright, "-max")), cond1))

    cond2cuts <- c(convert_scale2act(cond2scale, as.numeric(tkcget(tt$env$scalebottom, "-min")), cond2),
                   convert_scale2act(cond2scale, as.numeric(tkcget(tt$env$scalebottom, "-max")), cond2))


    resp_cat <- cut_interval(resp, respcuts)
    attr(resp_cat, 'colors') <- assign_colors(resp, respcuts, seg1col, seg2col, seg3col)


    cond1_cat <- cut_interval(-cond1, -cond1cuts)
    cond1_cat <- assign_levels(cond1_cat)

    cond2_cat <- cut_interval(cond2, cond2cuts)
    cond2_cat <- assign_levels(cond2_cat)

    new_df <- data_wcol(resp = resp, resp_cat = resp_cat,
                        cond1_cat = cond1_cat, cond2_cat = cond2_cat)

    update_colors(new_df)
    update_labels(new_df)

  }


  # Reset function to be used with the Reset button. Restores to initial slider settings when
  # the code was first ran
  Reset <- function() {

    respcuts <- attr(spdf@data$resp, 'cuts')
    cond1cuts <- attr(spdf@data$cond1_cat, 'cuts')
    cond2cuts <- attr(spdf@data$cond2_cat, 'cuts')

    tcl(tt$env$scaletop, 'configure',
        min = convert_act2scale(respscale, respcuts[1], resp),
        max = convert_act2scale(respscale, respcuts[2], resp))

    tcl(tt$env$scaleright, 'configure',
        min = convert_act2scale(cond1scale, cond1cuts[1], cond1),
        max = convert_act2scale(cond1scale, cond1cuts[2], cond1))

    tcl(tt$env$scalebottom, 'configure',
        min = convert_act2scale(cond2scale, cond2cuts[1], cond2),
        max = convert_act2scale(cond2scale, cond2cuts[2], cond2))


    orig_df <- data_wcol(resp = resp, resp_cat = spdf@data$resp_cat,
                         cond1_cat = spdf@data$cond1_cat, cond2_cat = spdf@data$cond2_cat)

    update_colors(orig_df)
    update_labels(orig_df)

  }


  # Sliders and labels -----
  # Creates sliders and slider labels

  if ('labelscaletop' %in% names(tt$env)) {

    tcl(tt$env$labelscaletop, 'configure', text = ifelse(is.null(respvar.lab), respvar, respvar.lab))
    tcl(tt$env$labelscaleright, 'configure', text = ifelse(is.null(cond1var.lab), cond1var, cond1var.lab))
    tcl(tt$env$labelscalebottom, 'configure', text = ifelse(is.null(cond2var.lab), cond2var, cond2var.lab))

  } else {

    tt$env$labelscaletop <- tcl('label', l_subwin(tt,'scalelabel_top'),
                                text = ifelse(is.null(respvar.lab), respvar, respvar.lab))

    tt$env$labelscaleright <- tcl('label', l_subwin(tt,'scalelabel_right'),
                                  text = ifelse(is.null(cond1var.lab), cond1var, cond1var.lab))

    tt$env$labelscalebottom <- tcl('label', l_subwin(tt,'scalelabel_bottom'),
                                   text = ifelse(is.null(cond2var.lab), cond2var, cond2var.lab))

  }


  tt$env$scaletop <- tcl('::minmax_scale2_h', l_subwin(tt, 'scaletop'),
                         from = convert_act2scale(respscale, min(resp), resp),
                         to = convert_act2scale(respscale, max(resp), resp),
                         min = convert_act2scale(respscale, attr(spdf@data$resp_cat, 'cuts')[1], resp),
                         max = convert_act2scale(respscale, attr(spdf@data$resp_cat, 'cuts')[2], resp),
                         min_act = min(resp),
                         max_act = max(resp),
                         act = paste(resp, collapse = ' '),
                         scale = respscale,
                         resolution = 0.1,
                         seg1col = seg1col, seg2col = seg2col, seg3col = seg3col)


  tt$env$scaleright <- tcl('::minmax_scale2_v', l_subwin(tt, 'scaleright'),
                           from = convert_act2scale(cond1scale, min(cond1), cond1),
                           to = convert_act2scale(cond1scale, max(cond1), cond1),
                           min = convert_act2scale(cond1scale, attr(spdf@data$cond1_cat, 'cuts')[1], cond1),
                           max = convert_act2scale(cond1scale, attr(spdf@data$cond1_cat, 'cuts')[2], cond1),
                           min_act = min(cond1),
                           max_act = max(cond1),
                           act = paste(cond1, collapse = ' '),
                           scale = cond1scale,
                           resolution = 0.1)

  tt$env$scalebottom <- tcl('::minmax_scale2_h', l_subwin(tt, 'scalebottom'),
                            from = convert_act2scale(cond2scale, min(cond2), cond2),
                            to = convert_act2scale(cond2scale, max(cond2), cond2),
                            min = convert_act2scale(cond2scale, attr(spdf@data$cond2_cat, 'cuts')[1], cond2),
                            max = convert_act2scale(cond2scale, attr(spdf@data$cond2_cat, 'cuts')[2], cond2),
                            min_act = min(cond2),
                            max_act = max(cond2),
                            act = paste(cond2, collapse = ' '),
                            scale = cond2scale,
                            resolution = 0.1)

  tcl(tt$env$scaletop, 'configure', command = function(...) updateGraph())
  tcl(tt$env$scalebottom, 'configure', command = function(...) updateGraph())
  tcl(tt$env$scaleright, 'configure', command = function(...) updateGraph())


  # Reset button
  tt$env$resetbutton <- tkbutton(tt, command = function(...) Reset(), text = "Reset Sliders")


  # Layout -----
  tkgrid(tt$env$labelscaletop, row = 0, column = 0, sticky = "e")
  tkgrid(tt$env$scaletop, row = 0, column = 1, sticky = "new")

  for (i in 1:3) {
    for (j in 0:2) {
      tkgrid(tt$env$p_base[(i-1)*3+j+1], row = i, column = j, sticky = "nesw")
    }
  }

  tkgrid(tt$env$labelscalebottom, row = 4, column = 0, sticky = "e")
  tkgrid(tt$env$scalebottom, row = 4, column = 1, sticky = "new")
  tkgrid(tt$env$labelscaleright, row = 1, column = 3, sticky = "sew")
  tkgrid(tt$env$scaleright, row = 2, column = 3, rowspan = 2, sticky = "new")
  tkgrid(tt$env$r2label, row = 4, column = 3, sticky="nesw")
  tkgrid(tt$env$resetbutton, row = 4, column = 0)


  tkgrid.columnconfigure(tt, 3, weight = 0, minsize = 50)
  tkgrid.rowconfigure(tt, 0, weight = 0, minsize = 50)
  tkgrid.rowconfigure(tt, 4, weight = 0, minsize = 50)

  for (i in 1:3) { tkgrid.rowconfigure(tt, i, weight = 1) }
  for (j in 0:2) { tkgrid.columnconfigure(tt, j, weight = 1) }


  # Inspector -----
  ccInspector <- function(w) {

    tt_inspector <- tktoplevel()
    tktitle(tt_inspector) <- 'CCmaps Inspector'

    overall <- tkframe(tt_inspector)
    var_selector <- tkframe(overall, relief = 'groove', borderwidth = 3)
    sz <- tkframe(overall, borderwidth = 3)
    final <- tkframe(overall, borderwidth = 3)


    # Variable section
    vars <- setdiff(names(spdf@data)[sapply(spdf@data, is.numeric)],
                    c('id', 'name', 'NAME', 'group'))


    respvar_i <- tclVar(respvar)
    box.respvar <- ttkcombobox(var_selector, values = vars,
                               textvariable = respvar_i,
                               state = 'readonly')
    respvar.lab_i <- tclVar(ifelse(is.null(respvar.lab), '', respvar.lab))
    entry.respvar.lab <- tkentry(var_selector, textvariable = respvar.lab_i, width = 25)

    respvar.scale_i <- tclVar(respscale)
    respvar.scaleact <- tkradiobutton(var_selector, variable = respvar.scale_i, value = 'actual')
    respvar.scalepct <- tkradiobutton(var_selector, variable = respvar.scale_i, value = 'percent')
    respvar.scalelog <- tkradiobutton(var_selector, variable = respvar.scale_i, value = 'log')


    cond1var_i <- tclVar(cond1var)
    box.cond1var <- ttkcombobox(var_selector, values = vars,
                                textvariable = cond1var_i,
                                state = 'readonly')
    cond1var.lab_i <- tclVar(ifelse(is.null(cond1var.lab), '', cond1var.lab))
    entry.cond1var.lab <- tkentry(var_selector, textvariable = cond1var.lab_i, width = 25)

    cond1var.scale_i <- tclVar(cond1scale)
    cond1var.scaleact <- tkradiobutton(var_selector, variable = cond1var.scale_i, value = 'actual')
    cond1var.scalepct <- tkradiobutton(var_selector, variable = cond1var.scale_i, value = 'percent')
    cond1var.scalelog <- tkradiobutton(var_selector, variable = cond1var.scale_i, value = 'log')


    cond2var_i <- tclVar(cond2var)
    box.cond2var <- ttkcombobox(var_selector, values = vars,
                                textvariable = cond2var_i,
                                state = 'readonly')
    cond2var.lab_i <- tclVar(ifelse(is.null(cond2var.lab), '', cond2var.lab))
    entry.cond2var.lab <- tkentry(var_selector, textvariable = cond2var.lab_i, width = 25)

    cond2var.scale_i <- tclVar(cond2scale)
    cond2var.scaleact <- tkradiobutton(var_selector, variable = cond2var.scale_i, value = 'actual')
    cond2var.scalepct <- tkradiobutton(var_selector, variable = cond2var.scale_i, value = 'percent')
    cond2var.scalelog <- tkradiobutton(var_selector, variable = cond2var.scale_i, value = 'log')


    tkgrid(tklabel(var_selector, text = 'Variables', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 0, column = 0)

    tkgrid(tklabel(var_selector, text = 'Response:', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 0, column = 1, columnspan = 2)

    tkgrid(tklabel(var_selector, text = 'Conditioning 1 (vert):', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 0, column = 3, columnspan = 2)

    tkgrid(tklabel(var_selector, text = 'Conditioning 2 (hor):', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 0, column = 5, columnspan = 2)


    tkgrid(tklabel(var_selector, text = 'Names: ', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 1, column = 0)

    tkgrid(box.respvar, sticky = 'w', padx = 5, pady = 5, row = 1, column = 1, columnspan = 2)

    tkgrid(box.cond1var, sticky = 'w', padx = 5, pady = 5, row = 1, column = 3, columnspan = 2)

    tkgrid(box.cond2var, sticky = 'w', padx = 5, pady = 5, row = 1, column = 5, columnspan = 2)


    tkgrid(tklabel(var_selector, text = 'Labels: ', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 2, column = 0)

    tkgrid(entry.respvar.lab, sticky = 'w', padx = 5, pady = 5, row = 2, column = 1, columnspan = 2)

    tkgrid(entry.cond1var.lab, sticky = 'w', padx = 5, pady = 5, row = 2, column = 3, columnspan = 2)

    tkgrid(entry.cond2var.lab, sticky = 'w', padx = 5, pady = 5, row = 2, column = 5, columnspan = 2)


    tkgrid(tklabel(var_selector, text = 'Scales: ', anchor = 'w'),
           tklabel(var_selector, text = 'Actual', anchor = 'w'), respvar.scaleact,
           tklabel(var_selector, text = 'Actual', anchor = 'w'), cond1var.scaleact,
           tklabel(var_selector, text = 'Actual', anchor = 'w'), cond2var.scaleact,
           sticky = 'w', padx = 5, pady = 5, row = 3)

    tkgrid(tklabel(var_selector, text = '', anchor = 'w'),
           tklabel(var_selector, text = 'Percent', anchor = 'w'), respvar.scalepct,
           tklabel(var_selector, text = 'Percent', anchor = 'w'), cond1var.scalepct,
           tklabel(var_selector, text = 'Percent', anchor = 'w'), cond2var.scalepct,
           sticky = 'w', padx = 5, pady = 5, row = 4)

    tkgrid(tklabel(var_selector, text = '', anchor = 'w'),
           tklabel(var_selector, text = 'Log', anchor = 'w'), respvar.scalelog,
           tklabel(var_selector, text = 'Log', anchor = 'w'), cond1var.scalelog,
           tklabel(var_selector, text = 'Log', anchor = 'w'), cond2var.scalelog,
           sticky = 'w', padx = 5, pady = 5, row = 5)


    # Font size
    currSize <- tclVar(as.character(size))
    size_disp <- tklabel(sz, textvariable = currSize)

    minus <- tkbutton(sz, text = '-', command = function() downsize())
    plus <- tkbutton(sz, text = '+', command = function() upsize())


    tkgrid(tklabel(sz, text = 'Fontsize: ', anchor = 'w'),
           minus, plus, size_disp)


    # Submit button
    opt <- tkcheckbutton(final)
    optValue <- tclVar(as.character(as.numeric(optimize)))
    tkconfigure(opt, variable = optValue)


    submit <- tkbutton(final, text = 'Submit', command = function() updatecc())


    tkgrid(tklabel(final, text = 'Optimize R^2 '), opt, submit,
           sticky = 'nse', padx = 5, pady = 5)


    tkgrid(var_selector, sticky = 'w')
    tkgrid(sz, sticky = 'w')
    tkgrid(final, sticky = 'e')
    tkgrid(overall)



    upsize <- function() {

      newsize <- as.numeric(tclvalue(currSize)) + 1
      tclvalue(currSize) <- as.character(newsize)

      tcl(tt$env$r2label, 'configure', font = tkfont.create(size = newsize))
      lapply(1:length(tt$env$plabel), function(i) l_configure(c(tt$env$p_base[i], tt$env$plabel[i]), size = newsize))

    }


    downsize <- function() {

      s <- as.numeric(tclvalue(currSize))

      if (s == 1) {
        newsize <- 1
      } else {
        newsize <- s - 1
      }

      tclvalue(currSize) <- as.character(newsize)


      tcl(tt$env$r2label, 'configure', font = tkfont.create(size = newsize))
      lapply(1:length(tt$env$plabel), function(i) l_configure(c(tt$env$p_base[i], tt$env$plabel[i]), size = newsize))

    }


    updatecc <- function() {

      # Disable any clicks until the code finishes running
      # (since optimization takes a long time to run)
      tcl(submit, 'configure', state = 'disabled')
      tcl('tk', 'busy', tt_inspector)


      respscale_new <- tclvalue(respvar.scale_i)
      cond1scale_new <- tclvalue(cond1var.scale_i)
      cond2scale_new <- tclvalue(cond2var.scale_i)


      if (tclvalue(respvar_i) == tt$env$respvar) {

        respvar_new <- tt$env$respvar
        resp_new <- spdf@data[[respvar_new]]

        respbreaks_new <- c(convert_scale2act(tt$env$respscale, as.numeric(tkcget(tt$env$scaletop, "-min")), resp_new),
                            convert_scale2act(tt$env$respscale, as.numeric(tkcget(tt$env$scaletop, "-max")), resp_new))


      } else {

        respvar_new <- tclvalue(respvar_i)
        respbreaks_new <- 2

        tt$env$respvar <- respvar_new

      }


      if (identical(tclvalue(respvar.lab_i), '')) {
        respvar.lab_new <- NULL
      } else {
        respvar.lab_new <- tclvalue(respvar.lab_i)
      }


      if (tclvalue(cond1var_i) == tt$env$cond1var) {

        cond1var_new <- tt$env$cond1var
        cond1_new <- spdf@data[[cond1var_new]]

        cond1breaks_new <- c(convert_scale2act(tt$env$cond1scale, as.numeric(tkcget(tt$env$scaleright, '-min')), cond1_new),
                             convert_scale2act(tt$env$cond1scale, as.numeric(tkcget(tt$env$scaleright, '-max')), cond1_new))


      } else {

        cond1var_new <- tclvalue(cond1var_i)
        cond1breaks_new <- 2

        tt$env$cond1var <- cond1var_new

      }


      if (identical(tclvalue(cond1var.lab_i), '')) {
        cond1var.lab_new <- NULL
      } else {
        cond1var.lab_new <- tclvalue(cond1var.lab_i)
      }


      if (tclvalue(cond2var_i) == tt$env$cond2var) {

        cond2var_new <- tt$env$cond2var
        cond2_new <- spdf@data[[cond2var_new]]

        cond2breaks_new <- c(convert_scale2act(tt$env$cond2scale, as.numeric(tkcget(tt$env$scalebottom, '-min')), cond2_new),
                             convert_scale2act(tt$env$cond2scale, as.numeric(tkcget(tt$env$scalebottom, '-max')), cond2_new))


      } else {

        cond2var_new <- tclvalue(cond2var_i)
        cond2breaks_new <- 2

        tt$env$cond2var <- cond2var_new

      }

      if (identical(tclvalue(cond2var.lab_i), '')) {
        cond2var.lab_new <- NULL
      } else {
        cond2var.lab_new <- tclvalue(cond2var.lab_i)
      }



      optimize_new <- as.character(tclvalue(optValue))
      optimize_new <- ifelse(optimize_new == '1', TRUE, FALSE)

      size_new <- as.numeric(tclvalue(currSize))


      tryCatch({

        l_ccmaps(tt = w, cc_inspector = FALSE,
                 spdf = spdf,
                 respvar = respvar_new, respvar.lab = respvar.lab_new,
                 cond1var = cond1var_new, cond1var.lab = cond1var.lab_new,
                 cond2var = cond2var_new, cond2var.lab = cond2var.lab_new,
                 respbreaks = respbreaks_new,
                 cond1breaks = cond1breaks_new, cond2breaks = cond2breaks_new,
                 respscale = respscale_new,
                 cond1scale = cond1scale_new, cond2scale = cond2scale_new,
                 size = size_new, seg1col = seg1col, seg2col = seg2col, seg3col = seg3col,
                 optimize = optimize_new, otry = otry,
                 title = title)

      }, error = function(err) {

        print(paste0('CCmaps update ran into the following error: ', err))

      })


      tcl('tk', 'busy', 'forget', tt_inspector)
      tcl(submit, 'configure', state = 'normal')

    }

    # Closes inspector window if the CCmaps display window is closed
    tkbind(w, '<Destroy>', function() tkdestroy(tt_inspector))

    # Do not allow inspector window to close otherwise
    tcl("wm", "protocol", tt_inspector, "WM_DELETE_WINDOW",
        quote(cat('To close inspector, close the main display window\n')))

    tt_inspector

  }

  if (cc_inspector) ccInspector(tt)


  ret <- list(top = tt,
              respvar = respvar, cond1var = cond1var, cond2var = cond2var,
              maps = list(base = tt$env$p_base, polygons = tt$env$p))

  attr(ret, 'class') <- c('l_ccmaps', 'l_compound', 'loon')

  return(invisible(ret))

}


## Helper Functions -----

# Partitions x (numeric vector) into up to intervals, with break points
# defined by cuts. Break points need not be unique
cut_interval <- function(x, cuts) {

  cut(x,
      breaks = unique(c(min(x), cuts, max(x))),
      include.lowest = TRUE)

}


# Renames the levels of a vector of factors levels to numbers
assign_levels <- function(x) {

  if (is.factor(x)) x <- droplevels(x)

  factor(x, labels = 1:nlevels(x))

}


# Determines the colors to use for polygons on maps. Needed since there might
# not be three categories for the study variable (e.g. when the first and
# second break point are equal)
assign_colors <- function(var, cuts, seg1col, seg2col, seg3col) {

  min <- min(var)
  max <- max(var)
  cut1 <- cuts[1]
  cut2 <- cuts[2]

  if (length(unique(c(min, max, cut1, cut2))) == 4) {

    c(seg1col, seg2col, seg3col)

  } else if (length(unique(c(min, max, cut1, cut2))) == 3) {

    if (min == cut1) c(seg2col, seg3col)
    else if (max == cut2) c(seg1col, seg2col)
    else if (cut1 == cut2) c(seg1col, seg3col)

  } else {

    seg2col

  }

}


# Calculates R^2 given the study variable values and the panel assignment
# based on the conditioning variables
r2_calc <- function(x, group) {

  overall_mean <- mean(x)

  fitted_df <- aggregate(x, by = list(group), mean)
  colnames(fitted_df) <- c("group", "fitted_value")

  act_df <- data.frame(act_value = x, group = group, stringsAsFactors = F)

  model_values <- merge(fitted_df, act_df, by = "group")
  model_values$group <- as.character(model_values$group)

  r2 <- sum((model_values$fitted_value - overall_mean)^2)/sum((model_values$act_value - overall_mean)^2)

  attr(r2, 'model_values') <- model_values

  r2

}


# Computes the R^2 measures by trying a number of possible partitioning points
# on the two conditioning variables, and returns the results in a data.frame
# sorted in decreasing order by R^2
r2_optimize <- function(otry, resp, cond1, cond2) {

  # otry number of values to try for each conditioning variable. Picks {otry choose 2} pairs (since
  # order doesn't matter) of points as the middle two break points for partioning, as well as
  # cases where the two middle break points are equal
  cond1_seq <- quantile(cond1, probs = seq(0, 1, length.out = otry)) %>% unique()

  cond1_combn <- cbind(combn(cond1_seq, m = 2),
                       matrix(rep(cond1_seq, times = 2), nrow = 2, byrow = T))

  cond1_combn_str <- apply(cond1_combn, 2, function(x) paste0(x, collapse = ', '))


  cond2_seq <- quantile(cond2, probs = seq(0, 1, length.out = otry)) %>% unique()

  cond2_combn <- cbind(combn(cond2_seq, 2),
                       matrix(rep(cond2_seq, times = 2), nrow = 2, byrow = T))

  cond2_combn_str <- apply(cond2_combn, 2, function(x) paste0(x, collapse = ', '))

  r2_df <- expand.grid(cond1_combn_str, cond2_combn_str, stringsAsFactors = F) %>%
    rowwise() %>%
    transmute(cond1cut1 = unlist(strsplit(Var1, ', '))[1],
              cond1cut2 = unlist(strsplit(Var1, ', '))[2],
              cond2cut1 = unlist(strsplit(Var2, ', '))[1],
              cond2cut2 = unlist(strsplit(Var2, ', '))[2]) %>%
    mutate_all(as.numeric)

  r2s <- Map(function(resp, cond1, cond2, cond1cut1, cond1cut2, cond2cut1, cond2cut2) {

    cond1_cat <- cut_interval(-cond1, -c(cond1cut1, cond1cut2))
    cond1_cat <- assign_levels(cond1_cat)

    cond2_cat <- cut_interval(cond2, c(cond2cut1, cond2cut2))
    cond2_cat <- assign_levels(cond2_cat)


    gr <- paste0(cond1_cat, cond2_cat)

    r2_calc(resp, gr)

  }, list(resp), list(cond1), list(cond2),
  r2_df$cond1cut1, r2_df$cond1cut2, r2_df$cond2cut1, r2_df$cond2cut2)

  r2_df$r2 <- unlist(r2s)

  r2_df %>%
    sample_n(., nrow(r2_df)) %>%
    arrange(desc(r2)) %>%
    data.frame()

}


# Change from actual/raw numbers to percentile or log scale numbers
convert_act2scale <- function(scale = c('actual', 'percent', 'log'), val, vals = NULL) {

  if (scale == 'actual') {

    val

  } else if (scale == 'percent') {

    Fn <- ecdf(vals)
    Fn(val) * 100

  } else if (scale == 'log') {

    if (any(vals <= 0)) stop('Cannot use log scale for variables containing zero or negative values')

    log(val)

  }

}


# Change from percentile or log scale numbers to actual/raw numbers
convert_scale2act <- function(scale = c('actual', 'percent', 'log'), val, vals = NULL) {

  if (scale == 'actual') {

    val

  } else if (scale == 'percent') {

    quantile(vals, probs = val/100)

  } else if (scale == 'log') {

    exp(val)

  }

}

