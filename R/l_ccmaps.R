
## Modified MinMaxSlider -----
# Note that one \ is escaped with \\
# Definition of modified MinMaxSlider (2 handles)
.Tcl(
  '
  oo::class create ::loon::classes::MinMaxScale2 {

  superclass ::loon::classes::Inspector2

  variable path canvas from to min max resolution orient seg1col seg2col seg3col\\
  b_w b_e b_s b_n slider_width slider_width2 slider_peak pad_text\\
  current_slider n_pix_per_res n_pix_per_res2\\
  mouse_x mouse_y

  constructor {Path} {

  set mouse_x 0

  set current_slider ""
  set n_pix_per_res 1
  set n_pix_per_res2 0.5

  set b_w 35
  set b_e 35
  set b_s 35
  set b_n 35
  set slider_peak 5
  set slider_width 12
  set slider_width2 6
  set pad_text 3

  next $Path

  my New_state from double 1 0
  my New_state to double 1 1
  my New_state min double 1 0
  my New_state max double 1 1
  my New_state resolution positive_double 1 0.01
  my New_state orient string 1 "horizontal"
  #my New_state showvalues orient boolean 1 TRUE
  my New_state command string 1 ""
  my New_state seg1col colorOrTransparent 1 "darkgrey"
  my New_state seg2col colorOrTransparent 1 "darkgrey"
  my New_state seg3col colorOrTransparent 1 "darkgrey"

  my SetStateDescription from\\
  "from value of scale"
  my SetStateDescription to\\
  "to value of scale"
  my SetStateDescription min\\
  "position of min slider"
  my SetStateDescription max\\
  "position of max slider"
  my SetStateDescription resolution\\
  "resolution for the scale"
  my SetStateDescription orient\\
  "orientation for the scale"
  my SetStateDescription seg1col\\
  "color of the first segment of the scale"
  my SetStateDescription orient\\
  "color of the second segment of the scale"
  my SetStateDescription orient\\
  "color of the third segment of the scale"
  my SetStateDescription command\\
  "callback that is evaluated with any state change"

  }

  method EvalConfigure {} {
  my variable confDict

  next

  ## check that from <= min <= max <= to
  set hasValue FALSE
  foreach val {min max from to} {
  if {[dict exists $confDict arg_$val]} {
  set hasValue TRUE
  set val_$val [dict get $confDict arg_$val]
  } else {
  set val_$val [set $val]
  }
  }

  if {$hasValue} {

  if {$val_from > $val_min} {
  error "min > from: $val_min > $val_from"
  }
  if {$val_min > $val_max} {
  error "min > max: $val_min > $val_max"
  }
  if {$val_max > $val_to} {
  error "max > to: $val_max > $val_to"
  }
  }

  }
  method EvalCommand {} {
  my variable command path
  if {$command ne ""} {
  uplevel #0 [string map [list %W $path %min $min %max $max] $command]
  }

  }

  method Make {} {
  frame $path -class LoonMinMaxScale
  set canvas [canvas ${path}.canvas]

  pack $canvas -fill both -expand TRUE -side top

  bind $canvas <Configure> "[self namespace]::my Redraw"
  $canvas bind "min||max||drag1||drag2" <ButtonPress-1>\\
  "[self namespace]::my SelectSlider %x %y"
  bind $canvas <ButtonRelease>\\
  "[self namespace]::my ReleaseSlider"
  bind $canvas <Button1-Motion>\\
  "[self namespace]::my DragSlider %x %y"
  }

  method HookAfterStatesSet {} {
  my variable changedStates
  if {[llength $changedStates] > 0} {
  my Redraw
  }
  if {"min" in $changedStates || "max" in $changedStates} {
  my EvalCommand
  }

  }

  method Redraw {} {

  set w [winfo width $canvas]
  set h [winfo height $canvas]

  $canvas delete all
  set x0 $b_w
  set x1 [expr {$w - $b_e}]

  set y0 $b_n
  set y1 [expr {$h - $b_s}]

  $canvas create rect $x0 $y0 $x1 $y1 -fill $seg2col

  if {$orient eq "horizontal"} {

  set x_min [expr {$b_e + $slider_width}]
  set x_max [expr {$w - $b_w - $slider_width}]
  set dx [expr {double($x_max - $x_min)}]

  ## Location of the min and slider
  set loc_min [expr {$x_min + ($min-$from)/($to - $from)*$dx}]
  set loc_max [expr {$x_min + ($max-$from)/($to - $from)*$dx}]

  $canvas create rect\\
  $x0 $y0 $loc_min $y1\\
  -fill $seg1col -tag drag1

  $canvas create rect\\
  $loc_max $y0 $x1 $y1\\
  -fill $seg3col -tag drag2

  $canvas create rect\\
  [expr {$loc_min - $slider_width}] $y0 $loc_min $y1\\
  -fill white -tag min

  $canvas create text\\
  [expr {$loc_min - $slider_width2}] [expr {$y1 + $slider_peak + $pad_text}]\\
  -text [format "%.3g" $min] -anchor n -justify center

  $canvas create rect\\
  $loc_max $y0 [expr {$loc_max + $slider_width}] $y1\\
  -fill white -tag max

  $canvas create text\\
  [expr {$loc_max + $slider_width2}] [expr {$y0 - $slider_peak - $pad_text}]\\
  -text [format "%.3g" $max] -anchor s -justify center

  set n_pix_per_res [expr {$dx/($to - $from)*$resolution}]
  set n_pix_per_res2 [expr {$n_pix_per_res/2.0}]

  } elseif {$orient eq "vertical"} {
  set y_min [expr {$h - $b_s - $slider_width}]
  set y_max [expr {$b_n + $slider_width}]
  set dy [expr {double($y_min - $y_max)}]

  ## Location of the min and slider
  set loc_max [expr {$y_min - ($max-$from)/($to - $from)*$dy}]
  set loc_min [expr {$y_min - ($min-$from)/($to - $from)*$dy}]

  $canvas create rect\\
  $x0 $loc_min $x1 $y1\\
  -fill $seg1col -tag drag1

  $canvas create rect\\
  $x0 $y0 $x1 $loc_max\\
  -fill $seg3col -tag drag2

  $canvas create rect\\
  $x0 $loc_min $x1 [expr {$loc_min + $slider_width}]\\
  -fill white -tag min

  $canvas create text\\
  [expr {$x0 - 6*$pad_text}] $loc_min\\
  -text [format "%.3g" $min] -anchor n -justify center

  $canvas create rect\\
  $x0 $loc_max $x1 [expr {$loc_max - $slider_width}]\\
  -fill white -tag max

  $canvas create text\\
  [expr {$x1 + 6*$pad_text}] $loc_max\\
  -text [format "%.3g" $max] -anchor s -justify center

  set n_pix_per_res [expr {$dy/($to - $from)*$resolution}]
  set n_pix_per_res2 [expr {$n_pix_per_res/2.0}]
  }
  }

  method SelectSlider {x y} {
  if {$orient eq "horizontal"} {
  set mouse_x $x
  set tag [lindex [$canvas gettags current] 0]
  switch -- $tag {
  min {
  set current_slider "min"
  }
  max {
  set current_slider "max"
  }
  drag1 {
  set current_slider "drag1"
  }
  drag2 {
  set current_slider "drag2"
  }
  default {
  set current_slider ""
  }
  }
  } else {
  set mouse_y $y
  set tag [lindex [$canvas gettags current] 0]
  switch -- $tag {
  min {
  set current_slider "min"
  }
  max {
  set current_slider "max"
  }
  drag1 {
  set current_slider "drag1"
  }
  drag2 {
  set current_slider "drag2"
  }
  default {
  set current_slider ""
  }
  }
  }
  }
  method ReleaseSlider {} {
  set current_slider ""
  }

  method DragSlider {x y} {
  set w [winfo width $canvas]
  set h [winfo height $canvas]

  if {$orient eq "horizontal"} {
  switch -- $current_slider {
  min {
  set x_loc [expr {$x - $b_e - $slider_width + $slider_width2 + $n_pix_per_res2}]
  set mul [expr {int($x_loc / $n_pix_per_res )}]
  set x_min [expr {$from + $mul*$resolution}]

  if {$x_min < $from} {
  set x_min $from
  } elseif {$x_min > $max} {
  set x_min $max
  }
  my configure -min $x_min
  }
  max {
  set x_loc [expr {$x - $b_e - $slider_width - $slider_width2 + $n_pix_per_res2}]
  set mul [expr {int($x_loc / $n_pix_per_res )}]
  set x_max [expr {$from + $mul*$resolution}]

  if {$x_max > $to} {
  set x_max $to
  } elseif {$x_max < $min} {
  set x_max $min
  }
  my configure -max $x_max
  }
  drag1 {

  set m_dx [expr {$x - $mouse_x}]

  set x_change [expr {$m_dx/double($n_pix_per_res)*$resolution}]

  set x_min [expr {$min + $x_change}]

  if {$x_max < $x_min} {
  set x_change [expr {$max - $min}]
  } elseif {$x_min < $from} {
  set x_change [expr {$from - $min}]
  }

  my configure -min [expr {$min + $x_change}]

  }
  drag2 {

  set m_dx [expr {$x - $mouse_x}]

  set x_change [expr {$m_dx/double($n_pix_per_res)*$resolution}]

  set x_max [expr {$max + $x_change}]

  if {$x_max > $to} {
  set x_change [expr {$to - $max}]
  } elseif {$x_min > $x_max} {
  set x_change [expr {$min - $max}]
  }

  my configure -max [expr {$max + $x_change}]

  }
  }
  set mouse_x $x
  } elseif {$orient eq "vertical"} {
  switch -- $current_slider {
  min {
  set y_loc [expr {$h - $y - $b_s - $slider_width + $slider_width2 + $n_pix_per_res2}]
  set mul [expr {int($y_loc / $n_pix_per_res )}]
  set y_min [expr {$from + $mul*$resolution}]

  if {$y_min < $from} {
  set y_min $from
  } elseif {$y_min > $max} {
  set y_min $max
  }
  my configure -min $y_min
  }
  max {
  set y_loc [expr {$h - $y - $b_s - $slider_width - $slider_width2 + $n_pix_per_res2}]
  set mul [expr {int($y_loc / $n_pix_per_res )}]
  set y_max [expr {$from + $mul*$resolution}]

  if {$y_max > $to} {
  set y_max $to
  } elseif {$y_max < $min} {
  set y_max $min
  }
  my configure -max $y_max
  }
  drag1 {

  set m_dy [expr {$y - $mouse_y}]

  set y_change [expr {$m_dy/double($n_pix_per_res)*$resolution}]

  set y_min [expr {$min - $y_change}]

  if {$y_min < $from} {
  set y_change [expr {$min - $from}]
  } elseif {$y_min > $y_max} {
  set y_change [expr {$min - $max}]
  }

  my configure -min [expr {$min - $y_change}]
  }
  drag2 {

  set m_dy [expr {$y - $mouse_y}]

  set y_change [expr {$m_dy/double($n_pix_per_res)*$resolution}]

  set y_max [expr {$max - $y_change}]

  if {$y_max > $to} {
  set y_change [expr {$max - $to}]
  } elseif {$y_max < $y_min} {
  set y_change [expr {$max - $min}]
  }

  my configure -max [expr {$max - $y_change}]
  }
  }
  set mouse_y $y
  }

  }

  }
  '
)


# Creater function
.Tcl(
  "
  proc minmax_scale2 {args} {
  return [WidgetFactory MinMaxScale2 minmaxscale2 {*}$args]
  }
  "
)


## Main function -----

#' @title Conditioned choropleth maps
#'
#' @description 2-way panel of maps for visualizing multivariate data analysis
#'
#' @param data SpatialPolygonsDataFrame to hold polygon coordinates and attributes,
#'   including values for variables used for determining map coloring and panel positioning
#' @param respvar character name of the response value variable
#' @param cond1var character name of the first conditioning variable (controls
#'   panel assignment in the vertical direction)
#' @param cond2var character name of the second conditioning variable (controls
#'   panel assignment in the horizontal direction)
#' @param respbreaks determines how the response data is divided into three groups.
#'   Can either be the integer 3 or a numeric vector of four break points.
#'   Defaults to 3, in which case the response values are divided into tertiles
#' @param optimize logical value indicating whether panel assignment should be optimized
#'   for \eqn{R^2}. Defaults to FALSE, in which case the conditioning data is
#'   divided into tertiles
#' @param otry integer (greater than 0) indicating number of values to try
#'   for optimization (see above). Required if \code{optimize = TRUE}.
#'   Defaults to 10. A higher \code{otry} value leads to more precise estimates
#'   at the cost of longer computation time
#' @param title character string of the title of the map. Appears in the title bar
#'   of the plot window. Defaults to "CCmaps"
#'
#' @importFrom dplyr mutate mutate_at rowwise funs
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' ## Example 1
#' ## Get data
#' library(maptools)
#' library(rgdal)
#' columbus <- readOGR(system.file("shapes/columbus.shp", package = "maptools")[1], verbose = F)
#'
#' ## Plot
#' l_ccmaps(spdf = columbus,
#'          respvar = "CRIME", cond1var = "PLUMB", cond2var = "HOVAL",
#'          optimize = TRUE,
#'          title = "Columbus Residential Burglaries and Vehicle Thefts")
#'
#'
#' ## Example 2
#' ## Get data
#' library(maptools)
#' library(rgdal)
#' nc.sids <- readOGR(system.file("shapes/sids.shp", package = "maptools")[1], verbose = F)
#'
#' proj4string(nc.sids) <- CRS("+proj=longlat +ellps=clrk66")
#' row.names(nc.sids) <- as.character(nc.sids$FIPSNO)
#'
#' nc.sids$ft.SID74 <- sqrt(1000)*(sqrt(nc.sids$SID74/nc.sids$BIR74) + sqrt((nc.sids$SID74+1)/nc.sids$BIR74))
#'
#' nc.sids$ft.NWBIR74 <- sqrt(1000)*(sqrt(nc.sids$NWBIR74/nc.sids$BIR74) + sqrt((nc.sids$NWBIR74+1)/nc.sids$BIR74))
#'
#' ## Plot
#' l_ccmaps(spdf = nc.sids,
#'          respvar = "SID79", cond1var = "BIR79", cond2var = "SID74",
#'          optimize = FALSE,
#'          title = "North Carolina SIDS Rates")
#'
#'}
#'
l_ccmaps <- function(tt = tktoplevel(), var_inspector = TRUE,
                     spdf,
                     respvar, respvar.lab = NULL,
                     cond1var, cond1var.lab = NULL,
                     cond2var, cond2var.lab = NULL,
                     respbreaks = 3,
                     seg1col = 'blue', seg2col = 'darkgrey', seg3col = 'red',
                     optimize = FALSE, otry = 10,
                     title = "CCmaps") {


  # Input checks -----
  if (!is(tt, 'tkwin'))
    stop('tt should be a Tk toplevel window generated by tktoplevel()')

  if (!is.logical(var_inspector)) {
    stop("var_inspector must be TRUE or FALSE only")
  }

  if (!is(spdf, "SpatialPolygonsDataFrame")) {
    stop("spdf has to be of SpatialPolygonsDataFrame class")}


  if (length(unique(spdf@plotOrder)) != nrow(spdf@data)) {
    stop('plotOrder for spdf does not correspond to the number of data points')
  }


  var_len <- sapply(c('respvar', 'cond1var', 'cond2var', 'var_inspector',
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

  respbreaks_errmsg <- "respbreaks must be a numeric vector of four break points or the integer 3"

  if (!is.numeric(respbreaks)) {
    stop(respbreaks_errmsg)
  }

  if (length(respbreaks) == 1) {
    if (respbreaks != 3) stop(respbreaks_errmsg)
  } else if (length(respbreaks) != 4) {
    stop(respbreaks_errmsg)
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



  resp <- spdf@data[, respvar]
  cond1 <- spdf@data[, cond1var]
  cond2 <- spdf@data[, cond2var]

  if (!is.numeric(resp)) {
    stop("Study variable must be numeric")
  }

  if (!is.numeric(cond1) | !is.numeric(cond2)) {
    stop("Conditioning variables must be numeric")
  }


  # Setting up plots -----
  tktitle(tt) <- title


  p_base <- vector(length = n)
  p <- vector("list",length = n)

  for (i in 1:n) {

    p_base[i] <- l_plot(parent = tt)

    p[[i]] <- l_layer(p_base[i], spdf,
                      color = "cornsilk", asSingleLayer = TRUE)

    l_scaleto_world(p_base[i])

  }


  # Determine panel membership of polygons and colors -----

  # If break points not given, use default of equal quantiles. Otherwise, use user-defined
  # break points
  if (respbreaks == 3) {
    respcuts <- quantile(resp, probs=c(1/3, 2/3))
  } else {
    respcuts <- c(respbreaks[2], respbreaks[3])
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

    cond1cuts <- quantile(cond1, probs = c(1/3, 2/3))
    cond2cuts <- quantile(cond2, probs = c(1/3, 2/3))

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

      l_configure(c(p_base[k], p[[k]]), color = data[[colname]])

    }
  }


  update_labels <- function(data) {

    data <- data[!duplicated(data$id), ]

    r2 <- r2_calc(data$resp, data$group)
    tcl(r2label, 'configure', text = paste0("R^2: ", round(r2, 2)))


    model_values <- attr(r2, 'model_values')


    for (i in 1:n) {

      match <- model_values[model_values$group == group[i], ]

      if (nrow(match) == 1) {

        fitted_r <- round(match[['fitted_value']], 2)

        l_configure(c(p_base[i], plabel[i]), text = fitted_r)

      } else {

        l_configure(c(p_base[i], plabel[i]), text = "NA")

      }
    }

  }


  df <- data_wcol(resp = resp, resp_cat = spdf@data$resp_cat,
                  cond1_cat = spdf@data$cond1_cat, cond2_cat = spdf@data$cond2_cat)

  update_colors(df)


  # R2 -----
  # Model values and R^2 label
  r2 <- r2_calc(df$resp[!duplicated(df$id)], df$group[!duplicated(df$id)])
  r2label <- tcl('label', l_subwin(tt,'r2label'), text = paste0("R^2: ", round(r2, 2)))

  model_values <- attr(r2, 'model_values')


  xcoord <- spdf@bbox[1,1] - 0.15
  ycoord <- spdf@bbox[2,1]

  plabel <- vector(length = n)

  for (i in 1:n) {

    match <- model_values[model_values$group == group[i], ]

    if (nrow(match) == 1) {

      fitted_r <- round(match[['fitted_value']], 2)

      plabel[i] <- l_layer_text(p_base[i], x = xcoord, y = ycoord,
                                text = fitted_r,
                                size = 10, index = "end", color = "grey")

    } else {

      plabel[i] <- l_layer_text(p_base[i], x = xcoord, y = ycoord,
                                text = "NA",
                                size = 10, index = "end", color = "grey")

    }

    l_scaleto_world(p_base[i])

  }


  # Update function to be used on sliders
  updateGraph <- function() {

    respcuts <- c(as.numeric(tkcget(scaletop, "-min")),
                  as.numeric(tkcget(scaletop, "-max")))

    cond1cuts <- c(as.numeric(tkcget(scaleright, "-min")),
                   as.numeric(tkcget(scaleright, "-max")))

    cond2cuts <- c(as.numeric(tkcget(scalebottom, "-min")),
                   as.numeric(tkcget(scalebottom, "-max")))

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

    tcl(scaletop, 'configure', min = respcuts[1], max = respcuts[2])
    tcl(scaleright, 'configure', min = cond1cuts[1], max = cond1cuts[2])
    tcl(scalebottom, 'configure', min = cond2cuts[1], max = cond2cuts[2])


    orig_df <- data_wcol(resp = resp, resp_cat = spdf@data$resp_cat,
                         cond1_cat = spdf@data$cond1_cat, cond2_cat = spdf@data$cond2_cat)

    update_colors(orig_df)
    update_labels(orig_df)

  }


  # Sliders and labels -----
  # Creates sliders and slider labels
  labelscaletop <- tcl('label', l_subwin(tt,'scalelabel_top'),
                       text = ifelse(is.null(respvar.lab), respvar, respvar.lab
                       ))
  scaletop <- tcl('::minmax_scale2', l_subwin(tt, 'scaletop'),
                  from = min(resp), to = max(resp),
                  min = attr(spdf@data$resp_cat, 'cuts')[1],
                  max = attr(spdf@data$resp_cat, 'cuts')[2],
                  resolution = 0.1, orient = 'horizontal',
                  seg1col = seg1col, seg2col = seg2col, seg3col = seg3col)

  labelscaleright <- tcl('label', l_subwin(tt,'scalelabel_right'),
                         text = ifelse(is.null(cond1var.lab), cond1var, cond1var.lab))

  scaleright <- tcl('::minmax_scale2', l_subwin(tt, 'scaleright'),
                    from = min(cond1), to = max(cond1),
                    min = attr(spdf@data$cond1_cat, 'cuts')[1],
                    max = attr(spdf@data$cond1_cat, 'cuts')[2],
                    resolution = 0.1, orient="vertical")

  labelscalebottom <- tcl('label',l_subwin(tt,'scalelabel_bottom'),
                          text = ifelse(is.null(cond2var.lab), cond2var, cond2var.lab))

  scalebottom <- tcl('::minmax_scale2', l_subwin(tt, 'scalebottom'),
                     from = min(cond2), to = max(cond2),
                     min = attr(spdf@data$cond2_cat, 'cuts')[1],
                     max = attr(spdf@data$cond2_cat, 'cuts')[2],
                     resolution = 0.1, orient = "horizontal")

  tcl(scaletop, 'configure', command = function(...) updateGraph())
  tcl(scalebottom, 'configure', command = function(...) updateGraph())
  tcl(scaleright, 'configure', command = function(...) updateGraph())


  # Reset button
  resetbutton <- tkbutton(tt, command = function(...) Reset(), text = "Reset Sliders")


  # Layout -----
  tkgrid(labelscaletop, row = 0, column = 0, sticky = "e")
  tkgrid(scaletop, row = 0, column = 1, sticky = "new")

  for (i in 1:3) {
    for (j in 0:2) {
      tkgrid(p_base[(i-1)*3+j+1], row = i, column = j, sticky = "nesw")
    }
  }

  tkgrid(labelscalebottom, row = 4, column = 0, sticky = "e")
  tkgrid(scalebottom, row = 4, column = 1, sticky = "new")
  tkgrid(labelscaleright, row = 1, column = 3, sticky = "sew")
  tkgrid(scaleright, row = 2, column = 3, rowspan = 2, sticky = "new")
  tkgrid(r2label, row = 4, column = 3, sticky="nesw")
  tkgrid(resetbutton, row = 4, column = 0)

  tkgrid.columnconfigure(tt, 3, weight = 6)
  tkgrid.rowconfigure(tt, 0, weight = 4)
  tkgrid.rowconfigure(tt, 4, weight = 4)

  for (i in 1:3) { tkgrid.rowconfigure(tt, i, weight = 5) }
  for (j in 0:2) { tkgrid.columnconfigure(tt, j, weight = 5) }


  # Inspector -----
  ccInspector <- function(w) {

    tt_inspector <- tktoplevel()
    tktitle(tt_inspector) <- 'CCmaps Variable Selector'

    overall <- tkframe(tt_inspector)
    var_selector <- tkframe(overall, relief = 'groove', borderwidth = 3)
    final <- tkframe(overall, borderwidth = 3)


    # Grouping section
    vars <- setdiff(names(spdf@data)[sapply(spdf@data, is.numeric)],
                    c('id', 'name', 'NAME', 'group'))


    respvar_i <- tclVar(respvar)
    box.respvarvar <- ttkcombobox(var_selector, values = vars,
                                  textvariable = respvar_i,
                                  state = 'readonly')
    respvar.lab_i <- tclVar(ifelse(is.null(respvar.lab), '', respvar.lab))
    entry.respvar.lab <- tkentry(var_selector, textvariable = respvar.lab_i, width = 20)


    cond1var_i <- tclVar(cond1var)
    box.cond1var <- ttkcombobox(var_selector, values = vars,
                                textvariable = cond1var_i,
                                state = 'readonly')
    cond1var.lab_i <- tclVar(ifelse(is.null(cond1var.lab), '', cond1var.lab))
    entry.cond1var.lab <- tkentry(var_selector, textvariable = cond1var.lab_i, width = 20)


    cond2var_i <- tclVar(cond2var)
    box.cond2var <- ttkcombobox(var_selector, values = vars,
                                textvariable = cond2var_i,
                                state = 'readonly')
    cond2var.lab_i <- tclVar(ifelse(is.null(cond2var.lab), '', cond2var.lab))
    entry.cond2var.lab <- tkentry(var_selector, textvariable = cond2var.lab_i, width = 20)



    tkgrid(tklabel(var_selector, text = 'Variable: ', anchor = 'w'),
           tklabel(var_selector, text = 'Name: ', anchor = 'w'),
           tklabel(var_selector, text = 'Label: ', anchor = 'w'),
           sticky = 'w', padx = 5, pady = 5, row = 0)


    tkgrid(tklabel(var_selector, text = 'Response: ', anchor = 'w'),
           box.respvarvar,
           entry.respvar.lab,
           sticky = 'w', padx = 5, pady = 5, row = 1)

    tkgrid(tklabel(var_selector, text = 'Conditioning 1 (vertical): ', anchor = 'w'),
           box.cond1var,
           entry.cond1var.lab,
           sticky = 'w', padx = 5, pady = 5, row = 2)


    tkgrid(tklabel(var_selector, text = 'Conditioning 2 (horizontal): ', anchor = 'w'),
           box.cond2var,
           entry.cond2var.lab,
           sticky = 'w', padx = 5, pady = 5, row = 3)


    opt <- tkcheckbutton(final)
    optValue <- tclVar(as.character(as.numeric(optimize)))
    tkconfigure(opt, variable = optValue)


    submit <- tkbutton(final, text = 'Submit', command = function() updatecc())

    tkgrid(tklabel(final, text = 'Optimize R^2 '), opt, submit,
           sticky = 'nse', padx = 5, pady = 5)

    tkgrid(var_selector, sticky = 'w')
    tkgrid(final, sticky = 'e')
    tkgrid(overall)


    updatecc <- function() {

      respvar_new <- tclvalue(respvar_i)

      if (identical(tclvalue(respvar.lab_i), '')) {
        respvar.lab_new <- NULL
      } else {
        respvar.lab_new <- tclvalue(respvar.lab_i)
      }


      cond1var_new <- tclvalue(cond1var_i)

      if (identical(tclvalue(cond1var.lab_i), '')) {
        cond1var.lab_new <- NULL
      } else {
        cond1var.lab_new <- tclvalue(cond1var.lab_i)
      }

      cond2var_new <- tclvalue(cond2var_i)

      if (identical(tclvalue(cond2var.lab_i), '')) {
        cond2var.lab_new <- NULL
      } else {
        cond2var.lab_new <- tclvalue(cond2var.lab_i)
      }


      optimize_new <- as.character(tclvalue(optValue))
      optimize_new <- ifelse(optimize_new == '1', TRUE, FALSE)


      l_ccmaps(tt = w, var_inspector = FALSE,
               spdf = spdf,
               respvar = respvar_new, respvar.lab = respvar.lab_new,
               cond1var = cond1var_new, cond1var.lab = cond1var.lab_new,
               cond2var = cond2var_new, cond2var.lab = cond2var.lab_new,
               respbreaks = respbreaks,
               seg1col = seg1col, seg2col = seg2col, seg3col = seg3col,
               optimize = optimize_new, otry = otry,
               title = title)

    }

    tt_inspector

  }

  if (var_inspector) ccInspector(tt)


  ret <- list(top = tt,
              maps = list(base = p_base, polygons = p))

  return(invisible(ret))

}


## Helper Functions -----

#' Partitions x (numeric vector) into up to three intervals, with break points
#'   defined by the minimum, cut_lwr, cut_upr and maximum.
#'   Break points need not be unique
#'
cut_interval <- function(x, cuts) {

  cut(x,
      breaks = unique(c(min(x), cuts, max(x))),
      include.lowest = TRUE)

}


#' Renames the levels for a vector of factors levels from '1' up to '3'
#'   depending on the number of existing levels in use
#'
assign_levels <- function(x) {

  if (is.factor(x)) x <- droplevels(x)

  factor(x, labels = 1:nlevels(x))

}


#' Determines the colors to use for coloring regions. Needed since there might
#'   not be three categories for the study variable (e.g. when the first and
#'   second break point are equal)
#'
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


#' Calculates R^2 given the study variable values and the panel assignment
#'   based on the conditioning variables
#'
#' @importFrom dplyr group_by mutate ungroup summarise select
#' @importFrom magrittr %>%
#'
r2_calc <- function(x, group) {

  overall_mean <- mean(x)

  df <- data.frame(act_value = x, group = group) %>%
    group_by(group) %>%
    mutate(fitted_value = mean(act_value)) %>%
    ungroup()


  r2 <- df %>%
    summarise(sum((fitted_value - overall_mean)^2)/sum((act_value - overall_mean)^2)) %>%
    as.numeric()

  attr(r2, 'model_values') <- df %>% select(-act_value) %>% unique()

  r2

}


#' Computes the R^2 measures by trying a number of possible partitioning points
#'   on the two conditioning variables, and returns the results in a data.frame
#'   sorted in decreasing order by the R^2
#'
#' @importFrom dplyr arrange transmute mutate_all rowwise
#' @importFrom magrittr %>%
#'
r2_optimize <- function(otry, resp, cond1, cond2) {

  # otry number of values to try for each conditioning variable. Picks {otry choose 2} pairs (since
  # order doesn't matter) of points as the middle two break points for partioning, as well as
  # cases where the two middle break points are equal
  cond1_seq <- seq(min(cond1), max(cond1), length.out = otry)

  cond1_combn <- cbind(combn(cond1_seq, m = 2),
                       matrix(rep(cond1_seq, times = 2), nrow = 2, byrow = T))

  cond1_combn_str <- apply(cond1_combn, 2, function(x) paste0(x, collapse = ', '))


  cond2_seq <- seq(min(cond2), max(cond2), length.out = otry)

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

    # if (!is.null(spdf@plotOrder)) gr <- gr[spdf@plotOrder, ]

    r2_calc(resp, gr)

  }, list(resp), list(cond1), list(cond2),
  r2_df$cond1cut1, r2_df$cond1cut2, r2_df$cond2cut1, r2_df$cond2cut2)

  r2_df$r2 <- unlist(r2s)

  r2_df %>%
    arrange(desc(r2)) %>%
    data.frame()

}

