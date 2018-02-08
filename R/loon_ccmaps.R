
## First add a modified MinMaxSlider
## currently to use the WidgetFactory procedure
## the class needs to be in the loon::classes namespace

## note one \ is escaped with \\
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




## creater function
.Tcl(
  "
  proc minmax_scale2 {args} {
  return [WidgetFactory MinMaxScale2 minmaxscale2 {*}$args]
  }
  "
)

assignLevels <- function(x) {
  x <- droplevels(as.factor(x))

  if (nlevels(x)==1) {
    x <- factor(x, labels=c("1"))
  } else if (nlevels(x)==2) {
    x <- factor(x, labels=c("1", "2"))
  } else {
    x <- factor(x, labels=c("1", "2", "3"))
  }

  x
}

# Partitions x (numeric list) using min(x), max(x), cut1 and cut2 as the break points. The breaks
# do not need to be unique
cutInterval <- function(x, cut1, cut2) {
  cut(x, breaks=unique(c(min(x), cut1, cut2, max(x))), include.lowest=TRUE)
}

# Calculates slider settings for conditioning variables that gives high R^2 measures
optimize <- function(otry, data, resp, cond1, cond2) {

  grandmean <- mean(resp)

  # otry number of values to try for each conditioning variable. Picks {otry choose 2} pairs (since
  # order doesn't matter) of points as the middle two break points for partioning, as well as
  # cases where the two middle break points are equal.
  cond1combn <- combn(seq(min(cond1), max(cond1), length.out=otry), 2)
  mat1 <- matrix(rep(seq(min(cond1), max(cond1), length.out=otry),2), nrow=2, byrow=T)
  cond1combn <- cbind(cond1combn, mat1)
  cond2combn <- combn(seq(min(cond2), max(cond2), length.out=otry), 2)
  mat2 <- matrix(rep(seq(min(cond2), max(cond2), length.out=otry),2), nrow=2, byrow=T)
  cond2combn <- cbind(cond2combn, mat2)

  r2combn <- data.frame(numeric(), numeric(), numeric(), numeric(), numeric())

  # Assigns groupings based on new combination of partioning points, and saves the corresponding
  # R^2.
  for (i in 1:ncol(cond1combn)) {
    for (j in 1:ncol(cond2combn)) {
      Cond1Cut1 <- cond1combn[1,i]
      Cond1Cut2 <- cond1combn[2,i]
      Cond2Cut1 <- cond2combn[1,j]
      Cond2Cut2 <- cond2combn[2,j]

      data$cond1_cat <- cutInterval(-cond1, -Cond1Cut1, -Cond1Cut2)
      data$cond2_cat <- cutInterval(cond2, Cond2Cut1, Cond2Cut2)
      data$cond1_cat <- assignLevels(data$cond1_cat)
      data$cond2_cat <- assignLevels(data$cond2_cat)

      gr <- data.frame(group=paste0(data$cond1_cat,data$cond2_cat))
      if (!is.null(data@plotOrder)) {
        gr <- gr[data@plotOrder,]
      }

      r2 <- r2calc(resp[data@plotOrder], gr)
      r2combn <- rbind(r2combn, data.frame(Cond1Cut1, Cond1Cut2, Cond2Cut1, Cond2Cut2, r2))
    }
  }
  colnames(r2combn) <- c("cond1cut1", "cond1cut2", "cond2cut1", "cond2cut2", "rsquared")
  r2combn
}

# Source: https://github.com/waddella/loon/blob/5c2f0bb4fe71cbbd44ec97b96ec5f2ea1ec26a81/R/R/l_subwin.R
# Was not part of the loon package loaded, so I included the function here
# Creates a child path name under a parent (used for creating the sliders and labels for sliders)
l_subwin <- function(parent, name="w") {
  if(is(parent,"tkwin")) {
    parent <- .Tk.ID(parent)
  }

  i <- 0
  child <- paste0(parent, '.', name)
  while(as.logical(tcl('winfo','exists', child))) {
    i <- i + 1
    child <- paste0(parent, '.', name, i)
  }

  return(child)
}

# Determines the colors to use for coloring regions. Needed since there might not be three categories
# for the study variable (e.g. when the first and second break point are equal)
colorList <- function(x, min, max, cut1, cut2) {
  levels(x) <- levels(droplevels(x))
  if (nlevels(x)==3) {
    c("blue","grey","red")
  } else if (nlevels(x)==2) {
    if (min==cut1) { c("grey","red") }
    else if (max==cut2) { c("blue","grey") }
    else if (cut1==cut2) { c("blue","red") }
  } else {
    if (min==cut2) c("red")
    else if (max==cut1) c("blue")
    else c("grey")
  }
}

# Calculates R^2 given the study variable values (i.e. state values in the formula) and the panel
# assignment based on conditioning variables
r2calc <- function(x, g) {
  grandmean <- mean(x)
  modelval <- aggregate(x, by=list(g), mean)
  colnames(modelval) <- c("group", "modelvalue")
  stateval <- data.frame(statevalue=x, group=g)
  vals <- merge(stateval, modelval, by="group")
  r2 <- sum((vals$modelvalue - grandmean)^2)/sum((vals$statevalue-grandmean)^2)
  r2
}


#' Conditioned Choropleth Maps
#'
#' @export
#'
CCmaps_loon <- function (data, respvar, cond1var, cond2var, respbreaks=3, optimize=FALSE,
                         otry=10, title="CCmaps") {

  # Basic checks on input arguments
  if (!is(data, "SpatialPolygonsDataFrame")) {
    stop("Data has to be of SpatialPolygonsDataFrame class")}

  if (is.null(respvar)) {
    stop("Study variable required")
  }

  if (is.null(cond1var) | is.null(cond2var)) {
    stop("Two conditioning variables required")
  }

  if (length(respvar)!=1) {
    stop("Only one response variable allowed")
  }

  if (length(cond1var)!=1 | length(cond1var)!=1) {
    stop("The two conditioning variables must be entered as separate arguments")
  }

  if (otry%%1!=0 | otry<1) {
    stop("otry must be an integer of greater than 0")
  }

  if (!is.logical(optimize)) {
    stop("optimize must be either TRUE or FALSE")
  }

  if (!is.character(title)) {
    stop("title must be a string or NULL")
  }

  if (!is.numeric(respbreaks)) {
    stop("breaks must be either a numeric vector of four break points or a single 3")
  }

  if ((length(respbreaks)!=4) && (length(respbreaks)!=1)) {
    stop("breaks must be either a numeric vector of four break points or an integer 31")
  }

  if ((length(respbreaks)==1) && (respbreaks!=3)) {
    stop("breaks must be either a numeric vector of four break points or an integer 3")
  }

  if (!(respvar %in% names(data))) {
    stop("Response variable does not exist in the data")
  }

  if (!(cond1var %in% names(data)) | !(cond2var %in% names(data))) {
    stop("Conditioning variables must exist in the data")
  }

  tt <- tktoplevel()
  tktitle(tt) <- title

  n <- 9 # 9 plots from 3x3 panel

  # Group names, used for determining which regions are assigned to each plot. First number
  # refers to the horizontal conditioning variable category, and second number refers to the
  # vertical conditioning variable
  group <- c("11", "12", "13", "21", "22", "23", "31", "32", "33")

  resp <- data@data[,respvar]
  cond1 <- data@data[,cond1var]
  cond2 <- data@data[,cond2var]

  if (!is.numeric(resp)) {
    stop("Study variable must be continuous")
  }

  if (!is.numeric(cond1) | !is.numeric(cond2)) {
    stop("Conditioning variables must be continuous")
  }

  # If break points not given, use default of equal quantiles. Otherwise, use user-defined
  # break points
  if (respbreaks==3) {
    RespCut1 <- quantile(resp, probs=1/3)
    RespCut2 <- quantile(resp, probs=2/3)
  } else {
    RespCut1 <- respbreaks[2]
    RespCut2 <- respbreaks[3]
  }

  orig_RespCut1 <- RespCut1
  orig_RespCut2 <- RespCut2

  # Categorical variable corresponding to the intervals in study variable
  data$resp_cat <- cutInterval(resp, RespCut1, RespCut2)

  # If optimize=TRUE, break points for conditioning variables are obtained from the optimize()
  # function. Otherwise, use equal quantiles
  if (optimize==TRUE) {
    results <- optimize(otry, data, resp, cond1, cond2)
    results <- results[order(results$rsquared,decreasing=TRUE),]
    rsquared <- results[1,5]

    Cond1Cut1 <- results[1,1]
    Cond1Cut2 <- results[1,2]
    Cond2Cut1 <- results[1,3]
    Cond2Cut2 <- results[1,4]
  } else {
    Cond1Cut1 <- quantile(cond1, probs=1/3)
    Cond1Cut2 <- quantile(cond1, probs=2/3)
    Cond2Cut1 <- quantile(cond2, probs=1/3)
    Cond2Cut2 <- quantile(cond2, probs=2/3)
  }

  orig_Cond1Cut1 <- Cond1Cut1
  orig_Cond1Cut2 <- Cond1Cut2
  orig_Cond2Cut1 <- Cond2Cut1
  orig_Cond2Cut2 <- Cond2Cut2

  # Categorical variables corresponding to the levels in conditioning variables
  # For the vertical conditioning variable, need in reverse order (plotting top to bottom, but
  # the bottom rows represent lower levels)
  data$cond1_cat <- cutInterval(-cond1, -Cond1Cut1, -Cond1Cut2)
  data$cond2_cat <- cutInterval(cond2, Cond2Cut1, Cond2Cut2)
  data$cond1_cat <- assignLevels(data$cond1_cat)
  data$cond2_cat <- assignLevels(data$cond2_cat)

  # Data frame with the study variable, color assignment, and group (i.e. which panel) by region
  cols <- data.frame(resp=resp, resp_col=factor(data$resp_cat,
                                                labels=colorList(data$resp_cat, min(resp), max(resp), RespCut1, RespCut2)),
                     group=paste0(data$cond1_cat,data$cond2_cat))
  if (!is.null(data@plotOrder)) {
    cols <- cols[data@plotOrder,]
  }

  # Setting up plots and layer groups
  p <- vector(length=n)
  for (i in 1:length(p)) {
    p[i] <- l_plot(parent=tt)
  }

  g <- vector(length=n)
  for (i in 1:length(g)) {
    g[i] <- l_layer_group(p[i])
  }

  # Plots polygons
  pols <- vector("list",length=n)
  for (i in 1:length(pols)) {
    pols[[i]] <- l_layer(p[i], parent=g[i], data, color="cornsilk", asSingleLayer=FALSE)
    l_scaleto_world(p[i])
  }

  # Color according to study response category and panel assignment
  for (k in 1:n) {
    for (i in 1:length(slot(data, "polygons"))) {
      for (j in 1:length(slot(data@polygons[[data@plotOrder[i]]], "Polygons"))) {
        l_configure(c(p[k], pols[[k]][[i]][j]), color=ifelse((cols[i,3]==group[k]),
                                                             as.character(cols[i,2]), "cornsilk"))
      }
    }
  }

  # Model value and R^2 label
  r2 <- r2calc(resp[data@plotOrder], cols$group)
  r2label <- tcl('label',l_subwin(tt,'r2label'), text=paste0("R^2: ", round(r2,2)))

  modelvalues <- aggregate(cols$resp, by=list(cols$group), mean)
  colnames(modelvalues) <- c("group", "resp")

  xcoord <- data@bbox[1,1] - 0.15
  ycoord <- data@bbox[2,1]

  plabel <- vector(length=n)
  for (i in 1:length(plabel)) {
    if (length(modelvalues$resp[which(modelvalues$group==as.numeric(group[i]))])==1) {
      plabel[i] <- l_layer_text(p[i],x=xcoord, y=ycoord,
                                text=round(modelvalues$resp
                                           [which(modelvalues$group==as.numeric(group[i]))],2),
                                size=10, index="end", color="grey")
    } else {
      plabel[i] <- l_layer_text(p[i],x=xcoord, y=ycoord,
                                text="NA", size=10, index="end", color="grey")
    }
    l_scaleto_world(p[i])
  }

  # Update function to be used on sliders
  updateGraph <- function() {

    RespCut1 <- as.numeric(tkcget(scaletop, "-min"))
    RespCut2 <- as.numeric(tkcget(scaletop, "-max"))
    Cond1Cut1 <- as.numeric(tkcget(scaleright, "-min"))
    Cond1Cut2 <- as.numeric(tkcget(scaleright, "-max"))
    Cond2Cut1 <- as.numeric(tkcget(scalebottom, "-min"))
    Cond2Cut2 <- as.numeric(tkcget(scalebottom, "-max"))

    data$resp_cat <- cutInterval(resp, RespCut1, RespCut2)
    data$cond1_cat <- cutInterval(-cond1, -Cond1Cut1, -Cond1Cut2)
    data$cond2_cat <- cutInterval(cond2, Cond2Cut1, Cond2Cut2)
    data$cond1_cat <- assignLevels(data$cond1_cat)
    data$cond2_cat <- assignLevels(data$cond2_cat)

    cols <- data.frame(resp=resp, resp_col=factor(data$resp_cat,
                                                  labels=colorList(data$resp_cat, min(resp),
                                                                   max(resp), RespCut1, RespCut2)),
                       group=paste0(data$cond1_cat,data$cond2_cat))
    if (!is.null(data@plotOrder)) {
      cols <- cols[data@plotOrder,]
    }

    for (k in 1:n) {
      for (i in 1:length(slot(data, "polygons"))) {
        for (j in 1:length(slot(data@polygons[[data@plotOrder[i]]], "Polygons"))) {
          l_configure(c(p[k], pols[[k]][[i]][j]),
                      color=ifelse((cols[i,3]==group[k]), as.character(cols[i,2]), "cornsilk"))
        }
      }
    }

    newr2 <- r2calc(cols$resp, cols$group)
    tcl(r2label, 'configure', text=paste0("R^2: ", round(newr2,2)))

    modelvalues <- aggregate(cols$resp, by=list(cols$group), mean)
    colnames(modelvalues) <- c("group", "resp")

    for (i in 1:n) {
      if (sum(modelvalues$group==as.numeric(group[i]))==1) {
        l_configure(c(p[i],plabel[i]),
                    text=round(modelvalues$resp[which(modelvalues$group==as.numeric(group[i]))],2))
      } else {
        l_configure(c(p[i],plabel[i]), text="NA")
      }
    }
  }

  # Reset function to be used with the Reset button. Restores to initial slider settings when
  # the code was first ran
  Reset <- function(RespCut1=orig_RespCut1, RespCut2=orig_RespCut2, Cond1Cut1=orig_Cond1Cut1,
                    Cond1Cut2=orig_Cond1Cut2, Cond2Cut1=orig_Cond2Cut1, Cond2Cut2=orig_Cond2Cut2) {

    tcl(scaletop, 'configure', min=RespCut1, max=RespCut2)
    tcl(scaleright, 'configure', min=Cond1Cut1, max=Cond1Cut2)
    tcl(scalebottom, 'configure', min=Cond2Cut1, max=Cond2Cut2)

    data$resp_cat <- cutInterval(resp, RespCut1, RespCut2)
    data$cond1_cat <- cutInterval(-cond1, -Cond1Cut1, -Cond1Cut2)
    data$cond2_cat <- cutInterval(cond2, Cond2Cut1, Cond2Cut2)
    data$cond1_cat <- assignLevels(data$cond1_cat)
    data$cond2_cat <- assignLevels(data$cond2_cat)

    cols <- data.frame(resp=resp,
                       resp_col=factor(data$resp_cat, labels=colorList(data$resp_cat, min(resp),
                                                                       max(resp), RespCut1, RespCut2)),
                       group=paste0(data$cond1_cat,data$cond2_cat))
    if (!is.null(data@plotOrder)) {
      cols <- cols[data@plotOrder,]
    }

    for (k in 1:n) {
      for (i in 1:length(slot(data, "polygons"))) {
        for (j in 1:length(slot(data@polygons[[data@plotOrder[i]]], "Polygons"))) {
          l_configure(c(p[k], pols[[k]][[i]][j]),
                      color=ifelse((cols[i,3]==group[k]), as.character(cols[i,2]), "cornsilk"))
        }
      }
    }

    newr2 <- r2calc(cols$resp, cols$group)
    tcl(r2label, 'configure', text=paste0("R^2: ", round(newr2,2)))

    modelvalues <- aggregate(cols$resp, by=list(cols$group), mean)
    colnames(modelvalues) <- c("group", "resp")

    for (i in 1:n) {
      if (sum(modelvalues$group==as.numeric(group[i]))==1) {
        l_configure(c(p[i],plabel[i]),
                    text=round(modelvalues$resp[which(modelvalues$group==as.numeric(group[i]))],2))
      } else {
        l_configure(c(p[i],plabel[i]), text="NA")
      }
    }
  }

  # Creates sliders and slider labels
  labelscaletop <- tcl('label',l_subwin(tt,'scalelabel_top'),text=respvar)
  scaletop <- tcl('::minmax_scale2', l_subwin(tt, 'scaletop'), from=min(resp),
                  to=max(resp),min=RespCut1, max=RespCut2, resolution=0.1,
                  seg1col="blue", seg3col="red", orient="horizontal")

  labelscaleright <- tcl('label',l_subwin(tt,'scalelabel_right'),text=cond1var)
  scaleright <- tcl('::minmax_scale2', l_subwin(tt, 'scaleright'), resolution=0.1,
                    from=min(cond1), to=max(cond1),min=Cond1Cut1, max=Cond1Cut2, orient="vertical")

  labelscalebottom <- tcl('label',l_subwin(tt,'scalelabel_bottom'),text=cond2var)
  scalebottom <- tcl('::minmax_scale2', l_subwin(tt, 'scalebottom'), resolution=0.1,
                     from=min(cond2), to=max(cond2), min=Cond2Cut1, max=Cond2Cut2, orient="horizontal")

  tcl(scaletop, 'configure', command=function(...)updateGraph())
  tcl(scalebottom, 'configure', command=function(...)updateGraph())
  tcl(scaleright, 'configure', command=function(...)updateGraph())

  # Creates reset button
  resetbutton <- tkbutton(tt, command=function(...)Reset(), text="Reset Sliders")

  # Layout
  tkgrid(labelscaletop, row=0, column=0, sticky="e")
  tkgrid(scaletop, row=0, column=1, sticky="new")
  for (i in 1:3) {
    for (j in 0:2) {
      tkgrid(p[(i-1)*3+j+1], row=i, column=j, sticky="nesw")
    }
  }

  tkgrid(labelscalebottom, row=4, column=0, sticky="e")
  tkgrid(scalebottom, row=4, column=1, sticky="new")
  tkgrid(labelscaleright, row=1, column=3, sticky="sew")
  tkgrid(scaleright, row=2, column=3, rowspan=2, sticky="new")
  tkgrid(r2label, row=4, column=3, sticky="nesw")
  tkgrid(resetbutton, row=4, column=0)

  tkgrid.columnconfigure(tt, 3, weight=10)
  tkgrid.rowconfigure(tt, 0, weight=4)
  tkgrid.rowconfigure(tt, 4, weight=4)
  for (i in 1:3) { tkgrid.rowconfigure(tt, i, weight=5) }
  for (j in 0:2) { tkgrid.columnconfigure(tt, j, weight=5) }

}

