
# Load code for modified minmaxscale (slider with 2 handles)
.onLoad <- function(libname, pkgname) {

  ## Modified MinMaxSlider
  # Note that one \ is escaped with \\
  # Definition of modified MinMaxSlider (2 handles)
  .Tcl(
    '
    oo::class create ::loon::classes::MinMaxScale2 {

    superclass ::loon::classes::Inspector2

    variable path canvas from to min max resolution orient seg1col seg2col seg3col from_text to_text\\
    b_w b_e b_s b_n slider_width slider_width2 slider_peak pad_text\\
    current_slider n_pix_per_res n_pix_per_res2\\
    mouse_x mouse_y

    constructor {Path} {

    set mouse_x 0

    set current_slider ""
    set n_pix_per_res 1
    set n_pix_per_res2 0.5

    set b_w 20
    set b_e 20
    set b_s 20
    set b_n 20
    set slider_peak 5
    set slider_width 12
    set slider_width2 6
    set pad_text 2

    next $Path

    my New_state from double 1 0
    my New_state to double 1 1
    my New_state min double 1 0
    my New_state max double 1 1
    my New_state from_text double 1 0
    my New_state to_text double 1 1
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
    my SetStateDescription from_text\\
    "from value text of scale"
    my SetStateDescription to_text\\
    "to value text of scale"
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

    pack $canvas -fill none -expand TRUE -side top

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

    $canvas create text\\
    [expr {$x0 - $slider_width2}] [expr {$y1 + $slider_peak + $pad_text}]\\
    -text [format "%.3g" $from_text] -anchor n -justify center

    $canvas create rect\\
    $loc_max $y0 [expr {$loc_max + $slider_width}] $y1\\
    -fill white -tag max

    $canvas create text\\
    [expr {$loc_max + $slider_width2}] [expr {$y0 - $slider_peak - $pad_text}]\\
    -text [format "%.3g" $max] -anchor s -justify center

    $canvas create text\\
    [expr {$x1 + $slider_width2}] [expr {$y0 - $slider_peak - $pad_text}]\\
    -text [format "%.3g" $to_text] -anchor s -justify center

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

    $canvas create text\\
    [expr {$x0 - 6*$pad_text}] $y1\\
    -text [format "%.3g" $from_text] -anchor n -justify center

    $canvas create rect\\
    $x0 $loc_max $x1 [expr {$loc_max - $slider_width}]\\
    -fill white -tag max

    $canvas create text\\
    [expr {$x1 + 6*$pad_text}] $loc_max\\
    -text [format "%.3g" $max] -anchor s -justify center

    $canvas create text\\
    [expr {$x1 + 6*$pad_text}] $y0\\
    -text [format "%.3g" $to_text] -anchor s -justify center

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

}

