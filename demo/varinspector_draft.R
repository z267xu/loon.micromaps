
# https://stackoverflow.com/questions/3482513/multiple-comboboxes-in-r-using-tcltk


mmInspector <- function(w) {

  tt_inspector <- tktoplevel()
  tktitle(tt_inspector) <- 'Micromaps Inspector'


  vars <- setdiff(names(spdf@data)[sapply(spdf@data, is.numeric)],
                  c('id', 'name', 'NAME'))


  ord.var_i <- tclVar(ord.var)
  box.ord.var <- ttkcombobox(tt_inspector, values = vars,
                             textvariable = ord.var_i,
                             state = 'readonly')
  ord.var.label_i <- tclVar(ord.var.label)
  entry.ord.var.label <- tkentry(tt_inspector, textvariable = ord.var.label_i, width = 20)
  ord.var.axis_i <- tclVar(ord.var.axis)
  entry.ord.var.axis <- tkentry(tt_inspector, textvariable = ord.var.axis_i, width = 20)



  var2_i <- tclVar(ifelse(is.null(var2), 'Choose one', var2))
  box.var2 <- ttkcombobox(tt_inspector, values = c('NA', vars),
                          textvariable = var2_i,
                          state = 'readonly')
  var2.label_i <- tclVar(var2.label)
  entry.var2.label <- tkentry(tt_inspector, textvariable = var2.label_i, width = 20)
  var2.axis_i <- tclVar(var2.axis)
  entry.var2.axis <- tkentry(tt_inspector, textvariable = var2.axis_i, width = 20)


  submit <- tkbutton(tt_inspector, text = 'Submit', command = function() updatemm())


  tkgrid(tklabel(tt_inspector, text = 'Ordering variable: '), box.ord.var,
         tklabel(tt_inspector, text = 'Plot label: '), entry.ord.var.label,
         tklabel(tt_inspector, text = 'Axis label: '), entry.ord.var.axis,
         sticky = 'sw', padx = 5, pady = 5)

  tkgrid(tklabel(tt_inspector, text = 'Second variable (optional): '), box.var2,
         tklabel(tt_inspector, text = 'Plot label: '), entry.var2.label,
         tklabel(tt_inspector, text = 'Axis label: '), entry.var2.axis,
         sticky = 'sw', padx = 5, pady = 5)


  tkgrid(submit, sticky = 'ew', padx = 5, pady = 5, column = 5)


  updatemm <- function() {

    ord.var_new <- tclvalue(ord.var_i)
    ord.var.axis_new <- tclvalue(ord.var.axis_i)
    ord.var.label_new <- tclvalue(ord.var.label_i)

    var2_new <- tclvalue(var2_i)

    if (var2_new == 'NA') var2_new <- NULL


    var2.axis_new <- tclvalue(var2.axis_i)
    var2.label_new <- tclvalue(var2.label_i)

    l_micromaps(tt = mm$top,
                spdf = spdf, labels = labels, grouping = grouping,
                ord.var = ord.var_new, ord.var.axis = ord.var.axis_new , ord.var.label = ord.var.label_new,
                var2 = var2_new, var2.axis = var2.axis_new, var2.label = var2.label_new,
                map.label = map.label, lab.label = lab.label, title = title)


  }

  tt_inspector

}


mmInspector(mm)

