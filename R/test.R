
tt <- tktoplevel()
b <- vector(length = 2)
b2 <- vector(length = 2)
b3 <- vector(length = 3)

l_scat <- l_plot(parent = tt,
                 x = data$pov,
                 y = seq(grouping, grouping - nrow(data) + 1),
                 color = palette[1:nrow(data)],
                 size = 6)

p <- l_plot(parent = tt)

p_map <- l_layer(p, USstates,
                 color = 'cornsilk',
                 asSingleLayer = TRUE,
                 label = 'map',
                 index = 'end')

l_scaleto_world(p)

mapping_scat2map <- lapply(data$NAME, function(x) which(attr(p_map, 'NAME') == x))
names(mapping_scat2map) <- data$NAME

# mapping_map2scat <- sapply(attr(p_map, 'NAME'), function(x) which(data$NAME == x))
# mapping_map2scat <- Filter(function(x) length(x) > 0, mapping_map2scat)

mapping_map2scat <- match(attr(p_map, 'NAME'), data$NAME)


cols <- data.frame(ST = df$ST, key = 1:nrow(df), colors = NA, stringsAsFactors = F)

cols$colors <- data$colors[match(cols$ST, data$ST)]
cols$colors[is.na(cols$colors)] <- 'cornsilk'

mapcolors <- cols$colors[match(plotorder_f, cols$key)]


updateMap_sp <- function() {

  sel <- l_scat['selected']

  i <- unlist(mapping_scat2map[sel])

  orig_cols <- mapcolors
  new_cols <- orig_cols
  new_cols[i] <- 'magenta'

  p_map['color'] <- new_cols

}

b[1] <- l_bind_state(l_scat, 'selected', function(){updateMap_sp()})


updatePlot_sp <- function(add) {

  i <- l_currentindex(p)

  if(i !=-1 && !is.na(mapping_map2scat[i])) {

    if (add) {

      l_configure(l_scat, selected = TRUE, which = mapping_map2scat[i])


    } else {

      sel <- rep(FALSE, l_scat['n'])
      sel[mapping_map2scat[i]] <- TRUE
      l_scat['selected'] <- sel

    }
  }
}

b2[1] <- l_bind_item(p, paste0(p_map), '<ButtonPress-1>',
                  function(){updatePlot_sp(FALSE)})
b3[1] <- l_bind_item(p, paste0(p_map), '<Shift-ButtonPress-1>',
                  function(){updatePlot_sp(TRUE)})




data2 <- df[1:8, ]
data2$colors <- palette[1:nrow(data2)]

l_scat2 <- l_plot(parent = tt,
                 x = data2$pov,
                 y = seq(grouping, grouping - nrow(data2) + 1),
                 color = palette[1:nrow(data2)],
                 size = 6)

p2 <- l_plot(parent = tt)

p_map2 <- l_layer(p2, USstates,
                 color = 'cornsilk',
                 asSingleLayer = TRUE,
                 label = 'map',
                 index = 'end')

l_scaleto_world(p2)

mapping_scat2map2 <- lapply(data2$NAME, function(x) which(attr(p_map2, 'NAME') == x))
names(mapping_scat2map2) <- data2$NAME

# mapping_map2scat <- sapply(attr(p_map, 'NAME'), function(x) which(data$NAME == x))
# mapping_map2scat <- Filter(function(x) length(x) > 0, mapping_map2scat)

mapping_map2scat2 <- match(attr(p_map2, 'NAME'), data2$NAME)


cols2 <- data.frame(ST = df$ST, key = 1:nrow(df), colors = NA, stringsAsFactors = F)

cols2$colors <- data2$colors[match(cols2$ST, data2$ST)]
cols2$colors[is.na(cols2$colors)] <- 'cornsilk'

mapcolors2 <- cols2$colors[match(plotorder_f, cols2$key)]


updateMap_sp2 <- function() {

  sel <- l_scat2['selected']

  i <- unlist(mapping_scat2map2[sel])

  orig_cols <- mapcolors2
  new_cols <- orig_cols
  new_cols[i] <- 'magenta'

  p_map2['color'] <- new_cols

}

b[2] <- l_bind_state(l_scat2, 'selected', function(){updateMap_sp2()})


updatePlot_sp2 <- function(add) {

  i <- l_currentindex(p2)

  if(i !=-1 && !is.na(mapping_map2scat2[i])) {

    if (add) {

      l_configure(l_scat2, selected = TRUE, which = mapping_map2scat2[i])


    } else {

      sel <- rep(FALSE, l_scat2['n'])
      sel[mapping_map2scat2[i]] <- TRUE
      l_scat2['selected'] <- sel

    }
  }
}

b2[2] <- l_bind_item(p2, paste0(p_map2), '<ButtonPress-1>',
                  function(){updatePlot_sp2(FALSE)})
b3[2] <- l_bind_item(p2, paste0(p_map2), '<Shift-ButtonPress-1>',
                  function(){updatePlot_sp2(TRUE)})



tkgrid(p, row = 0, column = 0, sticky = "nesw")
tkgrid(l_scat, row = 0, column = 1, sticky = "nesw")
tkgrid(p2, row = 1, column = 0, sticky = "nesw")
tkgrid(l_scat2, row = 1, column = 1, sticky = "nesw")
# l_bind_item(p, 'all', '<ButtonPress-1> ', function(W) print(l_currenttags(W)))
# l_bind_item(l_scat, 'all', '<ButtonPress-1> ', function(W) print(l_currenttags(W)))
