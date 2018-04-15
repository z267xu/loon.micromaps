
library(rgdal)

# Read in data -----
usa <- readOGR(dsn = "data/usa_states", layer="states")
# Source: http://www.arcgis.com/home/item.html?id=f7f805eb65eb4ab787a0a3e1116ca7e

statedata <- read.csv("data/usa_states/statedata.csv")
# Source:

usadata <- merge(usa, statedata, by.x = "STATE_ABBR", by.y = "state.abbr")


# Pre-processing on polygons -----

## Remove the smaller polygons from Alaska and Hawaii, only keeping the main island since the other
# islands are very small
ak <- which(usadata@data$STATE_ABBR == 'AK')

ak_pols <- slot(usadata@polygons[[ak]], 'Polygons')

ak_pols_areas <- vapply(ak_pols, function(x) slot(x, 'area'), FUN.VALUE = numeric(1))

ak_keep <- which.max(ak_pols_areas)

slot(usadata@polygons[[ak]], "Polygons") <- ak_pols[ak_keep]
slot(usadata@polygons[[ak]], "plotOrder") <- 1L


hi <- which(usadata@data$STATE_ABBR == 'HI')

hi_pols <- slot(usadata@polygons[[hi]], 'Polygons')

hi_pols_areas <- vapply(hi_pols, function(x) slot(x, 'area'), FUN.VALUE = numeric(1))

hi_keep <- which.max(hi_pols_areas)

slot(usadata@polygons[[hi]], "Polygons") <- hi_pols[hi_keep]
slot(usadata@polygons[[hi]], "plotOrder") <- 1L


## Shift the Hawaii polygon closer to continental U.S.
usadata@polygons[[hi]]@Polygons[[1]]@coords[, 1] <- usadata@polygons[[hi]]@Polygons[[1]]@coords[, 1] + 30
usadata@polygons[[hi]]@Polygons[[1]]@coords[, 2] <- usadata@polygons[[hi]]@Polygons[[1]]@coords[, 2] + 10


## Scale the Alaska polygon smaller and shift it closer to continental U.S.
xmin <- bbox(usa@polygons[[ak]]@Polygons[[1]])[1, 1]
xmax <- bbox(usa@polygons[[ak]]@Polygons[[1]])[1, 2]
ymin <- bbox(usa@polygons[[ak]]@Polygons[[1]])[2, 1]
ymax <- bbox(usa@polygons[[ak]]@Polygons[[1]])[2, 2]

usadata@polygons[[ak]]@Polygons[[1]]@coords[, 1] <- xmin +
  10*(usadata@polygons[[ak]]@Polygons[[1]]@coords[, 1] - xmin)/(xmax - xmin)

usadata@polygons[[ak]]@Polygons[[1]]@coords[, 2] <- ymin +
  10*(usadata@polygons[[ak]]@Polygons[[1]]@coords[, 2] - ymin)/(xmax - xmin)

usadata@polygons[[ak]]@Polygons[[1]]@coords[, 1] <- usadata@polygons[[ak]]@Polygons[[1]]@coords[, 1] + 30
usadata@polygons[[ak]]@Polygons[[1]]@coords[, 2] <- usadata@polygons[[ak]]@Polygons[[1]]@coords[, 2] - 10


## Get updated bbox coordinates
min_x <- NULL
min_y <- NULL
max_x <- NULL
max_y <- NULL

for (i in 1:length(slot(usadata, "polygons"))) {
  for (j in 1:length(slot(usadata@polygons[[i]], "Polygons"))) {

    min_x <- min(min_x, bbox(usadata@polygons[[i]]@Polygons[[j]])[1, 1])
    min_y <- min(min_y, bbox(usadata@polygons[[i]]@Polygons[[j]])[2, 1])
    max_x <- max(max_x, bbox(usadata@polygons[[i]]@Polygons[[j]])[1, 2])
    max_y <- max(max_y, bbox(usadata@polygons[[i]]@Polygons[[j]])[2, 2])

  }
}

slot(usadata, "bbox") <- matrix(c(min_x, min_y, max_x, max_y), ncol = 2)


# Draw micromaps -----
l_micromaps(spdf = usadata,
            lab.label = 'States',
            variables = list(id.var = 'STATE_ABBR',
                             grouping.var = list(name = 'lung_bronch_death'),
                             var2 = list(name = 'pm25', label = 'Fine Particulate Matter Level'),
                             var3 = list(name = 'income', label = 'Income per Capita')),
            linkingGroup = 'States', sync = 'push')


# Draw CCmaps -----
l_ccmaps(spdf = usadata,
         respvar = 'lung_bronch_death', respvar.lab = 'Lung & Bronchus Cancer Death Rate',
         cond1var = 'pm25', cond1var.lab = 'Fine Particulate Matter Level',
         cond2var = 'income', cond2var.lab = 'Income per Capita',
         optimize = TRUE, otry = 10,
         title = 'CCmaps')
# Takes ~40s to run. For faster results without optimizing R^2, set optimize=FALSE

