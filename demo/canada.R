
library(rgdal)

canada <- readOGR(dsn = ".", layer = 'canada')

# https://gis.stackexchange.com/questions/83416/extract-points-from-regular-data-frame-with-coordinates-based-on-spatialpolygons
