# https://www.wvview.org/os_sa/15b_Raster_Analysis_terra.html
# https://gis.stackexchange.com/questions/436817/focalmat-weights-adjustment
# https://zia207.quarto.pub/digital-terrain-analysis.html
# https://stackoverflow.com/questions/67569214/how-to-calculate-geographic-distance-from-point-a-along-its-contour-line-to-the
# https://microsite.geo.uzh.ch/geo372/PDF/week4_geo372_terrain.pdf
# https://www.spatialanalysisonline.com/HTML/gradient__slope_and_aspect.htm
v <- vect(system.file("ex/lux.shp", package="terra"))
r <- r_ <- rast(system.file("ex/elev.tif", package="terra"))
r[45:50, 45:50] <- NA
plot(r_)
plot(r)

circle_focal <- focal(r, focalMat(r, 2, "circle"), f)
# Error: [focal] nrow(w) > 2 * nrow(x)