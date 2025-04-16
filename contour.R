# TODO
# https://eliocamp.github.io/metR/reference/geom_contour_fill.html
# https://cran.r-project.org/web/packages/metR/vignettes/Visualization-tools.html
# https://stackoverflow.com/questions/69778748/ggplot-label-contours-with-sf-object
# stars density
# https://www.andrewheiss.com/blog/2023/07/28/gradient-map-fills-r-sf/
# https://stackoverflow.com/questions/68643517/smoothed-density-maps-for-points-in-using-sf-within-a-given-boundary-in-r

library(purrr)
library(eks)

# kde2d contour -----------------------------------------------------------------
# https://gis.stackexchange.com/questions/444689/create-density-polygons-from-spatial-points-in-r
xy <-  st_coordinates(landslides_c %>% st_intersection(bnd))
de <- MASS::kde2d(xy[,1], xy[,2])
cl <- contourLines(de)
cllines <- do.call(rbind,
                   Map(function(x){
                     st_as_sf(
                       data.frame(
                         Z=x$level,
                         geometry=st_sfc(
                           st_linestring(cbind(x$x, x$y))
                         )
                       ), 
                       crs = crs_nepal$input
                     )},cl
                   ))


ggplot() + geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1) + 
  geom_sf(data = cllines) + 
  geom_sf (data = nepal_bnd, col = "blue" , fill = NA) + 
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave("figures/landslides_contour_kde2d.pdf", width = tw, height = tw)

# density2d contour ---------------------------------------------------

# https://www.andrewheiss.com/blog/2023/07/28/gradient-map-fills-r-sf/
# https://gis.stackexchange.com/questions/444689/create-density-polygons-from-spatial-points-in-r
landslides_c_bnd <- landslides_c %>% st_intersection(bnd)
landslides_c_ <- as.data.frame(landslides_c_bnd) #convert to dataframe
# TODO this does not work for some reasons
# landslides_c_$x <- st_coordinates(landslides_c)[1]
# landslides_c_$y <- st_coordinates(landslides_c)[2]

# takes forever
# ggplot(landslides_c_, aes(x, y, z = logarea_m2)) +
#   geom_contour_fill() +
#   geom_contour(color = "black", size = 0.1)
#   # geom_sf (data = nepal_bnd, col = "blue" , fill = NA) + 
#   # geom_sf(data = bnd, col = "red", fill = NA)
# ggsave("figures/landslides_contour.pdf", width = tw, height = tw)

landslides_c_$x <- unlist(map(landslides_c_bnd$geometry,1)) #adding lon column
landslides_c_$y <- unlist(map(landslides_c_bnd$geometry,2)) #adding lat column


#Plot using geom_density_2d_filled()
p <- ggplot(data = landslides_c_, aes(x,y)) + 
  # This would create filled polygons that we use for create our polygon
  geom_density2d_filled() +
  geom_point() +
  geom_density_2d() 
p

ggsave("figures/landslides_density.pdf", width = tw, height = tw)

landslides_c_$logarea_m2_gp <- cut(landslides_c_$logarea_m2, breaks = 5)

p <- ggplot(data = landslides_c_, aes(x,y)) + 
  # This would create filled polygons that we use for create our polygon
  geom_density2d_filled(contour_var = "ndensity") +
  geom_point() +
  geom_density_2d(contour_var = "ndensity") 
p

ggsave("figures/landslides_ndensity.pdf", width = tw, height = tw)


data2d <- layer_data(p, i = 1) 
# Id of polygon
data2d$pol <- paste0(data2d$group, "_", data2d$subgroup)
ids <- unique(data2d$pol)

# Split and create polygons based on the id
pols <- lapply(ids, function(x){
  topol <- data2d[data2d$pol == x, ]
  
  closepol <- rbind(topol, topol[1, ])
  
  pol <- st_polygon(list(as.matrix(closepol[,c("x", "y")])))
  
  # Add features
  df <- unique(topol[, grepl("level", names(topol))])
  
  tofeatures <- st_as_sf(df, geometry=st_sfc(pol))
  
  return(tofeatures)  
})

lds_contour <- do.call(rbind, pols)

# And force a crs, since we lost that on the process
st_crs(lds_contour) <- crs_nepal$input

ggplot() +
  geom_sf(data = lds_contour, aes(fill = level)) +
  geom_sf(data = landslides_c,size = 0.005, alpha = .5) + 
  geom_sf (data = nepal_bnd, col = "blue" , fill = NA) + 
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave("figures/landslides_density_den2d.pdf", width = tw, height = tw)

ggplot() +
  geom_sf(data = landslides_c,size = 0.005) + 
  geom_sf(data = lds_contour) +
  geom_sf (data = nepal_bnd, col = "blue" , fill = NA) + 
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave("figures/landslides_contour_den2d.pdf", width = tw, height = tw)

# 
# ggplot() +
# geom_density_2d_filled(data = landslides_c, mapping = aes(logarea_m2))
# 
# landslides_c_density <- st_as_sf(stars::st_as_stars(density(landslides_c, dimyx = 1)))
# 
# # Check to see what it looks like
# plot(density_campgrounds_stars)
# 
# landslides_c_ <- st_coordinates(landslides_c)
# EKS ---------------------------------------------------------------------


# https://adelieresources.com/2022/10/making-contour-maps-in-r/
Sta_den <- eks::st_kde(landslides_c) # calculate density

ggplot() +
  geom_sf(data=landslides_c, size = 0.01) +
  geom_sf(data=eks::st_get_contour(Sta_den,
                                   cont=c(20,40,60,80)),   
          aes(fill=eks::label_percent(contlabel)), alpha=.5) +
  colorspace::scale_fill_discrete_sequential(name = "Density Percentile") +
  geom_sf(data = nepal_bnd, col = "blue", fill = NA) +
  geom_sf(data = bnd, col = "red", fill = NA) +
  #   Add a scale bar
  # ggspatial::annotation_scale(data=landslides_c, aes(unit_category="imperial", style="ticks"),
  #                             location="br", width_hint=0.2, bar_cols=1) +
  #   This is an alternative way to add a scale
  # ggsn::scalebar(landslides_c, dist = 2, dist_unit = "mi", location="bottomleft",
  #           transform = TRUE, model = "WGS84") +
  #   Add title
  coord_sf(crs=crs_nepal) # required 
ggsave("figures/landslides_density_eks.pdf", width = tw, height = tw)


## without fill ------------------------------------------------------------


ggplot() +
  geom_sf(data=landslides_c, size = 0.01) +
  geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
  geom_sf(data = bnd, col = "red", fill = NA) +
  #   Add a scale bar
  # ggspatial::annotation_scale(data=landslides_c, aes(unit_category="imperial", style="ticks"),
  #                             location="br", width_hint=0.2, bar_cols=1) +
  #   This is an alternative way to add a scale
  # ggsn::scalebar(landslides_c, dist = 2, dist_unit = "mi", location="bottomleft",
  #           transform = TRUE, model = "WGS84") +
  #   Add title
  coord_sf(crs=crs_nepal) # required 
ggsave("figures/landslides_density_eks_nofill.pdf", width = tw, height = tw)



# spatstat ------------------------------------------------------------------
# https://kevintshoemaker.github.io/NRES-746/sppm.htmllibrary(spatstat)
gcov <- gorillas_sf_gcov()
gorillas_sf_ <- gorillas_sf$nests
gorillas_sf_$vegetation <- terra::extract(gcov$vegetation, vect(st_geometry(gorillas_sf$nests)), ID = FALSE)

gorillas_ppp <- as.ppp(gorillas_sf_)

p <- ggplot() + 
  geom_density2d_filled(data = gorillas_sf_, 
                        # alpha = 0.4, 
                        contour_var="density",
                        aes(x=x, y=y, 
                            fill = vegetation)) +
  geom_point() +
  geom_density_2d() 
p

ggsave("figures/landslides_density_logarea.pdf", width = tw, height = tw)


# contour by values -------------------------------------------------------


# https://adelieresources.com/2022/10/making-contour-maps-in-r/
