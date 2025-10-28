# fm_pixels for prediction ----------------------------------------------------------
# trying to make square pixels
xdiff <- st_bbox(bnd)[3] - st_bbox(bnd)[1]
ydiff <- st_bbox(bnd)[4] - st_bbox(bnd)[2]

# pxl %<-% {
#   fm_pixels(mesh_fm, dims = c(x_pxl, ceiling(x_pxl * ydiff / xdiff)), mask = bnd)
# }

pxl_terra <- fm_pixels(mesh_fm,
  dims = c(x_pxl, ceiling(x_pxl * ydiff / xdiff)), mask = bnd,
  format = "terra"
)

pxl_terra$count <- mask(rasterize(vect(landslides_c), pxl_terra, fun = "length", background = 0), pxl_terra$.mask)

# counting number of landslides in each spatraster cell
if (CV_thin | CV_chess) {
  pxl_terra$count_test <- mask(rasterize(vect(landslides_c_test), pxl_terra, fun = "length", background = 0), pxl_terra$.mask)
  # sum(values(pxl_terra$count), na.rm =TRUE)
  # > nrow(landslides_c)
  # [1] 10278 MATCH
}

# for predict.bru()
pxl <- st_as_sf(as.points(pxl_terra))

if (FALSE) {
  pxl_sf <- sf::st_as_sf(
    terra::intersect(
      as.polygons(pxl_terra, aggregate = FALSE, na.rm = FALSE), # na.rm = TRUE will remove landslides points
      terra::vect(bnd)
    )
  )
  box <- c(
    xmin = 353, xmax = 358,
    ymin = 3028, ymax = 3033
  )
  box <- st_bbox(box, crs = crs_nepal)
  pxl_sf_zm <- st_crop(pxl_sf, box)
  pxl_zm <- st_crop(pxl, box)
  ggplot() +
    geom_sf(data = pxl_sf["count"]) +
    geom_sf(data = pxl_zm)
  ggsave("figures/pxl_zm.png", width = tw, height = tw / 2)
}


# st_make_grid does not work
if (FALSE) {
  pxl_grid <- st_make_grid(pxl,
    n = c(x_pxl, ceiling(x_pxl * xdiff / ydiff)),
    square = TRUE
  ) %>%
    st_intersection(bnd)
  #   ggplot() + geom_sf(data = pxl_grid)
  # ggsave("pxl_grid.pdf")
}
