# hexagon tiling CV -------------------------------------------------------


a <- st_as_sf(st_make_grid(fm_nonconvex_hull(bnd), cellsize = 50, square = FALSE))
a$id <- 1:nrow(a)

# TODO
a_c <- st_centroid(a)
for(i in c(3,6,9)){
  if( x == st_bbox(a_c)$xmin + cellsize/2 * (i)){id <- 1}
  if( x == st_bbox(a_c)$xmin + cellsize/2 * (i+1)){id <- 2}
  if( x == st_bbox(a_c)$xmin + cellsize/2 * (i+2)){id <- 3}
}


ggplot(a) + geom_sf() + geom_sf_text(aes(label = id)) 

bnd_grid <- st_intersection(st_make_grid(fm_nonconvex_hull(bnd), cellsize = 10, square = FALSE), bnd)
# https://en.wikipedia.org/wiki/Hexagonal_tiling
# p6m, (*632)



bnd_grid_sf <- st_as_sf(st_as_sf(bnd_grid) %>% filter(st_geometry_type(.)
                                                      %in% c("POLYGON")))

ggplot() + geom_sf(data = bnd_grid_sf) + geom_sf(data = bnd, col = "red", fill = NA)
ggsave("bnd_grid.pdf")
# hexagon grid
bnd_grid$count <- lengths(st_intersects(bnd_grid, landslides_c_test))
# or the mesh
bnd_mesh <- st_intersection(fm_as_sfc(mesh_fm), bnd)
# bnd_mesh_ <- st_as_sf(bnd_mesh)
bnd_mesh$count <- lengths(st_intersects(st_as_sfc(bnd_mesh), landslides_c_test))