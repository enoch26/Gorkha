# https://gis.stackexchange.com/questions/358797/splitting-overlap-between-polygons-and-assign-to-nearest-polygon-using-r
st_no_overlap <- function(polygons) {
  
  centroids <- polygons %>% st_centroid
  
  # Voronoi tesselation
  voronoi <- 
    centroids %>% 
    st_geometry() %>%
    st_union() %>%
    st_voronoi() %>%
    st_collection_extract()
  
  # Put them back in their original order
  voronoi <-
    voronoi[unlist(st_intersects(centroids,voronoi))]
  
  # Keep the attributes
  result <- centroids
  
  # Intersect voronoi zones with buffer zones
  st_geometry(result) <-
    mapply(function(x,y) st_intersection(x,y),
           #st_buffer(st_geometry(centroids),dist), 
           polygons$geometry,
           voronoi,
           SIMPLIFY=FALSE) %>%
    st_sfc(crs=st_crs(centroids))
  
  result
}

nepal_geo_no_overlap <- st_no_overlap(nepal_geo_ex) %>% 
  st_difference(st_union(nepal_geo)) %>% 
  st_intersection(bnd_out) %>% st_collection_extract("POLYGON")
p_geo_no_overlap <- ggplot() +
  geom_sf(data = nepal_geo, aes(fill = ROCK_TYPES)) +
  geom_sf(data = nepal_geo_no_overlap, aes(fill = ROCK_TYPES), alpha = 0.1) 
p_geo_no_overlap 



# resolve overlapping polygon 
parts <- st_cast(st_union(nepal_geo_int),"POLYGON")
plot(parts)
st_intersects(nepal_geo_int, parts)