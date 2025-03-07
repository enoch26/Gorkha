landuse  <-  {
  st_read(
    here(
      "data",
      "hima_lc_npl",
      "lc_nepal.shp"
    )
  ) %>%
    st_transform(crs = crs_nepal) %>% # to UTM
    st_intersection(bnd_out)
}

landuse_fill <- st_cast(st_difference(bnd, st_union(landuse$geometry)), 
                          "POLYGON")

st_geometry(landuse_fill) <- "geometry" # This is not great

landuse_fill_ <- dplyr::bind_rows(landuse_fill, landuse)

landuse_fill_ <- landuse_fill_ %>% select(CODE1)
landuse_fill_$CODE1 <- bru_fill_missing(landuse,
                                               landuse_fill_,
                                               landuse_fill_$CODE1,
                                               layer = "CODE1")

ggplot() + geom_sf(data = landuse_fill_, aes(fill = CODE1)) + 
  geom_sf(data = bnd, col = "red", fill = NA) +
  scale_fill_viridis_d()
ggsave("figures/landuse_fill.pdf", width = tw, height = tw)

st_write(landuse_fill_, "data/landuse_fill_.shp")

## fill gap --------------------------------------------------------------------
landuse %<-% {
  st_read(
    here(
      "data",
      "landuse_fill_.shp"
    )
  ) %>%
    st_transform(crs = crs_nepal) %>% # to UTM
    st_intersection(bnd_out)
}
# fill landuse bnd_out gap, not elegant but it is good enough 
landuse_ex <- st_buffer(landuse, 2) # lets say 1 km is good enough
landuse_ex_ <- st_difference(landuse_ex, st_union(landuse))
landuse_int <- landuse_ex_ %>% st_intersection(bnd_out)
landuse_int <- st_collection_extract(landuse_int,"POLYGON")
# p_geo_int <- ggplot() +
#   geom_sf(data = landuse_int, aes(fill = ROCK_TYPES), alpha = 0.1) 
# p_geo_int
landuse_dis <- spatialEco::sf_dissolve(landuse_int, "CODE1")
landuse_overlap <- st_intersection(landuse_dis)
landuse_overlap <- st_collection_extract(landuse_dis, "POLYGON") # remove overlaps
st_geometry(landuse_overlap) <- "geometry" # This is not great

# p_geo_overlap <- ggplot() +
#   geom_sf(data = landuse_overlap, aes(fill = CODE1))

landuse_ <- dplyr::bind_rows(landuse, landuse_overlap)

p_geo_ <- ggplot() +
  geom_sf(data = landuse_, aes(fill = CODE1)) +
  geom_sf(data = bnd, col = "red", fill = NA) +
  geom_sf(data = bnd_out, col = "red", fill = NA)
p_geo_
ggsave("figures/landuse_fill_bnd_out.pdf", width = tw, height = tw)
st_write(landuse_, "data/landuse_.shp")