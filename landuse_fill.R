landcover  <-  {
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

landcover_fill <- st_cast(st_difference(bnd, st_union(landcover$geometry)), 
                          "POLYGON")

st_geometry(landcover_fill) <- "geometry" # This is not great

landcover_fill_ <- dplyr::bind_rows(landcover_fill, landcover)

landcover_fill_ <- landcover_fill_ %>% select(CODE1)
landcover_fill_$CODE1 <- bru_fill_missing(landcover,
                                               landcover_fill_,
                                               landcover_fill_$CODE1,
                                               layer = "CODE1")

ggplot() + geom_sf(data = landcover_fill_, aes(fill = CODE1)) + 
  geom_sf(data = bnd, col = "red", fill = NA) +
  scale_fill_viridis_d()
ggsave("figures/landcover_fill.pdf", width = tw, height = tw)

st_write(landcover_fill_, "data/landcover_fill_.shp")

## fill gap --------------------------------------------------------------------
landcover %<-% {
  st_read(
    here(
      "data",
      "landcover_fill_.shp"
    )
  ) %>%
    st_transform(crs = crs_nepal) %>% # to UTM
    st_intersection(bnd_out)
}
# fill landcover bnd_out gap, not elegant but it is good enough 
landcover_ex <- st_buffer(landcover, 2) # lets say 1 km is good enough
landcover_ex_ <- st_difference(landcover_ex, st_union(landcover))
landcover_int <- landcover_ex_ %>% st_intersection(bnd_out)
landcover_int <- st_collection_extract(landcover_int,"POLYGON")
# p_geo_int <- ggplot() +
#   geom_sf(data = landcover_int, aes(fill = ROCK_TYPES), alpha = 0.1) 
# p_geo_int
landcover_dis <- spatialEco::sf_dissolve(landcover_int, "CODE1")
landcover_overlap <- st_intersection(landcover_dis)
landcover_overlap <- st_collection_extract(landcover_dis, "POLYGON") # remove overlaps
st_geometry(landcover_overlap) <- "geometry" # This is not great

# p_geo_overlap <- ggplot() +
#   geom_sf(data = landcover_overlap, aes(fill = CODE1))

landcover_ <- dplyr::bind_rows(landcover, landcover_overlap)

p_geo_ <- ggplot() +
  geom_sf(data = landcover_, aes(fill = CODE1)) +
  geom_sf(data = bnd, col = "red", fill = NA) +
  geom_sf(data = bnd_out, col = "red", fill = NA)
p_geo_
ggsave("figures/landcover_fill_bnd_out.pdf", width = tw, height = tw)
st_write(landcover_, "data/landcover_.shp")