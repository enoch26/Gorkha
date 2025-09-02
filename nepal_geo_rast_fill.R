# bnd_out: from read_data.R
nepal_geo <- st_read(here("data", "Nep_Geo", "Geology", "Nep_geology.shp")) %>%
  st_transform(crs = crs_nepal) %>% st_intersection(bnd_out)
#   plot(r)
  
  # bru_fill_missing --------------------------------------------------------
  # TODO bru_fill_missing requires too muhc RAM another one of the option is to smooth
  
  # ggplot() + geom_sf(data = st_union((nepal_geo))) + 
  #   geom_sf(data = bnd, col = "red", fill = NA)
  
# bnd: from read_data.R

  nepal_geo_fill <- st_cast(st_difference(bnd, st_union(nepal_geo$geometry)), 
                            "POLYGON")
  st_geometry(nepal_geo_fill) <- "geometry" # This is not great
  ggplot() +
    geom_sf(data = nepal_geo_fill, fill = "green")  +
    geom_sf(data = nepal_geo, fill = "blue")+
    geom_sf(data = bnd, col = "red", fill = NA)
  
  nepal_geo_fill_ <- dplyr::bind_rows(nepal_geo_fill, nepal_geo)
  
  ggplot() + 
    geom_sf(data = nepal_geo_fill, fill="green") + 
    geom_sf(data = st_union(nepal_geo_fill_), fill="blue") +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  nepal_geo_fill_ <- nepal_geo_fill_ %>% select(ROCK_ID, ROCK_TYPES, LITHO_ID)
  nepal_geo_fill_$ROCK_TYPES <- bru_fill_missing(nepal_geo,
                                           nepal_geo_fill_,
                                           nepal_geo_fill_$ROCK_TYPES,
                                           layer = "ROCK_TYPES")

  ggplot() + geom_sf(data = nepal_geo_fill_, aes(fill = ROCK_TYPES)) + 
    geom_sf(data = bnd, col = "red", fill = NA) +
    scale_fill_viridis_d()
  ggsave("figures/nepal_geo_fill.pdf", width = tw, height = tw)

  st_write(nepal_geo_fill_, "data/nepal_geo_fill_.shp")
  
