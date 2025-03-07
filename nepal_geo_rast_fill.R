# This does not work, the moving window does not work on the boundary
# nepal_geo_rast_ex <- extend(nepal_geo_rast, c(10,10))
# nepal_geo_rast_fill <- focal(nepal_geo_rast_ex, w = 9, fun = "mean",
#                              na.policy = "only", na.rm = T)
# writeRaster(nepal_geo_rast_fill, here("data", "nepal_geo_rast_fill.tif"), overwrite = TRUE)

# TODO st_difference to obtain the sf polygon difference at the boundary and use bru_fill_missing

# r <- rast("data/nepal_geo_rast_fill.tif")

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
  # TODO  to fix
  # for (i in c("ROCK_ID", "ROCK_TYPES", "LITHO_ID")){
  #   nepal_geo_fill_[[i]] <- bru_fill_missing(nepal_geo,
  #                                            nepal_geo_fill_,
  #                                            nepal_geo_fill_[[i]],
  #                                            layer = i)
  # }
  # nepal_geo_fill_ <- dplyr::bind_rows(nepal_geo, nepal_geo_fill_)
  ggplot() + geom_sf(data = nepal_geo_fill_, aes(fill = ROCK_TYPES)) + 
    geom_sf(data = bnd, col = "red", fill = NA) +
    scale_fill_viridis_d()
  ggsave("figures/nepal_geo_fill.pdf", width = tw, height = tw)

  st_write(nepal_geo_fill_, "data/nepal_geo_fill_.shp")
  
  # nepal_geo_rast$rock_fill <- bru_fill_missing(nepal_geo_rast, nepal_geo,
  #                                              values = nepal_geo$ROCK_TYPES)
  # nepal_geo_rast$rock_fill <- bru_fill_missing(nepal_geo_rast, st_as_sf(vect(nepal_geo)),
  #                                               values = st_as_sf(vect(nepal_geo))$ROCK_TYPES)
  
  
  # TODO fill sf polygon rather than raster and with the original sf polygon
  # nepal_geo_rast_ex <- extend(nepal_geo_rast, c(10,10))
  # nepal_geo_rast_df <- as.data.frame(nepal_geo_rast_ex,
  #                                    xy = TRUE, na.rm = FALSE)
  # nepal_geo_rast_sf <- st_as_sf(nepal_geo_rast_df, 
  #                       coords = c("x", "y"), crs = crs_nepal$input)
  # nepal_geo_rast_val <- bru_fill_missing(nepal_geo_rast,
  #   where = nepal_geo_rast_sf,
  #   nepal_geo_rast_sf$ROCK_TYPES
  # )
  # nepal_geo_rast_fill <- nepal_geo_rast_ex
  # values(nepal_geo_rast_fill) <- nepal_geo_rast_val
  # length(nepal_geo_rast_val)
  
  ## fill gap --------------------------------------------------------------------
  # fill nepal_geo bnd_out gap, not elegant but it is good enough 
  # nepal_geo_ex <- st_buffer(nepal_geo, 2) # lets say 1 km is good enough
  # nepal_geo_ex_ <- st_difference(nepal_geo_ex, st_union(nepal_geo))
  # nepal_geo_int <- nepal_geo_ex_ %>% st_intersection(bnd_out)
  # nepal_geo_int <- st_collection_extract(nepal_geo_int,"POLYGON")
  # # p_geo_int <- ggplot() +
  # #   geom_sf(data = nepal_geo_int, aes(fill = ROCK_TYPES), alpha = 0.1) 
  # # p_geo_int
  # nepal_geo_dis <- spatialEco::sf_dissolve(nepal_geo_int, "ROCK_TYPES")
  # nepal_geo_overlap <- st_intersection(nepal_geo_dis)
  # nepal_geo_overlap <- st_collection_extract(nepal_geo_dis, "POLYGON") # remove overlaps
  # st_geometry(nepal_geo_overlap) <- "geometry" # This is not great
  # 
  # # p_geo_overlap <- ggplot() +
  # #   geom_sf(data = nepal_geo_overlap, aes(fill = ROCK_TYPES))
  # 
  # nepal_geo <- dplyr::bind_rows(nepal_geo, nepal_geo_overlap)
  
  # p_geo_ <- ggplot() +
  #   geom_sf(data = nepal_geo, aes(fill = ROCK_TYPES)) +
  #   geom_sf(data = bnd, col = "red", fill = NA) +
  #   geom_sf(data = bnd_out, col = "red", fill = NA)
  # p_geo_
  # ggsave("figures/nepal_geo_buffer.pdf", width = tw, height = tw)
  
  # another option of geology
  # geo %<-% {st_read(
  #   here(
  #     "data",
  #     "Geology of Nepal", "data", "Geology.shp"
  #   )
  # ) %>%
  #     st_transform(crs = crs_nepal) %>% # to UTM
  #     st_intersection(bnd_out)
  # }