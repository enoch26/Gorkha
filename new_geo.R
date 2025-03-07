############################### TODO 02/01/2025 separate higher and lower himalaya crystallines 
geo8apg <- st_read(
  here("data/geo8apg/geo8apg.shp")
) %>%
  st_transform(crs = crs_nepal) %>% # to UTM
  st_intersection(st_as_sfc(landslides_bbox))
st_intersection(bnd_out)

geo_gn <- geo %>% filter(CLASS == "Gn")

geo_hima <- geo %>% filter(CLASS == "Himal Group") 

nepal_geo_hima <- nepal_geo %>% 
  filter(ROCK_TYPES == "Higher Himalaya Crystallines") 
# Higher Himalaya Crystallines
geo_hima_int <- nepal_geo_hima %>% st_intersection(geo_hima)

ggplot() + geom_sf(data = nepal_geo_hima, fill = "grey") +
  # geom_sf(data = geo_hima, fill = "blue") + 
  geom_sf(data = geo_hima_int, fill = "red", alpha = 0.2) +
  geom_sf(data = geo_gn, fill = "green") 

geo_na <- geo %>% filter(CLASS == "No Data"| is.na(CLASS))


ggplot() + geom_sf(data = geo_na, aes(fill = CLASS))

geo_int <- geo_na %>% st_intersection(nepal_geo)
ggplot() + geom_sf(data = geo_int, aes(fill = CLASS)) 

#################################################