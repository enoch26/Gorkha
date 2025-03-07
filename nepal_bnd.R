nepal_nc %<-% {
  st_read(here(
    "data", "Local Unit",
    "local_unit.shp"
  )) %>%
    mutate(Province = stringr::str_replace(Province, "1", "Koshi")) %>%
    mutate(Province = stringr::str_replace(Province, "2", "Madhesh")) %>%
    mutate(Province = stringr::str_replace(Province, "5", "Lumbini")) %>%
    st_transform(crs = 32645) %>% # to UTM
    filter(Province %in% c("Koshi", "Bagmati", "Gandaki"))
}
crs_nepal <- fm_crs_set_lengthunit(fm_crs(nepal_nc), "km")
# set crs unit to km
nepal_nc <- nepal_nc %>% st_transform(crs = crs_nepal)

# data boundary box
# Valagussa and others (2021)
bbox_coords <- c(
  84.24265675600003, 27.383653885000058,
  86.69613706100006, 28.736201582000035
)
names(bbox_coords) <- c("xmin", "ymin", "xmax", "ymax")
landslides_bbox <- st_bbox(bbox_coords, crs = 4326) %>%
  st_transform(crs = crs_nepal)
# st_bbox(bbox_coords, crs = 4326) %>% st_as_sfc() %>% st_area()
# 36208604820 [m^2]

# Zhang and others (2016)
# https://www.sciencebase.gov/catalog/item/imap/5874a764e4b0a829a320bb3a
# bbox_coords_zhang <- c(
#   83.56403067945462, 27.451422357973854, 86.3577832466, 28.4712991961
# )
# names(bbox_coords_zhang) <- c("xmin", "ymin", "xmax", "ymax")
# landslides_bbox_zhang <- st_bbox(bbox_coords_zhang, crs = 4326) %>%
#   st_transform(crs = crs_nepal)
# st_bbox(bbox_coords_zhang, crs = 4326) %>% st_as_sfc() %>% st_area()
# 31118672300 [m^2]

# bnd should be the bbox if ignoring the administrative region
# TODO
nepal_bnd %<-% {
  st_cast(st_sf(data.frame(geometry = st_union(nepal_nc))), "POLYGON")
}

bnd <- st_as_sf(st_difference(
  st_as_sfc(landslides_bbox),
  st_difference(st_as_sfc(landslides_bbox), nepal_bnd)
))

bnd_out <- st_cast(
  st_sf(geometry = fm_nonconvex_hull(bnd, 2.5)),
  "POLYGON"
)

# within_idx  <- st_intersects(nepal_nc, st_as_sfc(landslides_bbox))
# nepal_nc <- nepal_nc[which(lengths(within_idx) != 0), ]
# bnd %<-% {
#   st_cast(st_sf(data.frame(geometry = st_union(nepal_nc))), "POLYGON")
# }

# another nepal shp
# nepal_muni %<-% {
#   st_read(here("data", "Municipality", "Municipality.shp")) %>%
#     st_transform(crs = crs_nepal) %>%
#     st_intersects(st_as_sfc(landslides_bbox))
# }
# bnd_muni %<-% {
#   st_cast(st_sf(data.frame(geometry = st_union(nepal_muni))), "POLYGON")
# }

# remove crs for covariate
# st_crs(nepal_nc) <- NA