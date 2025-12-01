CV_thin <- FALSE; CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
source("read_data.R")
pga_mean_raster$pga_mean_log10 <- log10(pga_mean_raster$pga_mean_exp)
ggplot() + 
  geom_spatraster(data = pga_mean_raster %>% crop(bnd, mask = TRUE), 
                  aes(fill = pga_mean_exp)) + 
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in")) +
  scale_fill_viridis_c(
    name = "PGA", 
    # option = "C",
    na.value = "transparent"  
  )
ggsave("figures/pga.pdf", width = 12, height = 8)

pga_std_raster <-
  rast(here(
    "data", "raster", "pga_std.flt"
  )) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)
ggplot() + geom_spatraster(data = pga_std_raster) +
  geom_sf(data = bnd, fill = NA, col = "red") +
  scale_fill_viridis_c(
    name = "log PGA sd", 
    # option = "C",
    na.value = "transparent"  
  )
ggsave("figures/pga_std.pdf", width = tw, height = tw / 2)

geology_ %<-% {
  st_read(here("data", "nepal_geo_fill_.shp")) %>%
    st_transform(crs = crs_nepal) %>%
    st_intersection(bnd)
}

ggplot() + geom_sf(data = geology_ %>% st_intersection(bnd), aes(fill = ROCK_TYPES)) + 
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))

ggsave("figures/geology.pdf", width = 12, height = 8)

ggplot() + geom_sf(data = landcover %>% st_intersection(bnd), aes(fill = as.factor(CODE1))) + 
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in")) +
  scale_fill_discrete(name = "Land cover")
ggsave("figures/landcover.png", width = 12, height = 8, dpi = 100)

log_ksn_tag$log10ksn <- log_ksn_tag$cop30dem_channel_tagged_pixels / log(10)
ggplot() + geom_spatraster(data = log_ksn_tag$log10ksn %>% 
                                 crop(bnd, mask = TRUE), 
                               aes(fill = log10ksn)) +
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in")) +
  scale_fill_viridis_c(
    name = expression(log[10] ~ k[sn]), 
    # option = "C",
    na.value = "transparent"  
  )
ggsave("figures/log_ksn.pdf", width = 12, height = 8)

ggplot() + geom_spatraster(data = rf2ch$rf2ch_km %>% 
                             crop(bnd, mask = TRUE), 
                           aes(fill = rf2ch_km)) +
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in")) +
  scale_fill_viridis_c(
    name = "rf2ch (km)", 
    na.value = "transparent"  
  )
ggsave("figures/rf2ch.pdf", width = 12, height = 8)

ggplot() + geom_spatraster(data = fd2ch$fd2ch_km %>% 
                             crop(bnd, mask = TRUE), 
                           aes(fill = fd2ch_km)) +
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in")) +
  scale_fill_viridis_c(
    name = "fd2ch (km)", 
    na.value = "transparent"  
  )
ggsave("figures/fd2ch.pdf", width = 12, height = 8)

ggplot() + geom_spatraster(data = rainfall$precip_2015 %>% 
                             crop(bnd, mask = TRUE), 
                           aes(fill = precip_2015)) +
  geom_sf(data = bnd, fill = NA, col = "red") +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in")) +
  scale_fill_viridis_c(
    name = "Rainfall (km)", 
    na.value = "transparent"  
  )
ggsave("figures/rainfall.pdf", width = 12, height = 8)


