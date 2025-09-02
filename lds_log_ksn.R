# This code is to remove landslides in glacier landscape because lsdtopotools needs extra handling for these landslides
# BUT IT IS NO LONGER NEEDED
# source mchi_zm.R
# 2228

basin_zm <- basin %>% filter(cop30dem_AllBasins == 2228)
basin_zm_sf <- as_sf(as.polygons(basin_zm))

basin_ <-  basin %>% crop(bnd_out, mask = TRUE) 

ksn_tag <- rast(here("data", "ksn_tag_near.tif")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd_out, mask = TRUE) %>%
  clamp(1, values = TRUE)
log_ksn_tag <- log(ksn_tag$cop30dem_channel_tagged_pixels)

log_ksn_tag_zm <- log_ksn_tag %>% crop(basin_zm_sf)

ksn_tag_ori <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels_near.tif")) %>%
  # ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd_out, mask = TRUE) %>%
  clamp(1, values = TRUE)
log_ksn_tag_ori <- log(ksn_tag_ori$cop30dem_channel_tagged_pixels)
log_ksn_tag_ori_zm <- log_ksn_tag_ori %>% crop(basin_zm_sf)

landslides_c$log_ksn_ori <- extract(log_ksn_tag_ori, vect(st_geometry(landslides_c)), ID = FALSE)
# landslides_c_glacier <- (landslides_c[landslides_c$log_ksn < 3.125,])
# > nrow(landslides_c_glacier)
# [1] 108

# landslides_c_glacier <- (landslides_c[landslides_c$log_ksn < 2.5,])
# > nrow(landslides_c_glacier)
# [1] 95

landslides_c_glacier <- (landslides_c[landslides_c$log_ksn_ori < 1.5,])
nrow(landslides_c_glacier)

bnd_zm <- st_intersection(bnd, basin_zm_sf)

# tile <- maptiles::get_tiles(st_as_sfc(bnd_out), provider = "Esri.WorldImagery", crop = TRUE, zoom = 13) 
tile <- maptiles::get_tiles(st_as_sfc(bnd_out), provider = "Esri.WorldImagery", crop = TRUE, zoom = 13) 

ggplot() + geom_spatraster_rgb(data = tile) + 
  geom_sf(data = landslides, col = "red",  size = 0.00005, aes(alpha = log(Area_m2))) +
  gg(data = bnd, col = "red", fill= "transparent") + guides(alpha = "none")+
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))

ggsave("figures/tile_ldsize.pdf", width = tw/2, height = tw/4)
ggsave("figures/tile_ldsize.png", width = tw/2, height = tw/4, dpi = 100)

ggplot() + geom_spatraster_rgb(data = tile) + 
  geom_sf(data = landslides, col = "red",  size = 0.00005, aes(alpha = log(Area_m2))) +
  geom_spatraster_contour_text(data = pga_mean_raster$pga_mean_exp %>% 
                                 crop(bnd, mask = TRUE), 
                               breaks = seq(.1, .9, .15),
                               # breaks = seq(.15, .85, .1),
                          color = "white") +
  gg(data = bnd, col = "red", fill= "transparent") + guides(alpha = "none") +
  geom_sf(data = st_as_sf(epic)[1,] , col = "green", fill = NA, shape = 2, size = 1) +
  # geom_sf(data = st_as_sf(epic)[2,] , col = "yellow", fill = NA, shape = 2, size = 0.75) +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
ggsave("figures/tile_ldsize_pga_exp_mw78.pdf", width = tw/2, height = tw/4)
ggsave("figures/tile_ldsize_pga_exp_mw78.png", width = tw/2, height = tw/4, dpi = 150)


ggplot() + geom_spatraster_rgb(data = tile) + 
  geom_sf(data = landslides, col = "red",  size = 0.00005, aes(alpha = log(Area_m2))) +
  geom_spatraster_contour_text(data = pga_mean_raster_mw73$pga_mean_exp %>% 
                                 crop(bnd, mask = TRUE), 
                               breaks = seq(.15, .55, .1),
                               # breaks = seq(.15, .85, .1),
                               color = "white") +
  gg(data = bnd, col = "red", fill= "transparent") + guides(alpha = "none") +
  # geom_sf(data = st_as_sf(epic)[1,] , col = "green", fill = NA, shape = 2, size = 0.75) +
  geom_sf(data = st_as_sf(epic)[2,] , col = "yellow", fill = NA, shape = 2, size = 1) +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
ggsave("figures/tile_ldsize_pga_exp_mw73.pdf", width = tw/2, height = tw/4)
ggsave("figures/tile_ldsize_pga_exp_mw73.png", width = tw/2, height = tw/4, dpi = 150)

ggplot() + geom_spatraster_rgb(data = tile) + 
  geom_sf(data = landslides_c, col = "red",  size = 0.00005) +
  gg(data = bnd, col = "red", fill= "transparent") + guides(alpha = "none")
ggsave("figures/tile_lds.pdf", width = tw/2, height = tw/4)
ggsave("figures/tile_lds.png", width = tw/2, height = tw/4, dpi = 100)

ggplot() + gg(data = log_ksn_tag_ori) + gg(data = landslides_c_glacier, col = "red", size = 0.01) + scale_fill_viridis_c(na.value = "transparent") 
ggsave("figures/landslides_c_glacier.pdf", width = tw, height = tw/2)
ggplot() + geom_spatraster_rgb(data = tile) + gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
  gg(data = bnd_out, col = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent") 
ggsave("figures/landslides_c_glacier_tile.png", width = tw, height = tw/2, dpi = 100)
ggsave("figures/landslides_c_glacier_tile.pdf", width = tw, height = tw/2)




# zoom in -----------------------------------------------------------------


# TODO https://sesync-ci.github.io/blog/mapping-with-Mapbox.html
tile_ <- maptiles::get_tiles(st_as_sfc(basin_zm_sf), provider = "Esri.WorldImagery", crop = TRUE, zoom = 13) 
# tile_topo <- maptiles::get_tiles(st_as_sfc(basin_zm_sf), provider = "Esri.WorldTopoMap", crop = TRUE, zoom = 13) 
# tile_landscape <- maptiles::get_tiles(st_as_sfc(basin_zm_sf), provider = "Thunderforest.Landscape", crop = TRUE, zoom = 13) 
# tile_netgeo <- maptiles::get_tiles(st_as_sfc(basin_zm_sf), provider = "Esri.NatGeoWorldMap", crop = TRUE, zoom = 13) 

p3 <- ggplot() + geom_spatraster_rgb(data = tile_) + 
  gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
  gg(data = bnd_zm, col = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent") +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))

p3
ggsave("figures/landslides_c_glacier_tile_zm.pdf", width = tw/3, height = tw/2)

p1 <- ggplot() + gg(data = log_ksn_tag_ori_zm) +
  gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
  gg(data = bnd_zm, col = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent", name = expression(log_k[sn])) +
  # guides(fill = guide_legend("log k_sn")) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))

p1
ggsave("figures/landslides_c_glacier_zm.pdf", width = tw/3, height = tw/2)

p2 <- ggplot() + gg(data = log_ksn_tag_zm) +
  gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
  gg(data = bnd_zm, col = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent", name = expression(log_k[sn])) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))

p2
ggsave("figures/landslides_c_glacier_zm_fix.pdf", width = tw/3, height = tw/2)


p3 + p1 + p2 + plot_layout(widths = c(1, 1, 1),guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
ggsave("figures/landslides_c_glacier_zm_all.png", width = tw, height = tw/2, dpi = 100)
ggsave("figures/landslides_c_glacier_zm_all.pdf", width = tw, height = tw/2)

# ggspatial::annotation_scale(location = "br") +
#   ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))


# ggplot() + geom_spatraster_rgb(data = tile_topo) + 
#   gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
#   scale_fill_viridis_c(na.value = "transparent") 
# ggsave("figures/landslides_c_glacier_topo_zm.pdf", width = tw, height = tw/2)
# ggplot() + geom_spatraster_rgb(data = tile_terrain) + 
#   gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
#   # gg(data = bnd_out, col = "red", fill = "transparent") +
#   scale_fill_viridis_c(na.value = "transparent") 
# ggsave("figures/landslides_c_glacier_terrain_zm.pdf", width = tw, height = tw/2)
# ggplot() + geom_spatraster_rgb(data = tile_netgeo) + 
#   gg(data = landslides_c_glacier, col = "red", size = 0.01) + 
#   # gg(data = bnd_out, col = "red", fill = "transparent") +
#   scale_fill_viridis_c(na.value = "transparent") 
# ggsave("figures/landslides_c_glacier_netgeo_zm.pdf", width = tw, height = tw/2)

