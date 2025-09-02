library(patchwork)
CV_thin <- FALSE; CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
source("read_data.R")

landslides_c_ <- st_intersection(landslides_c, landcover)
landslides_c_ <- landslides_c_[!is.na(landslides_c_$CODE1),]
df <- data.frame(
  Area_m2 = landslides_c_$Area_m2,
  logarea_m2 = landslides_c_$logarea_m2, landcover = landslides_c_$CODE1
)
ggplot(df, aes(x = logarea_m2, fill = landcover)) +
  geom_histogram() +
  labs(y="landslides count", x = expression(paste("log area (", m^{2},")"))) +
  guides(fill=guide_legend(ncol=2))
# ggsave("figures/landcover_log_hist.png", width = tw, height = tw / 2)
ggsave("figures/landcover_log_hist.png", width = tw/2, height = tw / 5)

df_ <- df %>% mutate(land_cover = substr(landcover, 0, 1))
df_$land_cover <- factor(df_$land_cover, 
                         levels = c("1", "2", "6", "8"),
                         labels = c("1: Cultivated Crops", 
                                    "2: Natural Vegetation",
                                    "6: Rock/Gravel/Stones/Boulders",
                                    "8: Ice/Snow/Water bodies"))

tile <- maptiles::get_tiles(st_as_sfc(bnd_out), provider = "Esri.WorldImagery", crop = TRUE, zoom = 13) 

landslides_ <- st_intersection(landslides, bnd)

p1 <- ggplot() + geom_spatraster_rgb(data = tile) + 
  geom_sf(data = landslides_, col = "red",  size = 0.00005, aes(alpha = log(Area_m2))) +
  geom_spatraster_contour_text(data = pga_mean_raster$pga_mean_exp %>% 
                                 crop(bnd, mask = TRUE), 
                               breaks = seq(.1, .9, .15),
                               # breaks = seq(.15, .85, .1),
                               color = "white") +
  gg(data = bnd, col = "red", fill= "transparent", lwd = 0.5) + guides(alpha = "none") +
  geom_sf(data = st_as_sf(epic)[1,] , col = "yellow", fill = NA, shape = 2, size = 2) +
  # geom_sf(data = st_as_sf(epic)[2,] , col = "yellow", fill = NA, shape = 2, size = 0.75) +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
# ggsave("figures/tile_ldsize_pga_exp_mw7C8.pdf", width = tw/2, height = tw/4)
# ggsave("figures/tile_ldsize_pga_exp_mw78.png", width = tw/2, height = tw/4, dpi = 150)


p2 <- ggplot() + geom_spatraster_rgb(data = tile) + 
  geom_sf(data = landslides_, col = "red",  size = 0.00005, aes(alpha = log(Area_m2))) +
  geom_spatraster_contour_text(data = pga_mean_raster_mw73$pga_mean_exp %>% 
                                 crop(bnd, mask = TRUE), 
                               breaks = seq(.15, .55, .1),
                               # breaks = seq(.15, .85, .1),
                               color = "white") +
  gg(data = bnd, col = "red", fill= "transparent", lwd = 0.5) + guides(alpha = "none") +
  # geom_sf(data = st_as_sf(epic)[1,] , col = "green", fill = NA, shape = 2, size = 0.75) +
  geom_sf(data = st_as_sf(epic)[2,] , col = "yellow", fill = NA, shape = 2, size = 2) +
  ggspatial::annotation_scale(location = "tr") +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
# ggsave("figures/tile_ldsize_pga_exp_mw73.pdf", width = tw/2, height = tw/4)
# ggsave("figures/tile_ldsize_pga_exp_mw73.png", width = tw/2, height = tw/4, dpi = 150)


p3 <-   ggplot(df_, aes(x = logarea_m2, fill = land_cover)) +
  geom_histogram() + 
  labs(y="landslides count", x = expression(paste("log area (", m^{2},")"))) +
  guides(fill = "none") + 
  facet_wrap(vars(land_cover), scales = "free_y", ncol = 1)
# ggsave("figures/landcover_log_hist_fw.png", width = tw/4, height = tw / 3)


patchwork <- (p1/p2)|p3  
  # theme(legend.position = "bottom") &
patchwork + 
  # plot_layout(widths = c(2, 1)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 12))
ggsave("figures/tile_ldsize_pga_exp_mw78_mw73_.png", width = tw, height = tw/1.75, dpi = 100)
