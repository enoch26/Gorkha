library(patchwork)
CV_thin <- FALSE; CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
source("read_data.R")

landslides_c_ <- st_intersection(landslides_c, landcover)
landslides_c_ <- landslides_c_[!is.na(landslides_c_$CODE1),]
df <- data.frame(
  Area_m2 = landslides_c_$Area_m2,
  logarea_m2 = log10(landslides_c_$Area_m2), 
  landcover = landslides_c_$CODE1
)
ggplot(df, aes(x = logarea_m2, fill = landcover)) +
  geom_histogram() +
  labs(y="landslides count", x = expression(paste(log[10]," area (", m^{2},")"))) +
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


bb <- sf::st_bbox(bnd)

x1 <- bb["xmin"] + 0.65 * (bb["xmax"] - bb["xmin"])
x2 <- bb["xmin"] + 0.998 * (bb["xmax"] - bb["xmin"])
y1 <- bb["ymin"] + 0.71 * (bb["ymax"] - bb["ymin"])
y2 <- bb["ymin"] + 0.998 * (bb["ymax"] - bb["ymin"])


# fault -------------------------------------------------------------------

if(FALSE){
faults78 <- st_read("data/Fault Lines of Hindu Kush Himalayan (HKH) Region/data/fault.shp")
faults78 <- st_transform(faults78, st_crs(landslides_c))

# bounding box of bnd as polygon
bb_poly <- st_as_sfc(st_bbox(bnd))
bb_poly <- st_set_crs(bb_poly, st_crs(bnd))

# keep only faults within / intersecting the bounding box
faults78_bb <- st_intersection(faults78, bb_poly)
}

sutures <- st_read("data/HimaTibetMap-master/HimaTibetMap-master/arc/sutures.shp")
volcanic_rocks <- st_read("data/HimaTibetMap-master/HimaTibetMap-master/arc/volcanic_rocks.shp")
faults <- st_read("data/HimaTibetMap-master/HimaTibetMap-master/arc/HimaTibetMap.shp")

# transform all to the CRS of landslides_c
faults <- st_transform(faults, st_crs(landslides_c))
sutures <- st_transform(sutures, st_crs(landslides_c))
volcanic_rocks <- st_transform(volcanic_rocks, st_crs(landslides_c))

# bounding box of bnd as polygon
bb_poly <- st_as_sfc(st_bbox(bnd))
bb_poly <- st_set_crs(bb_poly, st_crs(bnd))

# keep only features intersecting the bounding box
faults_bb <- st_intersection(faults, bb_poly)
sutures_bb <- st_intersection(sutures, bb_poly)
volcanic_rocks_bb <- st_intersection(volcanic_rocks, bb_poly)

# plot --------------------------------------------------------------------



p1 <- ggplot() +
  geom_spatraster_rgb(data = tile) +
  gg(data = bnd, col = "red", fill = "transparent", lwd = 0.5) +
  guides(alpha = "none") +
  geom_sf(data = landslides_, col = "red", size = 0.05, aes(alpha = log10(Area_m2))) +
  geom_spatraster_contour_text(
    data = pga_mean_raster$pga_mean_exp %>% crop(bnd, mask = TRUE),
    breaks = seq(.1, .9, .15),
    color = "white"
  ) +
  geom_sf(data = st_as_sf(epic)[1, ], col = "yellow", fill = NA, shape = 2, size = 3) +
  geom_sf(data = faults_bb, colour = "purple", linewidth = 0.7) +
  annotate(
    "rect",
    xmin = x1, xmax = x2,
    ymin = y1, ymax = y2,
    fill = scales::alpha("white", 1),
    colour = NA
  ) +
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.4, "in"),
    pad_y = unit(0.9, "in"),

    text_col = "black",
    line_col = "black"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.45, "in"),
    pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_col = "black",
      line_col = "black",
      fill = c("white", "black")
    )
  )
  ggsave("figures/tile_ldsize_pga_exp_mw78.jpg", plot = p1, width = tw/2, height = tw/4, type = "cairo", dpi = 200);
  ggsave("figures/tile_ldsize_pga_exp_mw78.png", plot = p1, width = tw/2, height = tw/4, device = ragg::agg_png, dpi = 200);
  ggsave("figures/tile_ldsize_pga_exp_mw78.pdf", plot = p1, width = tw/2, height = tw/4)


p2 <- ggplot() + geom_spatraster_rgb(data = tile) + 
  # gg(data = bnd, col = "blue", fill= "transparent", lwd = 0.5) + guides(alpha = "none") +
  gg(data = bnd, col = "red", fill= "transparent", lwd = 0.5) + guides(alpha = "none") +
  geom_sf(data = landslides_, col = "red",  size = 0.05, aes(alpha = log10(Area_m2))) +
  geom_spatraster_contour_text(data = pga_mean_raster_mw73$pga_mean_exp %>% 
                                 crop(bnd, mask = TRUE), 
                               breaks = seq(.15, .55, .1),
                               # breaks = seq(.15, .85, .1),
                               color = "white") +
  # geom_sf(data = st_as_sf(epic)[1,] , col = "green", fill = NA, shape = 2, size = 0.75) +
  geom_sf(data = st_as_sf(epic)[2,] , col = "yellow", fill = NA, shape = 2, size = 3) +
  annotate(
    "rect",
    xmin = x1, xmax = x2,
    ymin = y1, ymax = y2,
    fill = scales::alpha("white", 1),
    colour = NA
  ) +
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.4, "in"),
    pad_y = unit(0.9, "in"),
    
    text_col = "black",
    line_col = "black"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.45, "in"),
    pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_col = "black",
      line_col = "black",
      fill = c("white", "black")
    )
  )

ggsave("figures/tile_ldsize_pga_exp_mw73.png", plot = p2, width = tw/2, height = tw/4, device = ragg::agg_png, dpi = 200)
ggsave("figures/tile_ldsize_pga_exp_mw73.jpg", plot = p2, width = tw/2, height = tw/4, type = "cairo", dpi = 200)
ggsave("figures/tile_ldsize_pga_exp_mw73.pdf", plot = p2, width = tw/2, height = tw/4)


p3 <-   ggplot(df_, aes(x = logarea_m2, fill = land_cover)) +
  geom_histogram() + 
  labs(y="landslides count", x = expression(paste(log[10], " area (", m^{2},")"))) +
  guides(fill = "none") + 
  facet_wrap(vars(land_cover), scales = "free_y", ncol = 2) +
  theme(axis.text = element_text(size = 12), 
        axis.text.x = element_text( size = 12 ),
        axis.text.y = element_text( size = 12 ),
        axis.title = element_text( size = 12),
        strip.text = element_text(size=12))

p3 <-   ggplot(df_, aes(x = logarea_m2, fill = land_cover)) +
  geom_histogram() + 
  labs(y="landslides count", x = expression(paste(log[10], " area (", m^{2},")"))) +
  guides(fill = "none") + 
  facet_wrap(vars(land_cover), scales = "free_y", ncol = 4) +
  theme(axis.text = element_text(size = 12), 
        axis.text.x = element_text( size = 12 ),
        axis.text.y = element_text( size = 12 ),
        axis.title = element_text( size = 12),
        strip.text = element_text(size=12))
p3;
# ggsave("figures/landcover_log_hist_fw.png", width = tw/4, height = tw / 3, device = ragg::agg_png);
# ggsave("figures/landcover_log_hist_fw.jpg", width = tw/2, height = tw / 8, type = "cairo", dpi=300);
ggsave("figures/landcover_log_hist_fw.pdf", width = tw/1.25, height = tw/6)


# patchwork <- (p1/p2)|p3  
# patchwork <- (p1 | p2)/p3
patchwork <- (p1 / p2 / p3) 
  # theme(legend.position = "bottom") &
patchwork + 
  # plot_layout(widths = c(2, 1)) +
  plot_layout(heights = c(1.4, 1.4, 1)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm")) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16))
ggsave("figures/tile_ldsize_pga_exp_mw78_mw73_v.png", width = tw*.6, height = tw*.9, dpi = 100)
