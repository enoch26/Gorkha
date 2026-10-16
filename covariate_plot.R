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



# 3panel ------------------------------------------------------------------


library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(ggrepel)
library(maptiles)
library(patchwork)

# ============================================================
# KSN
# ============================================================
log_ksn_tag$log10ksn <-
  log_ksn_tag$cop30dem_channel_tagged_pixels / log(10)

ksn_ll <- terra::project(
  log_ksn_tag[["cop30dem_channel_tagged_pixels"]],
  "EPSG:4326"
)

# ============================================================
# RF2CH
# ============================================================
rf2ch_ll <- terra::project(
  rf2ch[["rf2ch_km"]],
  "EPSG:4326"
)

# ============================================================
# Common extent
# ============================================================
e <- terra::ext(
  85.30, 85.75,
  28.10, 28.40
)

ksn_crop <- terra::crop(ksn_ll, e)
rf2ch_crop <- terra::crop(rf2ch_ll, e)

# ============================================================
# Satellite imagery
# ============================================================
bbox <- st_as_sfc(
  st_bbox(
    c(
      xmin = 85.30,
      xmax = 85.75,
      ymin = 28.10,
      ymax = 28.40
    ),
    crs = 4326
  )
)

sat <- get_tiles(
  bbox,
  provider = "Esri.WorldImagery",
  crop = TRUE,
  zoom = 11
)

# ============================================================
# Locations
# ============================================================
pts <- data.frame(
  name = c(
    "Flood source",
    "Gyirong landslide",
    "Langtang Lirung",
    "Tsangbu Ri"
  ),
  lon = c(
    85.3359391438612,
    85.37852215939111,
    85.561,
    85.684
  ),
  lat = c(
    28.166030041941756,
    28.280142886122167,
    28.211,
    28.353
  )
)

pts_sf <- st_as_sf(
  pts,
  coords = c("lon", "lat"),
  crs = 4326
)

flood_sf <- pts_sf[pts_sf$name == "Flood source", ]
landslide_sf <- pts_sf[pts_sf$name == "Gyirong landslide", ]
peaks_sf <- pts_sf[
  pts_sf$name %in% c(
    "Langtang Lirung",
    "Tsangbu Ri"
  ),
]

# ============================================================
# SATELLITE PANEL
# ============================================================
p_sat <- ggplot() +
  geom_spatraster_rgb(data = sat) +
  geom_sf(
    data = flood_sf,
    shape = 24,
    fill = "red3",
    colour = "white",
    stroke = 0.4,
    size = 1.4
  ) +
  geom_sf(
    data = landslide_sf,
    shape = 24,
    fill = "darkorange",
    colour = "white",
    stroke = 0.4,
    size = 1.4
  ) +
  geom_sf(
    data = peaks_sf,
    shape = 17,
    colour = "white",
    size = 2.0
  ) +
  geom_text_repel(
    data = cbind(
      st_coordinates(pts_sf),
      st_drop_geometry(pts_sf)
    ),
    aes(X, Y, label = name),
    colour = "white",
    size = 2.8,
    segment.color = "white",
    box.padding = 0.25,
    point.padding = 0.15,
    seed = 123
  ) +
  coord_sf(
    xlim = c(85.30, 85.75),
    ylim = c(28.10, 28.40),
    expand = FALSE
  ) +
  labs(title = "(a) Satellite imagery") +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

# ============================================================
# KSN PANEL
# ============================================================
p_ksn <- ggplot() +
  geom_spatraster(
    data = ksn_crop,
    aes(fill = cop30dem_channel_tagged_pixels)
  ) +
  geom_sf(
    data = flood_sf,
    shape = 24,
    fill = "red3",
    colour = "black",
    size = 1.4,
    stroke = 0.4
  ) +
  geom_sf(
    data = landslide_sf,
    shape = 24,
    fill = "darkorange",
    colour = "black",
    size = 1.4,
    stroke = 0.4
  ) +
  geom_sf(
    data = peaks_sf,
    shape = 17,
    colour = "black",
    size = 2.0
  ) +
  geom_text_repel(
    data = cbind(
      st_coordinates(pts_sf),
      st_drop_geometry(pts_sf)
    ),
    aes(X, Y, label = name),
    size = 2.8,
    box.padding = 0.25,
    point.padding = 0.15,
    segment.size = 0.25,
    seed = 123
  ) +
  coord_sf(
    xlim = c(85.30, 85.75),
    ylim = c(28.10, 28.40),
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    option = "magma",
    name = expression(k[sn])
    # trans = "sqrt"
  ) +
  # scale_fill_viridis_c(
  #   name = expression(k[sn])
  # ) +
  labs(title = "(b) Channel steepness") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

# ============================================================
# RF2CH PANEL
# ============================================================
p_rf2ch <- ggplot() +
  geom_spatraster(
    data = rf2ch_crop,
    aes(fill = rf2ch_km)
  ) +
  geom_sf(
    data = flood_sf,
    shape = 24,
    fill = "red3",
    colour = "black",
    size = 1.4,
    stroke = 0.4
  ) +
  geom_sf(
    data = landslide_sf,
    shape = 24,
    fill = "darkorange",
    colour = "black",
    size = 1.4,
    stroke = 0.4
  ) +
  geom_sf(
    data = peaks_sf,
    shape = 17,
    colour = "black",
    size = 2.0
  ) +
  geom_text_repel(
    data = cbind(
      st_coordinates(pts_sf),
      st_drop_geometry(pts_sf)
    ),
    aes(X, Y, label = name),
    size = 2.8,
    box.padding = 0.25,
    point.padding = 0.15,
    segment.size = 0.25,
    seed = 123
  ) +
  coord_sf(
    xlim = c(85.30, 85.75),
    ylim = c(28.10, 28.40),
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    option = "cividis",
    name = "rf2ch (km)",
    trans = "sqrt"
  ) + 
  # scale_fill_viridis_c(
  #   name = "rf2ch (km)"
  # ) +
  labs(title = "(c) Relief to channel") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    )
  )

if(TRUE){
  p_rf2ch <- p_rf2ch +
    geom_text_repel(
      data = cbind(
        st_coordinates(pts_sf),
        st_drop_geometry(pts_sf)
      ),
      aes(X, Y, label = name),
      colour = "white",
      size = 2.8,
      box.padding = 0.25,
      point.padding = 0.15,
      segment.color = "white",
      segment.size = 0.25,
      seed = 123
    )
}

# ============================================================
# 3-PANEL FIGURE
# ============================================================
p3 <- p_sat + p_ksn + p_rf2ch +
  plot_layout(ncol = 3)

p3

# ============================================================
# SAVE
# ============================================================
ggsave(
  "figures/nepal_flash_flood_3panel.pdf",
  plot = p3,
  width = 15,
  height = 5
)

ggsave(
  "figures/nepal_flash_flood_3panel.png",
  plot = p3,
  width = 15,
  height = 5,
  dpi = 600
)