concavity <- TRUE
# concavity <- TRUE
# find the basin id for zoomed area
# long <- 85.5
# lat <- 27.7
# long <- 85
# lat <- 27.6

if(FALSE){
  pts <- st_as_sf(data.frame(long = long, lat = lat),
    coords = c("long", "lat"),
    crs = 4326
  ) %>% st_transform(crs = crs_nepal)
  
  basin <- rast(here("data", "lsdtt", fdr, "cop30dem_AllBasins.bil")) %>%
    project(crs_nepal$input, threads = TRUE) %>%
    crop(bnd, mask = TRUE)
  basin_info <- extract(basin, pts)
  basin_zm <- basin %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  basin_zm_sf <- as_sf(as.polygons(basin_zm))
}

# run this
# 14982, 15329, 17757, 22048
# for (l in c(14982, 15329, 17757, 22048)){source("mchi_zm.R")}
# for (l in c(17757)){source("mchi_zm.R")}



# mchi_sf_zm --------------------------------------------------------------
  basin_zm <- basin %>% filter(cop30dem_AllBasins == l)
  # basin_zm <- basin %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  basin_zm_sf <- as_sf(as.polygons(basin_zm))
  mchi_sf_zm <- mchi_sf %>%
    st_intersection(st_as_sfc(basin_zm_sf))
  landslides_zm <- st_intersection(landslides_c, st_as_sfc(basin_zm_sf))
  landslides_poly_zm <- st_intersection(landslides, st_as_sfc(basin_zm_sf))
  
  # hs$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  # hs_zm <- crop(hs, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  # 
  # rf2ch$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  # rf2ch_zm <- crop(rf2ch, basin_zm_sf) %>% filter(cop30dem_AllBasins == l)
  # rf2ch_zm <- crop(rf2ch, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  # 
  # fd2ch$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  # fd2ch_zm <- crop(fd2ch, basin_zm_sf) %>% filter(cop30dem_AllBasins == l)
  # fd2ch_zm <- crop(fd2ch, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  
  ksn_tag$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  ksn_tag_zm <- crop(ksn_tag, basin_zm_sf) %>% filter(cop30dem_AllBasins == l)
  # ksn_tag_zm <- crop(ksn_tag, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  
  # with get_tiles
  # https://dieghernan.github.io/202205_tidyterra/
  tile <- maptiles::get_tiles(st_as_sfc(basin_zm_sf), provider = "Esri.WorldImagery", crop = TRUE, zoom = 13)
  ksn_tag_zm$log_cop30dem_channel_tagged_pixels <- log10(ksn_tag_zm$cop30dem_channel_tagged_pixels)
  mchi_sf_zm$log_m_chi <- log10(mchi_sf_zm$m_chi)

# find the nearest pairs
mchi_lds_pairs <- st_nearest_feature(landslides_zm, mchi_sf_zm)
landslides_zm$flow_distance <- mchi_sf_zm$flow_distance[mchi_lds_pairs]
landslides_zm$elevation <- mchi_sf_zm$elevation[mchi_lds_pairs]

rownames(landslides_zm) <- rownames(mchi_sf_zm) <- NULL
landslides_zm_df <- as.data.frame(landslides_zm)



if (to_plot) {




  # https://bookdown.org/brianwood1/QDASS/simple-static-maps.html
  if (to_plot) {
    ggplot() +
      geom_spatraster_rgb(data = tile) +
      gg(
        data = mchi_sf_zm, aes(color = log10(m_chi)),
        # geom = "tile",
        # alpha = .5,
        size = 0.5
      ) +
      geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
      geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5) +
      scale_color_viridis_c(name = expression(log[10] ~ k[sn])) +
      ggspatial::annotation_scale(location = "br") +
      ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
    # ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_basin_zm_tile.pdf"), width = tw, height = tw / 2)
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tile.png"), width = tw/2, height = tw / 4, dpi = 100)
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tile.pdf"), width = tw/2, height = tw / 4)

    ggplot() +
      # geom_spatraster_rgb(data = tile) +
      geom_spatraster(
        data = ksn_tag_zm$log_cop30dem_channel_tagged_pixels,
        # alpha = .7,
        maxcell = 5e+06
      ) +
      # scale_fill_grass_c(palette = "celsius")
      scale_fill_grass_c(na.value = "transparent", palette = "viridis", name = expression(log[10] ~ k[sn])) +
      # geom_sf(data = mchi_sf_zm, aes(color = log_m_chi),
      #    # geom = "tile",
      #    alpha = .5,
      #    size = 0.1) +
      geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
      geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5) +
      ggspatial::annotation_scale(location = "br") +
      ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))

    if(l == 15329){
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tag.pdf"), width = tw/3, height = tw /3)
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tag.png"), width = tw/3, height = tw /3, dpi = 100)
    } else{
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tag.pdf"), width = tw/2, height = tw / 4)
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tag.png"), width = tw/2, height = tw / 4, dpi = 100)
      
    }
    # ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_basin_zm_tag.pdf"), width = tw, height = tw / 2)

    ggplot() +
      geom_spatraster_rgb(data = tile) +
      gg(
        data = mchi_sf_zm, aes(
          color = log10(m_chi)
        ), # geom = "tile",
        size = 0.5
      ) +
      scale_color_viridis_c(name = expression(log[10] ~ k[sn])) +
      geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5) +
      # geom_sf(data = landslides_poly_zm, fill = "red", col = "red", size = 0.1, aes(alpha = log10(Area_m2))) +
      geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
      # guides(fill = "log(area)") +
      ggspatial::annotation_scale(location = "br") +
      ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
    # ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_basin_zm_tile_poly.pdf"), width = tw, height = tw / 2)
    if(l == 15329){
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tile_poly.png"), width = tw/3, height = tw /3, dpi = 100)
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tile_poly.pdf"), width = tw/3, height = tw /3)
    } else{
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tile_poly.pdf"), width = tw/1.5, height = tw /3)
      ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_zm_tile_poly.png"), width = tw/1.5, height = tw /3, dpi = 100)
    }
    }


  ## mchi analysis -----------------------------------------------------------
  # https://lsdtopotools.github.io/LSDTT_documentation/LSDTT_visualisation.html
  # https://lsdtopotools.github.io/LSDTT_documentation/LSDTT_chi_analysis.html

  if (to_plot) {
    ggplot() +
      geom_point(
        data = as.data.frame(mchi_sf_zm),
        aes(x = flow_distance, y = elevation, col = log10(m_chi)), size = 0.5
      ) +
      geom_point(
        data = landslides_zm_df,
        aes(x = flow_distance, y = elevation), col = "red", size = 0.5
      ) + xlab("Relative flow distance (m)") + ylab("Elevation (m)") +
      # theme(axis.text.y = element_blank(),
      #   axis.ticks = element_blank())
      scale_color_viridis_c(name = expression(log[10] ~ k[sn]))
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_basin_mchi_analysis.png"), width = tw/2, height = tw /4)
    # ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_basin_mchi_analysis.png"), width = tw/2, height = tw /4)
  }
}



# names(bbox_zm) <- c("xmin", "ymin", "xmax", "ymax")
# landslides_bbox <- st_bbox(bbox_zm, crs = 4326) %>%
#   st_transform(crs = crs_nepal)
#
# mchi_sf_zm <- mchi_sf %>%
#   st_intersection(st_as_sfc(bbox_zm))
#
# ggplot() + gg(data = mchi_sf_zm, aes(color = log(m_chi)),
#               # geom = "tile",
#               # alpha = .5,
#               size = 0.2) +
#   scale_color_viridis_c() +
#   geom_sf(data = bnd, col = "red", fill = NA)


if (FALSE) {
  # DEM
  dem <- rast(here("data", "lsdtt", fdr, "cop30dem.bil")) %>%
    project(crs_nepal$input)

  landslides_zm$elevation <- unlist(extract(dem, landslides_zm, ID = FALSE))
}

# another way of doing it but flow distance to channel is not equal to flow distance at mchi
if (FALSE) {
  fd2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_FDTOCHAN.bil")) %>%
    project(crs_nepal$input)
  landslides_zm$fd2ch_lds <- extract(fd2ch, landslides_zm)
  ggplot() +
    gg(data = 1 / exp(rf2ch_zm$rf2ch_km)) +
    scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
    geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
  ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_basin_near_rf2ch_zm.pdf"), width = tw, height = tw / 2)

  ggplot() +
    gg(data = 1 / exp(fd2ch_zm$fd2ch_km)) +
    scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
    geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
  ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_basin_near_fd2ch_zm.pdf"), width = tw, height = tw / 2)
}


# compare concavity index --------------------------------------------------
# annotation_custom
# https://stackoverflow.com/questions/72822935/add-solid-colour-behind-ggspatial-scale-bar-and-label
if (concavity) {
  mchi04 <- read.csv(here("data", "lsdtt", "lanczos_mn4", "cop30dem_MChiSegmented.csv"), header = TRUE)
  mchi05 <- read.csv(here("data", "lsdtt", fdr, "cop30dem_MChiSegmented.csv"), header = TRUE)
  mchi06 <- read.csv(here("data", "lsdtt", "lanczos_mn6", "cop30dem_MChiSegmented.csv"), header = TRUE)

  mchi_sf04 <- st_as_sf(mchi04, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal)
  mchi_sf05 <- st_as_sf(mchi05, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal)
  mchi_sf06 <- st_as_sf(mchi06, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal)

  # 14982, 15329, 17757, 22048
  for (j in c(14982, 15329, 17757, 22048)) {
    basin_zm <- basin %>% filter(cop30dem_AllBasins == j)
    basin_zm_sf <- as_sf(as.polygons(basin_zm))
    tile <- maptiles::get_tiles(st_as_sfc(basin_zm_sf), 
                                provider = "Esri.WorldImagery", crop = TRUE, zoom = 13)
    mchi_sf_zm04 <- mchi_sf04 %>%
      st_intersection(st_as_sfc(basin_zm_sf))
    mchi_sf_zm05 <- mchi_sf05 %>%
      st_intersection(st_as_sfc(basin_zm_sf))
    mchi_sf_zm06 <- mchi_sf06 %>%
      st_intersection(st_as_sfc(basin_zm_sf))
    rownames(mchi_sf_zm04) <- rownames(mchi_sf_zm05) <- rownames(mchi_sf_zm06) <- NULL
    mchi_nm_ls <- c("mchi_sf_zm04", "mchi_sf_zm05", "mchi_sf_zm06")
    mchi_ls <- list()

    for (i in seq_along(mchi_nm_ls)) {
      mchi_ls[[i]] <- ggplot() +
        geom_spatraster_rgb(data = tile) +
        gg(
          data = get(mchi_nm_ls[i]), aes(color = log10(m_chi)),
          # geom = "tile",
          # alpha = .5,
          size = 0.25
        ) +
        geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
        ggtitle(paste0("m_n=0.", i + 3)) +
        # geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5) +
        scale_color_viridis_c(name = expression(log[10] ~ k[sn])) +
        ggspatial::annotation_scale(location = "br") +
        ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
    }
    patchwork::wrap_plots(mchi_ls, guide = "collect", byrow = TRUE, ncol = 3) + patchwork::plot_annotation(tag_levels = 'A') &
      theme(legend.position = "bottom")
    ggsave(paste0("data/lsdtt/", j, "_", "mchi_mn_wrap.pdf"), width = tw, height = tw / 2)
    ggsave(paste0("data/lsdtt/", j, "_", "mchi_mn_wrap.png"), width = tw, height = tw / 2, dpi = 75)
    # ggsave(paste0("data/lsdtt/", j, "_", i, ".pdf"), width = tw, height = tw / 2)
  }
}

if (FALSE) {
  basin_zm <- basin %>% filter(cop30dem_AllBasins == 2228)
  basin_zm_sf <- as_sf(as.polygons(basin_zm))
  
  ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels_near.tif")) %>%
    # ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels.bil")) %>%
    project(crs_nepal$input, threads = TRUE) %>%
    crop(bnd_out, mask = TRUE) %>%
    clamp(1, values = TRUE)
  
  basin_ <- basin %>% crop(bnd_out, mask = TRUE)
  
  ksn_tag$cop30dem_AllBasins <- basin_$cop30dem_AllBasins
  ksn_tag_zm <- crop(ksn_tag, basin_zm_sf) %>% filter(cop30dem_AllBasins == 2228)
  
  ksn_bnd <- boundaries(ksn_tag_zm$cop30dem_channel_tagged_pixels)
  
  ggplot() +
    gg(data = log10(ksn_tag_zm$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  
  ggsave("ksn_tag_zm2228.pdf", width = tw, height = tw / 2)
  
  ksn_tag_zm_ <- ksn_tag_zm$cop30dem_channel_tagged_pixels %>% clamp(lower = exp(3), values = FALSE)
  
  ggplot() +
    gg(data = log10(ksn_tag_zm_$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  
  ggsave("ksn_tag_zm2228_.pdf", width = tw, height = tw / 2)
  
  ksn_tag_zm_near <- interpNear(ksn_tag_zm_, as.points(ksn_tag_zm_),
                                interpolate = FALSE, # from a geoscience perspective, it is better to use FALSE
                                radius = 2, # circle seems good enough
                                field = "cop30dem_channel_tagged_pixels"
                                # threads = FALSE # when interpolate = TRUE no way to control threads
  )
  
  ggplot() +
    gg(data = log10(ksn_tag_zm_near$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  
  ggsave("ksn_tag_zm2228_near.pdf", width = tw, height = tw / 2)
  
  writeRaster(ksn_tag_zm_near$cop30dem_channel_tagged_pixels, here("data", "lsdtt", fdr, "ksn_tag_zm_near2228.tif"), overwrite = TRUE)
  
  # https://rdrr.io/cran/terra/man/cover.html
  # TODO cover this one ksn_tag; have to make those fall in selected basin NA
  # https://dieghernan.github.io/tidyterra/reference/drop_na.Spat.html
  
  
  
  ksn_tag_ <- ksn_tag %>%
    mutate(
      cop30dem_channel_tagged_pixels =
        ifelse(cop30dem_AllBasins == 2228, NA, cop30dem_channel_tagged_pixels)
    )
  ggplot() +
    gg(data = log10(ksn_tag_$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave("ksn_tag_.pdf", width = tw, height = tw / 2)
  ksn_tag_zm_near <- rast(here("data", "lsdtt", fdr, "ksn_tag_zm_near2228.tif")) %>%
    project(crs_nepal$input, threads = TRUE)
  # %>% crop(ksn_bnd, mask = TRUE)
  ggplot() +
    gg(data = log10(ksn_tag_zm_near$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave("ksn_tag_near.pdf", width = tw, height = tw / 2)
  
  ksn_tag_ <- merge(ksn_tag_zm_near$cop30dem_channel_tagged_pixels, ksn_tag_$cop30dem_channel_tagged_pixels)
  ggplot() +
    gg(data = log10(ksn_tag_$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave("ksn_tag_merged.pdf", width = tw, height = tw / 2)
  
  
  ksn_tag_near <- interpNear(ksn_tag_, as.points(ksn_tag_),
                             interpolate = FALSE, # from a geoscience perspective, it is better to use FALSE
                             radius = 4, # circle seems good enough
                             field = "cop30dem_channel_tagged_pixels"
                             # threads = FALSE # when interpolate = TRUE no way to control threads
  )
  ggplot() +
    gg(data = log10(ksn_tag_near$cop30dem_channel_tagged_pixels)) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave("ksn_tag_near.pdf", width = tw, height = tw / 2)
  writeRaster(ksn_tag_near$cop30dem_channel_tagged_pixels, here("data", "ksn_tag_near.tif"), overwrite = TRUE)
}

# farridge < 0 ------------------------------------------------------------




if (FALSE) {
  rf2fr_zero <- rf2fr %>% filter(cop30dem_RELIEFTOFARRIDGE < 1e-10)
  rf2fr_zero$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  rf2fr_zero_zm <- crop(rf2fr_zero, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  fd2fr_zero <- fd2fr %>% filter(cop30dem_FDTOFARRIDGE < .1)
  fd2fr_zero$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  fd2fr_zero_zm <- crop(fd2fr_zero, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  fd2fr$cop30dem_AllBasins <- basin$cop30dem_AllBasins
  fd2fr_zm <- crop(fd2fr_zero, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
  ggplot() +
    geom_spatraster_rgb(data = tile, alpha = 0.5) +
    gg(
      data = mchi_sf_zm, color = "red",
      # geom = "tile",
      # alpha = .5,
      size = 0.1
    ) +
    scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
    geom_spatraster(data = fd2fr_zero_zm$cop30dem_FDTOFARRIDGE, maxcell = 5e+07) +
    scale_fill_continuous(na.value = "transparent") +
    ggspatial::annotation_scale(location = "br") +
    ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
  ggsave(paste0("data/lsdtt/", fdr, "/figure/fd2fr_zero_zm.pdf"), width = tw, height = tw / 2)
}

if (FALSE) {
  # with hillshade hs
  pal <- gray.colors(100, rev = TRUE)
  ggplot() +
    geom_spatraster(data = hs_zm, aes(fill = cop30dem_hs)) +
    scale_fill_gradientn(colours = pal, na.value = NA) +
    #   scale_fill_gradientn(colors = gray.colors(100,
    #   start = 0.2,
    #   end = .8, rev = TRUE, na.value = NA
    # )) +
    geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
    gg(
      data = mchi_sf_zm, aes(color = log10(m_chi)),
      # geom = "tile",
      # alpha = .5,
      size = 0.5
    ) +
    geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5) +
    labs(col= expression(log[10](k['sn']))) +
    scale_color_viridis_c()
  # ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(basin_info$cop30dem_AllBasins), "_/basin_zm_hs.pdf"), width = tw, height = tw / 2)
  ggsave(paste0("data/lsdtt/", fdr, "/figure/", as.character(l), "_/basin_zm_hs.pdf"), width = tw, height = tw / 2)
}