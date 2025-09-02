# visualisation -----------------------------------------------------------
# https://stackoverflow.com/questions/76683473/aesthetics-by-column-named-with-variable-how-to-use-embrace-operator-in-aesthet
# https://rlang.r-lib.org/reference/topic-inject.html#embracing-with-
# https://dieghernan.github.io/tidyterra/reference/scale_hypso.html
# https://dieghernan.github.io/tidyterra/articles/palettes.html
# https://dieghernan.github.io/tidyterra/reference/grass_db.html

# muni --------------------------------------------------------------------

if (to_plot) {
  ggplot() + gg(data = nepal_bnd) + geom_sf(data = bnd, 
                                            color = "red", fill = NA) +
    geom_sf(data = bnd_out, color = "red", fill = NA)
  ggsave(here("figures", "nepal_bnd_bbox.pdf"), width = tw, height = tw / 2)
  
  ggplot() +
    geom_sf(data = nepal_nc_, aes(fill = NA)) +
    geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
    geom_sf(data = bnd, col = "blue", fill = NA) +
    geom_sf(data = bnd_out, col = "blue", fill = NA) +
    theme(legend.position = "none")
  ggsave(here("figures", "nepal_bbox.pdf"), width = tw, height = tw / 2)
}

if (to_plot) {
  ggplot() +
    geom_sf(data = nepal_nc, aes(fill = NA)) +
    # geom_sf(data = nepal_muni, aes(fill = Mun_Name), alpha = 0.1) +
    theme(legend.position = "none")
  ggsave(here("figures", "nepal_nc_muni.pdf"), width = tw, height = tw / 2)
}

## pga ---------------------------------------------------------------------


if (to_plot) {
  p_pga <- ggplot() +
    geom_spatraster(data = pga_mean_raster, aes(fill = pga_mean)) +
    scale_fill_viridis_c() +
    labs(fill = "PGA") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1)
  ggsave(here("figures", "nepal_pga_mean.pdf"), width = tw, height = tw / 2)
}

## geology -----------------------------------------------------------------

if (to_plot) {
    ggplot() + geom_sf(data = landuse_8wp, aes(fill = CODE1)) +
      geom_sf(data = bnd, col = "red", fill = NA) +
      geom_sf(data = bnd_out, col = "red", fill = NA)
    ggsave("figures/model/landuse_8wp.pdf", width = tw, height = tw/2)     
  p_geo <- ggplot() +
    geom_sf(data = geology, aes(fill = ROCK_TYPES)) +
    labs(fill = "ROCK_TYPES") +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA)
  p_geo
  ggsave(here("figures", "nepal_geo_rocktype.pdf"), width = tw, height = tw / 2)

  ggplot()+
    geom_sf(data = geo8apg, aes(fill = as.factor(GEO8AG_)),
            show.legend = FALSE) + 
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA)
  ggsave(here("figures", "nepal_geo8pg.pdf"), width = tw, height = tw / 2)
  
  ggplot() +
    geom_spatraster(data = geology_rast, aes(fill = ROCK_TYPES)) +
    labs(fill = "ROCK_TYPES") +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA)
  ggsave(here("figures", "nepal_geo_rast.pdf"), width = tw, height = tw / 2)


  ggplot() +
    geom_sf(data = geology, aes(fill = LITHO_ID)) +
    labs(fill = "LITHO_ID") +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1) +
    geom_sf(data = bnd, col = "red", fill = NA)
  ggsave(here("figures", "nepal_litho.pdf"), width = tw, height = tw / 2)
}

# ksn ---------------------------------------------------------------------


p_ksn <- ggplot() +
  geom_spatraster(data = log_ksn_tag, aes(fill = cop30dem_channel_tagged_pixels)) +
  labs(fill = "log_ksn") +
  # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1) +
  geom_sf(data = bnd, col = "red", fill = NA) +
  scale_fill_viridis_c(na.value = "transparent") 
# geom_sf(data = bnd_out, col = "red", fill = NA)
p_ksn
ggsave(here("figures", "nepal_log_ksn.png"), width = tw/2, height = tw / 4)

## terrain -----------------------------------------------------------------

if (to_plot) {
  # https://dieghernan.github.io/tidyterra/articles/palettes.html
  # https://stackoverflow.com/questions/76683473/aesthetics-by-column-named-with-variable-how-to-use-embrace-operator-in-aesthet
  # crit <- c("slope","aspect")
  for(i in crit){
    ggplot() +
      geom_spatraster(data = dem_terrain_mask, aes_string(fill = i)) +
      # geom_spatraster(data = dem_terrain_mask, aes(fill = .data$i))
      scale_fill_grass_c(palette = i)
    ggsave(paste0(i, ".png"), width = tw, height = tw/2)
  }
  
  ggplot() + geom_spatraster(data = dem, aes(fill = dem_km)) + geom_sf(data = bnd_out, fill = NA, col = "red")
  ggsave("figures/dem.png", width = tw, height = tw/2)

  for (i in c("dem_terrain_focal", "dem_terrain_focal2")) {
    ggplot() +
      geom_spatraster(data = get(i), aes(fill = grad_easting)) +
      scale_fill_hypso_c() +
      labs(fill = "grad_easting") +
      geom_sf(data = bnd, col = "red", fill = NA)
    ggsave(paste0("figures/", i, "_grad_easting.pdf"), width = tw, height = tw / 2)

    ggplot() +
      geom_spatraster(data = get(i), aes(fill = grad_northing)) +
      scale_fill_hypso_c() +
      labs(fill = "grad_northing") +
      geom_sf(data = bnd, col = "red", fill = NA)
    ggsave(paste0("figures/", i, "_grad_northing.pdf"), width = tw, height = tw / 2)
  }
  # 124
  ggplot() +
    geom_spatraster(data = dem_terrain_focal, aes(fill = relief)) +
    scale_fill_hypso_c() +
    labs(fill = "relief") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA)
  ggsave(paste0("figures/", "relief.pdf"), width = tw, height = tw / 2)

  # 139
  ggplot() +
    geom_spatraster(data = dem_terrain_focal2, aes(fill = relief)) +
    scale_fill_hypso_c() +
    labs(fill = "relief") +
    geom_sf(data = bnd, col = "red", fill = NA)
  ggsave(paste0("figures/", "relief2.pdf"), width = tw, height = tw / 2)

  p_relief <- ggplot() +
    geom_spatraster(data = dem_terrain_focal, aes(fill = relief)) +
    scale_fill_hypso_c() +
    labs(fill = "relief") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA) 
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1)
  ggsave(paste0("figures/", "relief_lds.pdf"), width = tw, height = tw / 2)

  p_relief9x9 <- ggplot() +
    geom_spatraster(data = dem_terrain_focal2, aes(fill = relief)) +
    scale_fill_hypso_c() +
    labs(fill = "relief") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.1)
  ggsave(paste0("figures/", "relief_lds2.pdf"), width = tw, height = tw / 2)

  p_slope <- ggplot() +
    geom_spatraster(data = dem_terrain_mask, aes(fill = slope)) +
    labs(fill = "slope") +
    # scale_fill_grass_c(palette = "slope") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA) 
  p_slope
  ggsave(paste0("figures/", "nepal_slope.pdf"), width = tw, height = tw / 2)
  p_aspect <- ggplot() +
    geom_spatraster(data = dem_terrain_mask, aes(fill = aspect)) +
    labs(fill = "aspect") +
    # scale_fill_grass_c(palette = "aspect") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA) 
  p_aspect
  ggsave(paste0("figures/", "nepal_aspect.pdf"), width = tw, height = tw / 2)

  # ggplot() +
  #   geom_spatraster(data = dem_terrain_mask, aes(fill = aspect)) +
  #   labs(fill = "aspect") + scale_fill_grass_c(palette = "aspect")
  # + geom_sf(data = bnd, col = "red", fill = NA)
  # ggsave(paste0("figures/", "aspect.pdf"), width = tw, height = tw/2)

  ggplot() +
    geom_spatraster(data = dem_terrain_focal) +
    scale_fill_hypso_c() +
    geom_sf(data = bnd, col = "red", fill = NA) +
    facet_wrap(~lyr, ncol = 2)
  ggsave(here("figures", "dem_terrain_focal.pdf"), width = tw, height = tw)

  ggplot() +
    geom_spatraster(data = dem_terrain_mask)
  ggsave(here("figures", "nepal_dem_terrain_mask.pdf"), width = tw, height = tw)
  ggsave(here("figures", "nepal_dem_terrain_mask.png"), width = tw, height = tw)


  ggplot() +
    geom_spatraster(data = dem, aes(fill = dem_km)) +
    labs(fill='DEM') +
    # guides(fill=guide_legend(title="DEM")) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA) 
    
  ggsave(here("figures", "nepal_dem.png"), width = tw, height = tw/2)

  ggplot() +
    geom_spatraster(data = dem_terrain_mask, aes(fill = slope)) +
    labs(fill = "slope") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    scale_colour_viridis_c(na.value = "transparent")
  ggsave(here("figures", "nepal_slope.pdf"), width = tw, height = tw / 2)

  # ggplot() +
  #   geom_spatraster(data = dem_terrain_mask, aes(fill = roughness)) +
  #   labs(fill='rough') +
  #   geom_sf(data = bnd, col = "red", fill = NA) +
  #   scale_colour_viridis_c(na.value = "transparent")
  # ggsave(here("figures", "nepal_rough.pdf"), width = tw, height = tw/2)
  #
  # ggplot() +
  #   geom_spatraster(data = dem_terrain_mask, aes(fill = TRIriley)) +
  #   labs(fill='riley') +
  #   geom_sf(data = bnd, col = "red", fill = NA) +
  #   scale_colour_viridis_c(na.value = "transparent")
  # ggsave(here("figures", "nepal_riley.pdf"), width = tw, height = tw/2)
  #
  # ggplot() +
  #   geom_spatraster(data = dem_terrain_mask, aes(fill = TRIriley)) +
  #   labs(fill='riley') +
  #   geom_sf(data = bnd, col = "red", fill = NA) +
  #   scale_colour_viridis_c(na.value = "transparent")
  # ggsave(here("figures", "nepal_riley.pdf"), width = tw, height = tw/2)
}


# curvature ---------------------------------------------------------------

if (to_plot){
  p_crv_planform <- ggplot() +
    geom_spatraster(data = crv_planform, aes(fill = crv_planform)) +
    # scale_fill_viridis_c() +
    # scale_fill_hypso_c() +
        labs(fill = "crv_planform") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA) 
  p_crv_planform
  ggsave("figures/crv_planform.pdf", width = tw, height = tw/2)
  p_crv_profile <- ggplot() +
    geom_spatraster(data = crv_profile, aes(fill = crv_profile)) +
    # scale_fill_viridis_c() +
    # scale_fill_hypso_c() +
        labs(fill = "crv_profile") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = bnd_out, col = "red", fill = NA) 
    
  p_crv_profile
  ggsave("figures/crv_profile.pdf", width = tw, height = tw/2)
}

# plot --------------------------------------------------------------------


if (to_plot) {
  ggplot() +
    geom_sf(data = head(landslides, 1), aes(col = log(Area_m2)), size = 0.1)
  ggsave(here("figures", "landslidesx1.pdf"), width = tw, height = tw / 2)

  ggplot() +
    geom_sf(data = nepal_nc, aes(fill = Province)) +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1)
  ggsave(here("figures", "nepal_landslides.pdf"), width = tw, height = tw / 2)

  ggplot() +
    geom_sf(data = geo, aes(fill = CLASS)) +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1)
  ggsave(here("figures", "nepal_geo.pdf"), width = tw, height = tw / 2)

  p_landcover <- ggplot() +
    geom_sf(data = landcover, aes(fill = CODE1)) +
    geom_sf(data = bnd, col = "red", fill = NA) 
    # geom_sf(data = bnd_out, col = "red", fill = NA) 
    # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.5)
    p_landcover
  ggsave(here("figures", "nepal_landuse.pdf"), width = tw, height = tw / 2)
  ggsave(here("figures", "nepal_landuse.png"), width = tw/2, height = tw / 4)
  ggsave(here("figures", "nepal_landuse.jpg"), width = tw, height = tw / 2)
}


p_lst <- list(p_pga, p_relief, p_relief9x9, p_slope, p_geo, p_landuse)
wrap_plots(p_lst, ncol = 2)
ggsave(here("figures", "nepal_plots.pdf"), width = tw, height = tw)

# kernel ------------------------------------------------------------------
if (to_plot) {
  norm_kernel_stack <- c(rast(padzeros2d(norm_kernel, stack_times)),
                         rast(norm_kernel2) %>%  rename(lyr.2 = lyr.1))
  ggplot() +
    geom_spatraster(data = norm_kernel_stack) +
    scale_fill_viridis_c() +
    facet_wrap(~lyr)
  ggsave(here("figures", "norm_kernel_stack.pdf"), width = tw, height = tw / 2)
}

# pair --------------------------------------------------------------------

dem_stack <- c(dem, dem_terrain_mask, dem_terrain_focal)
# https://rdrr.io/github/rspatial/terra/man/layerCor.html
# https://rdrr.io/cran/terra/man/focalPairs.html
fp_dem <- focalPairs(dem_stack, w = 5, "pearson", na.rm = TRUE)
# plot(fp_dem)

fp_dem <- focalPairs(dem, w = 9, "pearson", na.rm = TRUE)
ggplot() +
  geom_spatraster(data = fp_dem) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c()
ggsave(here("figures", "pearson.pdf"), width = tw, height = tw / 2)

dem_layercor <- layerCor(dem_stack, "pearson")
sink("figures/nepal_layercor.txt")
dem_layercor
sink()

# not working and giving NULL for some reasons
# https://rspatial.github.io/terra/reference/pairs.html
pdf("figures/pairs.pdf")
dem_pairs <- terra::pairs(
  dem_stack,
  hist = TRUE,
  cor = TRUE,
  # use = "pairwise.complete.obs",
  use = "everything",
  maxcells = 100000
)
dem_pairs
dev.off()


ggsave(here("figures", "pairs.pdf"), width = tw, height = tw)
