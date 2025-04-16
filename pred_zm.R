# run after source("pred.R")
# TODO increase resolution fro the models
# https://r.geocompx.org/raster-vector
# https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.html
# https://dieghernan.r-universe.dev/articles/tidyterra/welcome.html

# Satellite Imagery
# https://sesync-ci.github.io/blog/mapping-with-Mapbox.html
# contour
bin <- 10
basin_idx <- c(14982, 2228)
# in log scale
logexp <- TRUE 

if(logexp) {
  nm_log <- "log"
}
basin <- rast(here("data", "lsdtt", fdr, "cop30dem_AllBasins.bil")) %>%
  project(crs_nepal$input)
# 2228
# TODO contour with ksn to show the landslides centroids

mod_nm <- ls(pattern = "fit")
pred_nm <- ls(pattern = "fp")
mod_names_a <- mod_nm[seq_along(mod_nm) %% 2 > 0]
mod_names_b <- mod_nm[seq_along(mod_nm) %% 2 == 0]
pred_names_a <- pred_nm[seq_along(pred_nm) %% 2 > 0]
pred_names_b <- pred_nm[seq_along(pred_nm) %% 2 == 0]

for (i in basin_idx) {
  basin_zm <- basin %>% filter(cop30dem_AllBasins == i)
  basin_zm_sf <- as_sf(as.polygons(basin_zm)) %>% st_intersection(bnd)
  landslides_poly_zm <- st_intersection(landslides, st_as_sfc(basin_zm_sf))
  landslides_c_zm <- st_intersection(landslides_c, st_as_sfc(basin_zm_sf))

  log_ksn_tag_zm <- log_ksn_tag %>% crop(basin_zm_sf)

  for (j in mod_names_a) {
    # browser()
    fp_zm <- (get(pred_names_a[j == mod_names_a]))$lambda["mean"] %>% st_intersection(basin_zm_sf)
    
    fp_zm_terra <- sf2sr(fp_zm["mean"], log_ksn_tag_zm, udpate = TRUE, cover = TRUE, touches = TRUE)

    # if(FALSE){
    # norm_kernel <- gkernel(matrix(1, 2, 2), norm = TRUE) # radius 3*30 / 2 = 45 m
    # stack_times <- 10 # TODO should be larger than mesh size, or do subdivide mesh into 9
    # for (k in 1:(stack_times)) {
    #   norm_kernel <- gkernel(norm_kernel, norm = TRUE)
    # }
    # # iter <- as.integer(250 / stack_times)
    # # mchi_terra_normkern <- mchi_terra
    # # for (i in 1:iter) {
    # fp_zm_terra_ <- focal(fp_zm_terra,
    #                                w = norm_kernel, fun = "sum",
    #                                na.policy = "only", na.rm = T
    #   )
    #
    # # }
    # } else{
    fp_zm_terra_ <- focal(fp_zm_terra, 19, mean, na.policy = "only", expand = TRUE) %>% crop(basin_zm_sf)
    # }

    if (logexp) {
      fp_zm_terra_ <- log(fp_zm_terra_)
    }

    min_zm <- minmax(fp_zm_terra_)[1]
    max_zm <- minmax(fp_zm_terra_)[2]
    # fp_zm_contour <- fp_zm_terra_ %>% as.contour()

    # ggplot() +
    #   geom_spatraster(data = log(fp_zm_terra_)
    #                   ) + scale_fill_viridis_c(na.value="transparent") +
    #   geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
    #   geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    # ggsave(paste0( "figures/model/zoom/",i,"_", j, nm_log, "_zm_terra_log.pdf"), width = tw, height = tw)


    ggplot() +
      geom_spatraster(data = fp_zm_terra_) +
      scale_fill_viridis_c(na.value = "transparent") +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    ggsave(paste0("figures/model/zoom/", i, "_", j, nm_log, "_zm_terra.pdf"), width = tw, height = tw)



    ggplot() +
      gg(data = fp_zm, aes(fill = mean), geom = "tile") +
      scale_fill_viridis_c() +
      # gg(data = landslides_poly_zm, col="red", fill = "red", aes(alpha = log(Area_m2))) +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    ggsave(here("figures", "model", "zoom", paste0(i, "_", j, nm_log, "_zm.pdf")))

    # ggplot() +
    #   geom_spatraster_contour(
    #     data = fp_zm_terra_, breaks = seq(min_zm, max_zm, (max_zm - min_zm)/bin)) +
    #   geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
    #   geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    # ggsave(here("figures", "model", "zoom", paste0(i, "_", j, nm_log, "zm_contour_.pdf")))

    ggplot() +
      geom_spatraster_contour_filled(data = fp_zm_terra_, bins = bin) +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    ggsave(here("figures", "model", "zoom", paste0(i, "_", j, nm_log, "_zm_contour_filled.pdf")), width = tw, height = tw)

    ggplot() +
      geom_spatraster_contour(
        data = fp_zm_terra_, aes(color = after_stat(level)),
        bins = bin,
        # breaks = seq(5, max_zm, 1),
        # binwidth = .2,
        na.rm = TRUE,
        linewidth = 0.2
      ) +
      scale_color_gradientn(
        colours = hcl.colors(20, "Inferno"),
        guide = guide_coloursteps()
      ) +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    # theme_minimal()
    ggsave(here("figures", "model", "zoom", paste0(i, "_", j, nm_log, "_zm_contour.pdf")))
  }
}


# fp4a_zm_raster <- vect(fp4a_zm) %>% fortify() %>% rast(crs = crs_nepal$input, nrows= 3721, ncols = 2552, names = "mean")

# fp4a_zm_terra <- rasterize(fp4a_zm, log_ksn_tag_zm, field = "mean")


# fp4a_zm_terra_ <- rast(fp4a_zm_terra, crs = crs_nepal$input)
# https://r.geocompx.org/raster-vector
# https://stackoverflow.com/questions/76330119/how-to-get-a-spatraster-of-indices-of-the-nearest-na-cell-in-r-terra

# https://adelieresources.com/2022/10/making-contour-maps-in-r/
# https://stackoverflow.com/questions/76000065/geom-contour-error-after-projecting-wgs84-coordinates-to-utm


# https://stackoverflow.com/questions/76000065/geom-contour-error-after-projecting-wgs84-coordinates-to-utm
# fp4a_zm_terra <- sf2sr(fp4a_zm["mean"], aggregate(log_ksn_tag_zm, fact = 5), udpate = TRUE, cover = TRUE, touches = TRUE)

# ggplot() +
#   geom_spatraster_contour(
#     data = log_ksn_tag_zm)
