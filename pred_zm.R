# run after source("pred.R")
# TODO increase resolution fro the models
# https://r.geocompx.org/raster-vector
# https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.html
# https://dieghernan.r-universe.dev/articles/tidyterra/welcome.html

# inset map
# https://r.geocompx.org/adv-map.html
library(grid)
library(gridExtra)
# https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/
# https://mapping-in-r-workshop.ryanpeek.org/04_vig_making_inset_maps

tile <- maptiles::get_tiles(st_as_sfc(bnd), provider = "Esri.WorldImagery", crop = TRUE, zoom = 7) 
res_png <- 500

# Satellite Imagery
# https://sesync-ci.github.io/blog/mapping-with-Mapbox.html
# contour
bin <- 10
# basin_idx <- c(14982, 2228, 15329, 17757, 22048)
basin_idx <- c(2228, 14982, 15329)

# 12059 big piece but got that tail weird shape 
# 22048, 17757 too small
# 27417, 27959 useless
# in log scale
logexp <- FALSE 

if(logexp) {
  nm_log <- "log"
} else {
  nm_log <- ""
}
basin <- rast(here("data", "lsdtt", fdr, "cop30dem_AllBasins.bil")) %>%
  project(crs_nepal$input)
# 2228
# TODO contour with ksn to show the landslides centroids

mod_names_a <- ls(pattern = "fit.a")
mod_names_b <- ls(pattern = "fit.b")
pred_names_a <- ls(pattern = "fp.a")
pred_names_b <- ls(pattern = "fp.b")

if(FALSE){
  mod_names_a <- "fit6a"
  pred_names_a <- "fp6a"
}


pwr <- 1.75
sc <- scales::rescale(seq(0,1,length.out = 30)^pwr)

for (i in basin_idx) {
  basin_zm <- basin %>% filter(cop30dem_AllBasins == i)
  basin_zm_sf <- as_sf(as.polygons(basin_zm)) %>% st_intersection(bnd)
  landslides_poly_zm <- st_intersection(landslides, st_as_sfc(basin_zm_sf))
  landslides_c_zm <- st_intersection(landslides_c, st_as_sfc(basin_zm_sf))
  landslides_c_test_zm <- st_intersection(landslides_c_test, st_as_sfc(basin_zm_sf))

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

    inset_map <- ggplot() + geom_spatraster_rgb(data = tile) + 
      geom_sf(data = bnd, fill = NA, col = "red") +
      geom_sf(data = basin_zm_sf, aes(fill = "red", col = "red"), alpha = 0.75) +
      # geom_sf(data = landslides, aes(fill = "red"), alpha = 0.5) +
      # coord_sf(xlim = c(80.5, 89), ylim = c(26, 31), expand = FALSE) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.background = element_blank(),
            panel.border = element_blank(),
            plot.margin = unit(c(0, 0, 0 ,0), "mm")) 
    
    
    # main_map <- ggplot() +
    #   geom_spatraster(data = fp_zm_terra_) +
    #   scale_fill_viridis_c(na.value = "transparent", option = "C") +
    #   geom_sf(data = landslides_c_zm, fill = NA, col = "white", size = 0.001, alpha = 0.2) +
    #   geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    
    main_map <- ggplot() +
      geom_spatraster(data = fp_zm_terra_) +
      scale_fill_viridis_c(values = sc, na.value = "transparent", option = "D") +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.005, alpha = 0.5) +
      geom_sf(data = landslides_c_test_zm, fill = NA, col = "red", size = 0.005, alpha = 0.5) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1) +
      ggspatial::annotation_scale(location = "br") +
      ggspatial::annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.3, "in"))
    
    
    if(i == 15329){
      png(filename = paste0("figures/model/zoom/", i, j, nm_log, trainset, nm_chess,"_zm_terra.png"), 
          width = tw/3, height = tw/2, units = "in", res=res_png)
      grid.newpage()
      mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
      insetmap <- viewport(width = 0.4, height = 0.4, x = .31, y = 0.8)
      print(main_map, vp = mainmap) 
      print(inset_map, vp = insetmap)
      dev.off()
      
    } else if(i == 2228){
      png(filename = paste0("figures/model/zoom/", i, j, nm_log, trainset, nm_chess,"_zm_terra.png"), 
          width = tw/3, height = tw/2, units = "in", res=res_png)
      grid.newpage()
      mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
      insetmap <- viewport(width = 0.29, height = 0.29, x = .235, y = 0.49)
      # insetmap <- viewport(width = 0.3, height = 0.3, x = .67, y = 0.22)
      print(main_map, vp = mainmap) 
      print(inset_map, vp = insetmap)
      dev.off()
      
    } else if(i == 14982){
      png(filename = paste0("figures/model/zoom/", i, j, nm_log, trainset, nm_chess,"_zm_terra.png"), 
          width = tw/2.5, height = tw/3.5, units = "in", res=res_png)
      grid.newpage()
      mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
      insetmap <- viewport(width = 0.3, height = 0.3, x = .27, y = .73)
      print(main_map, vp = mainmap) 
      print(inset_map, vp = insetmap)
      dev.off()
    } else{
      png(filename = paste0("figures/model/zoom/", i, j, nm_log, trainset, nm_chess,"_zm_terra.png"), 
          width = tw/2.5, height = tw/3.5, units = "in", res=res_png)
      grid.newpage()
      mainmap <- viewport(width = 1, height = 1, x = 0.5, y = 0.5) # main map
      insetmap <- viewport(width = 0.15, height = 0.15, x = .25, y = .7)
      print(main_map, vp = mainmap) 
      print(inset_map, vp = insetmap)
      dev.off()
    }

    if(FALSE){
      
      ggplot() +
        geom_spatraster(data = log(fp_zm_terra_)
        ) + scale_fill_viridis_c(na.value="transparent") +
        geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
        geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
      ggsave(paste0( "figures/model/zoom/",i,"_", j, nm_log, trainset, nm_chess,"_zm_terra_.pdf"), width = tw, height = tw)
      
    ggplot() +
      geom_spatraster(data = fp_zm_terra_) +
      scale_fill_viridis_c(na.value = "transparent") +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    ggsave(paste0("figures/model/zoom/", i, j, nm_log, trainset, nm_chess,"_zm_terra.pdf"), width = tw, height = tw)
    
    ggplot() +
      gg(data = fp_zm, aes(fill = mean), geom = "tile") +
      scale_fill_viridis_c() +
      # gg(data = landslides_poly_zm, col="red", fill = "red", aes(alpha = log(Area_m2))) +
      geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
      geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    ggsave(here("figures", "model", "zoom", paste0(i, j, nm_log, trainset, nm_chess,"_zm.pdf")))
    
      
    }




    # ggplot() +
    #   geom_spatraster_contour(
    #     data = fp_zm_terra_, breaks = seq(min_zm, max_zm, (max_zm - min_zm)/bin)) +
    #   geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
    #   geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    # ggsave(here("figures", "model", "zoom", paste0(i, j, nm_log, trainset, nm_chess,"zm_contour_.pdf")))

    # ggplot() +
    #   geom_spatraster_contour_filled(data = fp_zm_terra_, bins = bin) +
    #   geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
    #   geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    # ggsave(here("figures", "model", "zoom", paste0(i, j, nm_log, trainset, nm_chess,"_zm_contour_filled.pdf")), width = tw, height = tw)

    # ggplot() +
    #   geom_spatraster_contour(
    #     data = fp_zm_terra_, aes(color = after_stat(level)),
    #     bins = bin,
    #     # breaks = seq(5, max_zm, 1),
    #     # binwidth = .2,
    #     na.rm = TRUE,
    #     linewidth = 0.2
    #   ) +
    #   scale_color_gradientn(
    #     colours = hcl.colors(20, "Inferno"),
    #     guide = guide_coloursteps()
    #   ) +
    #   geom_sf(data = landslides_c_zm, fill = NA, col = "red", size = 0.01) +
    #   geom_sf(data = basin_zm_sf, fill = NA, col = "red", size = 0.1)
    # # theme_minimal()
    # ggsave(here("figures", "model", "zoom", paste0(i, j, nm_log, trainset, nm_chess,"_zm_contour.pdf")))
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
