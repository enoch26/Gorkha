library(dplyr)
library(stars)
library(sf)
library(fmesher)
library(INLA)
library(inlabru)
library(here)
library(ggplot2)
library(terra)
library(tidyterra)
library(future)

# https://cran.r-project.org/web/packages/gdalraster/vignettes/gdal-config-quick-ref.html
gdalraster::set_config_option("GDAL_NUM_THREADS", "10") # somehow this works for interpNear with interpolate = FALSE
# setGDALconfig("GDAL_NUM_THREADS", value = 10) # this does not work


tw <- 15.55528
# fdr <- "lanczos_"

source("nepal_bnd.R")

# driver ----------------------------------------------------------------


dem <- rast(here("data", "lsdtt", fdr, "cop30dem.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
# dem_smooth <- rast(here("data", "lsdtt", fdr, "cop30dem_SMOOTH.bil")) %>% project(crs_nepal$input, threads = TRUE) %>% crop(bnd, mask = TRUE)
dinf <- rast(here("data", "lsdtt", fdr, "cop30dem_dinf_area.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
twi <- rast(here("data", "lsdtt", fdr, "cop30dem_TWI.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
hs <- rast(here("data", "lsdtt", fdr, "cop30dem_hs.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
basin <- rast(here("data", "lsdtt", fdr, "cop30dem_AllBasins.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
temp <- rast(here("data", "lsdtt", fdr, "temp_tagged.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
slp <- rast(here("data", "lsdtt", fdr, "cop30dem_SLOPE.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
rf2fr <- rast(here("data", "lsdtt", fdr, "cop30dem_RELIEFTOFARRIDGE.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
rf2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_RELIEFTOCHAN.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
fd2fr <- rast(here("data", "lsdtt", fdr, "cop30dem_FDTOFARRIDGE.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)
fd2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_FDTOCHAN.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd, mask = TRUE)

if(FALSE){
  fd2ch_fd2fr <- fd2ch$cop30dem_FDTOCHAN / (fd2ch$cop30dem_FDTOCHAN + fd2fr$cop30dem_FDTOFARRIDGE)
  rf2ch_rf2fr <- rf2ch$cop30dem_RELIEFTOCHAN / (rf2ch$cop30dem_RELIEFTOCHAN + rf2fr$cop30dem_RELIEFTOFARRIDGE)
  
  # which bit of the path the landslides on  --------------------------------
  
  fd2ch_fd2fr_crop <- fd2ch_fd2fr %>% crop(bnd, mask = TRUE)
  rf2ch_rf2fr_crop <- rf2ch_rf2fr %>% crop(bnd, mask = TRUE)
  
  rf2fr_zero <- rf2fr %>% crop(bnd_out, mask = TRUE) %>% filter(cop30dem_RELIEFTOFARRIDGE < 1e-10)
  
  landslides_c$lds_fd2ch_fd2fr <- extract(fd2ch_fd2fr_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  landslides_c$lds_rf2ch_rf2fr <- extract(rf2ch_rf2fr_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  a <- landslides_c %>% filter(lds_fd2ch_fd2fr == 1)
  b <- extract(fd2ch, vect(st_geometry(a)), ID = FALSE)
  c <- extract(fd2fr, vect(st_geometry(a)), ID = FALSE)
  
  
  # > summary(b)
  # cop30dem_FDTOCHAN
  # Min.   :  28.42  
  # 1st Qu.: 182.31  
  # Median : 440.17  
  # Mean   : 571.00  
  # 3rd Qu.: 842.57  
  # Max.   :2326.33  
  # > summary(c)
  # cop30dem_FDTOFARRIDGE
  # Min.   :0.000e+00    
  # 1st Qu.:0.000e+00    
  # Median :0.000e+00    
  # Mean   :1.781e-21    
  # 3rd Qu.:0.000e+00    
  # Max.   :1.294e-19
  
  summary(fd2ch_fd2fr_crop)
  summary(lds_fd2ch_fd2fr)
  
  summary(rf2ch_rf2fr_crop)
  summary(lds_rf2ch_rf2fr)
  
  ggplot() + geom_spatraster(data = fd2ch) +
    geom_sf(data = landslides_c %>% filter(lds_fd2ch_fd2fr == 1),
            fill = "red", col = "red", size = 0.2, alpha = .5) +
    geom_sf(data = bnd, col = "red", fill = NA) + 
    scale_fill_viridis_c(na.value = "transparent") 
  ggsave(paste0("data/lsdtt/", fdr, "/figure/landslides_fd2ch_fd2fr.pdf"), width = tw, height = tw / 2)
  
  lds_ls <- c("lds_fd2ch_fd2fr", "lds_rf2ch_rf2fr")
  dem_ls <- c("fd2ch_fd2fr_crop", "rf2ch_rf2fr_crop")
  title_ls <- c("RATIO_FDTOCHAN_FDTOFR", "RATIO_RFTOCHAN_RFTOFR")
  
  landslides_status <- c(
    T = "Pixels with landslides",
    F = "All pixels"
  )
  
  for (i in 1:length(lds_ls)) {
    df <- bind_rows(
      data.frame(value = as.numeric(unlist(get(lds_ls[i]))), landslides = "T"),
      data.frame(value = as.numeric(values(get(dem_ls[i]))), landslides = "F")
    )
    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 50) +
      # geom_density(alpha=.2, fill="#FF6666") +
      facet_wrap(~ landslides, ncol = 1, scales = "free_y", labeller = labeller(
        # .default = capitalize,
        landslides = landslides_status
      )) +
      ggtitle(title_ls[i])
    ggsave(paste0("data/lsdtt/", fdr, "/figure/hist/", title_ls[i], "_hist.pdf"), width = tw, height = tw / 2)
  }
}



# mchi --------------------------------------------------------------------


mchi <- read.csv(here("data", "lsdtt", fdr, "cop30dem_MChiSegmented.csv"), header = TRUE)

# mchi_tbl <- as_tibble(mchi %>% select(c("longitude", "latitude", "m_chi")), xy = TRUE)
# %>% select(longitude, latitude, m_chi)
mchi_sf <- st_as_sf(mchi, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_nepal)



# too big to visualise anything
if(FALSE){
  ggplot() + geom_spatraster(data = rf2fr_zero, maxcell = 5e+07, col = "red") + scale_fill_continuous(na.value = "transparent") +
    geom_sf(data = mchi_sf, aes(color = log(m_chi+12.3)), geom = "tile",
      # alpha = .5,
      size = 0.2) + scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = bnd, col = "red", fill = NA) 
  ggsave(paste0("data/lsdtt/", fdr, "/figure/rf2fr_zero.pdf"), width = tw, height = tw / 2)
}





# https://stackoverflow.com/questions/36960974/how-to-replace-raster-values-less-than-0-to-na-in-r-code
ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd_out, mask = TRUE) %>%
  clamp(1, values = TRUE)

# checking the rendering error for the missing pixels
if(FALSE){
  p <- rast()
  ext(p) <- c(300, 325, 3100, 3125)
  ggplot() + gg(data = ksn_tag %>% crop(p)) +
    scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    xlim(300, 325) +
    ylim(3100, 3125)
  ggsave(
    paste0("data/lsdtt/", fdr, "/figure/ksn_tag_zm.png"),
    width = tw,
    height = tw / 2
  )
}

if(FALSE){
  # gg_spatraster(log(ksn_tag_near$cop30dem_channel_tagged_pixels), "log_ksn_tag_near")
  ksn_tag_near <- interpNear(ksn_tag, as.points(ksn_tag),
                             interpolate = FALSE, # from a geoscience perspective, it is better to use FALSE
    radius = 4, # circle seems good enough 
    field = "cop30dem_channel_tagged_pixels"
    # threads = FALSE # when interpolate = TRUE no way to control threads
  )
  
  gg_spatraster(log(ksn_tag_near$cop30dem_channel_tagged_pixels), "log_ksn_tag_near")
  
  if(!file.exists(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels_near.bil"))){
    writeRaster(ksn_tag_near, paste0("data/", "lsdtt/", fdr, "/cop30dem_channel_tagged_pixels_near.tif"), overwrite = TRUE)
  }
}


if (FALSE) {
  dem_rough <- terrain(dem, v = "roughness")
  ggplot() +
    geom_spatraster(data = log(dem_rough)) +
    scale_fill_viridis_c(na.value = "transparent", option = "D") +
    geom_sf(data = bnd, col = "red", fill = NA) +
    coord_sf(crs = 32645)
}





## dem with range  ---------------------------------------------------------
if (FALSE) {
  dem_min <- 400
  dem_max <- 7250
  dem_filter <- dem %>%
    filter(`Band 1` > dem_min & `Band 1` < dem_max)
  ggplot() +
    geom_spatraster(data = dem_filter) +
    scale_fill_viridis_c(limits = c(dem_min, dem_max), na.value = "transparent") +
    geom_sf(data = landslides, fill = "red", col = "red") +
    geom_sf(data = bnd, col = "red", fill = NA)
  ggsave(paste0("data/lsdtt/", fdr, "/figure/dem_filter_lds.pdf"), width = tw, height = tw / 2)
  ggplot() +
    geom_spatraster(data = dem_filter) +
    scale_fill_viridis_c(limits = c(dem_min, dem_max), na.value = "transparent") +
    geom_sf(data = bnd, col = "red", fill = NA)
  # geom_sf(data = landslides, fill = "red", col = "red")
  ggsave(paste0("data/lsdtt/", fdr, "/figure/dem_filter.pdf"), width = tw, height = tw / 2)
}

# length(unique(values(dem_basin)))
# values(dem_basin) <- log(values(dem_basin) + 1)

# topo wetness index
# All requires GRASS GIS install
# https://r-packages.io/packages/fasterRaster/wetness
# QGIS in R to get TWI
# https://r.geocompx.org/gis
# https://github.com/rspatial/terra/issues/1436
# https://vt-hydroinformatics.github.io/rgeoraster.html#topographic-wetness-index

{
  names_ls <- c(
    "dem",
    # "dem_smooth",
    "dinf",
    "twi",
    "hs",
    "basin",
    "ksn_tag",
    # "temp",
    "slp", "rf2fr", "rf2ch", "fd2fr", "fd2ch"
    # "fd2ch_fd2fr", "rf2ch_rf2fr"
  )
  png_nm_ls <- c(
    "dem",
    # "SMOOTH",
    "log_dinf_area",
    "log_TWI",
    "hs",
    "AllBasins",
    "log_channel_tagged_pixels",
    # "temp_tagged",
    "SLOPE", "RELIEFTOFARRIDGE", "RELIEFTOCHAN", "FDTOFARRIDGE", "FDTOCHAN"
    # "FDTOCHAN_FDTOFARRIDGE", "RELIEFTOCHAN_RELIEFTOFARRIDGE"
  )
}


# with landslides centroids -----------------------------------------------------

i <- 1
for (r in names_ls) {
  if (r %in% c("dinf", "twi", "ksn_tag")) {
    ggplot() +
      # geom_spatraster(data = dem_hs) +
      geom_spatraster(data = log(get(r)), na.rm = TRUE) +
      geom_sf(data = landslides_c, fill = "red", col = "red", size = 0.05, alpha = 0.1) +
      # geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c(na.value = "transparent", option = "D") +
      geom_sf(data = bnd, col = "red", fill = NA) +
      coord_sf(crs = 32645) +
      ggtitle(png_nm_ls[i])
    # ggsave(paste0("./", r, ".pdf"), width = tw, height = tw / 2)
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", png_nm_ls[i], "_lds.pdf"), width = tw, height = tw / 2)
  } else {
    ggplot() +
      # geom_spatraster(data = dem_hs) +
      geom_spatraster(data = get(r), na.rm = TRUE) +
      geom_sf(data = landslides_c, fill = "red", col = "red", size = 0.05, alpha = 0.1) +
      # geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c(na.value = "transparent", option = "D") +
      geom_sf(data = bnd, col = "red", fill = NA) +
      coord_sf(crs = 32645) +
      ggtitle(png_nm_ls[i])
    # ggsave(paste0("./", r, ".pdf"), width = tw, height = tw / 2)
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", png_nm_ls[i], "_lds.pdf"), width = tw, height = tw / 2)
  }
  i <- i + 1
}



# without landslides centroids --------------------------------------------
i <- 1
for (r in names_ls) {
  if (r %in% c("dinf", "twi", "ksn_tag")) {
    ggplot() +
      # geom_spatraster(data = dem_hs) +
      geom_spatraster(data = log(get(r)), na.rm = TRUE) +
      # geom_sf(data = landslides_c, fill = "red", col = "red", size = 0.05, alpha = 0.1) +
      # geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c(na.value = "transparent", option = "D") +
      geom_sf(data = bnd, col = "red", fill = NA) +
      coord_sf(crs = 32645) +
      ggtitle(png_nm_ls[i])
    # ggsave(paste0("./", r, ".pdf"), width = tw, height = tw / 2)
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", png_nm_ls[i], ".pdf"), width = tw, height = tw / 2)
  } else {
    ggplot() +
      # geom_spatraster(data = dem_hs) +
      geom_spatraster(data = get(r), na.rm = TRUE) +
      # geom_sf(data = landslides_c, fill = "red", col = "red", size = 0.05, alpha = 0.1) +
      # geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c(na.value = "transparent", option = "D") +
      geom_sf(data = bnd, col = "red", fill = NA) +
      coord_sf(crs = 32645) +
      ggtitle(png_nm_ls[i])
    # ggsave(paste0("./", r, ".pdf"), width = tw, height = tw / 2)
    ggsave(paste0("data/lsdtt/", fdr, "/figure/", png_nm_ls[i], ".pdf"), width = tw, height = tw / 2)
  }
  i <- i + 1
}

if(FALSE){
  ggplot() +
    geom_spatraster(data = basin, na.rm = TRUE) +
    scale_fill_viridis_c(na.value = "transparent", option = "H") +
    geom_sf(data = water) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    # coord_sf(crs = 32645) +
    ggtitle("basin with water network")
  # ggsave(paste0("./", r, ".pdf"), width = tw, height = tw / 2)
  ggsave(paste0("data/lsdtt/", fdr, "/figure/", "basin_water.pdf"), width = tw, height = tw / 2)
  
}


# {
#   i <- 1
#   for (r in names_ls) {
#     ggplot() +
#       # geom_spatraster(data = dem_hs) +
#       geom_spatraster(data = get(r), na.rm = TRUE) +
#       # geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) +
#       # geom_sf(data = landslides, fill = "red", col = "red") +
#       scale_fill_viridis_c(na.value = "transparent", option = "D") +
#       geom_sf(data = bnd, col = "red", fill = NA) +
#       coord_sf(crs = 32645) +
#       ggtitle(png_nm_ls[i])
#     # ggsave(paste0("./", r, ".pdf"), width = tw, height = tw / 2)
#     ggsave(paste0("data/lsdtt/", fdr, "/figure/", png_nm_ls[i], ".pdf"), width = tw, height = tw / 2)
#     i <- i + 1
#   }
# }


# check lds wrt raster ----------------------------------------------------

if (FALSE) {
  dem_twi_crop <- dem_twi %>% crop(bnd, mask = TRUE)
  dem_rf2fr_crop <- dem_rf2fr %>% crop(bnd, mask = TRUE)
  dem_rf2ch_crop <- dem_rf2ch %>% crop(bnd, mask = TRUE)
  dem_fd2fr_crop <- dem_fd2fr %>% crop(bnd, mask = TRUE)
  dem_fd2ch_crop <- dem_fd2ch %>% crop(bnd, mask = TRUE)
  
  
  lds_twi <- extract(dem_twi_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  lds_rf2fr <- extract(dem_rf2fr_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  lds_rf2ch <- extract(dem_rf2ch_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  lds_fd2fr <- extract(dem_fd2fr_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  lds_fd2ch <- extract(dem_fd2ch_crop, vect(st_geometry(landslides_c)), ID = FALSE)
  


  print("twi")
  summary(dem_twi_crop)
  summary(lds_twi)
  print("rf2fr")
  summary(dem_rf2fr_crop)
  summary(lds_rf2fr)
  print("rf2ch")
  summary(dem_rf2ch_crop)
  summary(lds_rf2ch)
  print("fd2fr")
  summary(dem_fd2fr_crop)
  summary(lds_fd2fr)
  print("fd2ch")
  summary(dem_fd2ch_crop)
  summary(lds_fd2ch)


  lds_ls <- c("lds_twi", "lds_rf2fr", "lds_rf2ch", "lds_fd2fr", "lds_fd2ch")
  dem_ls <- c("dem_twi_crop", "dem_rf2fr_crop", "dem_rf2ch_crop", "dem_fd2fr_crop", "dem_fd2ch_crop")
  title_ls <- c("TWI", "RELIEFTOFARRIDGE", "RELIEFTOCHAN", "FDTOFARRIDGE", "FDTOCHAN")

  for (i in 1:length(lds_ls)) {
    df <- bind_rows(
      data.frame(value = as.numeric(unlist(get(lds_ls[i]))), landslides = "T"),
      data.frame(value = as.numeric(values(get(dem_ls[i]))), landslides = "F")
    )
    ggplot(df, aes(x = value)) +
      geom_histogram(bins = 200) +
      # geom_density(alpha=.2, fill="#FF6666") +
      facet_wrap(~landslides, ncol = 1, scales = "free_y") +
      ggtitle(title_ls[i])
    ggsave(paste0("data/lsdtt/", fdr, "/figure/hist/", title_ls[i], "_hist.pdf"), width = tw, height = tw / 2)
  }
}
# getting certain raster
# https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r
# https://stackoverflow.com/questions/76330119/how-to-get-a-spatraster-of-indices-of-the-nearest-na-cell-in-r-terra
if(FALSE){
  ggplot(data.frame(value = as.numeric(unlist(lds_rf2ch))), aes(x = value)) +
    geom_histogram(bins = 300) +
    geom_density(alpha = .2, fill = "#FF6666")
  ggsave(paste0("data/lsdtt/", fdr, "/figure/hist/", "lds_rf2ch_hist200.pdf"), width = tw, height = tw / 2)
  
}

# check any NA within raster ----------------------------------------------------
if(FALSE){
  dem_rf2ch_NA <- dem_rf2ch %>%
    crop(bnd, mask = TRUE) %>%
    clamp(lower = 10, upper = Inf, values = FALSE) %>%
    subst(NA, -9999)
  ggplot() +
    geom_spatraster(data = dem_rf2ch_NA) +
    geom_sf(data = landslides, fill = "red", col = "red")
  ggsave(paste0("data/lsdtt/", fdr, "/figure/dem_rf2ch_NA.pdf"), width = tw, height = tw / 2)
  
  
  lds_fd2ch_NA <- eval_spatial(data = dem_rf2ch_NA, where = st_geometry(landslides_c), layer = "cop30dem_RELIEFTOCHAN")
  # lds_fd2ch_NA <- extract(dem_rf2ch_NA, vect(st_geometry(landslides_c)), ID = FALSE)
  # any(as.numeric(terra::global(x, fun = "notNA")[,1]) > 0)
  summary(lds_fd2ch_NA == -9999)
}
# interactive  ------------------------------------------------------------

# if (interactive()) {
if (FALSE) {
  dem_twi <- rast(("./cop30dem_SMOOTH_TWI.bil"))
  dem_basin <- rast(("./cop30dem_SMOOTH_AllBasins.bil"))
  dem_channel <- rast(("./cop30dem_SMOOTH_channel_tagged_pixels.bil"))
  # dem_temp <- rast(("./temp_tagged.bil"))
  # dem_smooth_slp <- rast(("./cop30dem_SMOOTH_SLOPE.bil"))
  dem_smooth_rf2fr <- rast(("./cop30dem_SMOOTH_RELIEFTOFARRIDGE.bil"))
  dem_smooth_rf2ch <- rast(("./cop30dem_SMOOTH_RELIEFTOCHAN.bil"))
  dem_smooth_fd2fr <- rast(("./cop30dem_SMOOTH_FDTOFARRIDGE.bil"))
  dem_smooth_fd2ch <- rast(("./cop30dem_SMOOTH_FDTOCHAN.bil"))

  names_ls <- c(
    "dem_twi",
    "dem_basin", "dem_channel",
    # "dem_temp",
    # "dem_smooth_slp",
    "dem_smooth_rf2fr", "dem_smooth_rf2ch", "dem_smooth_fd2fr", "dem_smooth_fd2ch"
  )
  png_nm_ls <- c(
    "TWI",
    "SMOOTH_AllBasins", "SMOOTH_channel_tagged_pixels",
    # "temp_tagged",
    # "SMOOTH_SLOPE",
    "SMOOTH_RELIEFTOFARRIDGE", "SMOOTH_RELIEFTOCHAN", "SMOOTH_FDTOFARRIDGE", "SMOOTH_FDTOCHAN"
  )

  i <- 1
  for (r in names_ls) {
    ggplot() +
      # geom_spatraster(data = dem_hs) +
      geom_spatraster(data = get(r), na.rm = TRUE) +
      geom_sf(data = landslides_c, fill = "red", col = "red", size = 0.05, alpha = 0.1) +
      # geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c(na.value = "transparent", option = "D") +
      coord_sf(crs = 32645)
    ggsave(paste0("./figure/", png_nm_ls[i], "_lds.pdf"), width = tw, height = tw / 2)
    i <- i + 1
    # ggsave(paste0("data/lsdtt/01/figure/", r, ".pdf"), width = tw, height = tw / 2)
  }

  i <- 1
  for (r in names_ls) {
    ggplot() +
      # geom_spatraster(data = dem_hs) +
      geom_spatraster(data = get(r), na.rm = TRUE) +
      # geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) +
      # geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c(na.value = "transparent", option = "D") +
      coord_sf(crs = 32645)
    ggsave(paste0("./figure/", png_nm_ls[i], ".pdf"), width = tw, height = tw / 2)
    i <- i + 1
    # ggsave(paste0("data/lsdtt/01/figure/", r, ".pdf"), width = tw, height = tw / 2)
  }
}



# RELIEF2CHANNEL/PGA ------------------------------------------------------
if (FALSE) {
  pga_mean_raster <-
    rast(here(
      "data", "raster", "pga_mean.flt"
    )) %>%
    project("epsg:32645") %>%
    crop(bnd, mask = TRUE)

  pga_mean_raster_resample <- resample(pga_mean_raster, dem_rf2ch_crop, method = "cubic") %>% crop(ext(dem_rf2ch_crop))

  pga_exprf2ch <- exp(2 * pga_mean_raster_resample) / exp(.1 * dem_rf2ch_crop / 1000)
  ggplot() +
    geom_spatraster(data = pga_exprf2ch) +
    scale_fill_viridis_c(na.value = "transparent", option = "D")
  ggsave(paste0("data/lsdtt/", fdr, "/figure/pga_exprf2ch.pdf"), width = tw, height = tw / 2)

  ggplot() +
    geom_spatraster(data = pga_exprf2ch) +
    scale_fill_viridis_c(na.value = "transparent", option = "D") +
    geom_sf(data = landslides_c, fill = "red", col = "red", size = 0.05, alpha = 0.1)
  ggsave(paste0("data/lsdtt/", fdr, "/figure/pga_exprf2ch_lds.pdf"), width = tw, height = tw / 2)
}

# https://stackoverflow.com/questions/68502309/math-opperation-between-a-spatraster-and-a-vectors-using-terra-package

# terra algebra
# https://rspatial.org/spatial/8-rastermanip.html

#####################################################
#
# dem_twi <- rast(("./cop30dem_TWI.bil"))
# dem_hs <- rast(("./cop30dem_hs.bil"))
# dem_basin <- rast(("./cop30dem_AllBasins.bil"))
# dem_channel <- rast(("./cop30dem_channel_tagged_pixels.bil"))
# # dem_temp <- rast(("./temp_tagged.bil"))
# dem_slp <- rast(("./cop30dem_SLOPE.bil"))
# dem_rf2fr <- rast(("./cop30dem_RELIEFTOFARRIDGE.bil"))
# dem_rf2ch <- rast(("./cop30dem_RELIEFTOCHAN.bil"))
# dem_fd2fr <- rast(("./cop30dem_FDTOFARRIDGE.bil"))
# dem_fd2ch <- rast(("./cop30dem_FDTOCHAN.bil"))
#
# names_ls <- c("dem_twi", "dem_hs", "dem_basin", "dem_channel",
#               # "dem_temp",
#               "dem_slp", "dem_rf2fr", "dem_rf2ch", "dem_fd2fr", "dem_fd2ch")
# png_nm_ls <- c("TWI", "hs", "AllBasins", "channel_tagged_pixels",
#                # "temp_tagged",
#                "SLOPE", "RELIEFTOFARRIDGE", "RELIEFTOCHAN", "FDTOFARRIDGE", "FDTOCHAN")
#
# i <- 1
# for (r in names_ls) {
#   ggplot() +
#     # geom_spatraster(data = dem_hs) +
#     geom_spatraster(data = get(r), na.rm = TRUE) +
#     geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) +
#     # geom_sf(data = landslides, fill = "red", col = "red") +
#     scale_fill_viridis_c(na.value = "transparent", option = "A") +
#     coord_sf(crs = 32645)
#   ggsave(paste0("./", png_nm_ls[i], ".pdf"), width = tw, height = tw / 2)
#   i = i + 1
#   # ggsave(paste0("data/lsdtt/01/figure/", r, ".pdf"), width = tw, height = tw / 2)
# }


#####################################################
# ggplot() +
#   geom_spatraster(data = dem_hs) +
#   geom_spatraster(data = dem_temp, na.rm = TRUE) +
#   geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) +
#   # geom_sf(data = landslides, fill = "red", col = "red") +
#   scale_fill_continuous(na.value = "transparent") +
#   coord_sf(crs = 32645)
# ggsave("data/lsdtt/01/figure/dem_temp.pdf", width = tw, height = tw / 2)





# tutorial ----------------------------------------------------------------

#
#
# xian <- rast(here("data", "lsdtt", fdr, "Xian.bil"))
# xian_basin <- rast(here("data", "lsdtt", fdr, "Xian_AllBasins.bil"))
# outlet_pts <- read.csv(here("data", "lsdtt", fdr, "my_outlets.csv"), header = TRUE)
# outlet_pts <- st_as_sf(outlet_pts, coords = c("longitude", "latitude"), crs = 4326) %>%
#   st_transform(32645)
#
# ggplot() +
#   geom_spatraster(data = xian) +
#   geom_spatraster(data = xian_basin, aes(alpha = 0.7)) +
#   geom_sf(data = outlet_pts, col = "red", pch = 19)
#
#
# ggplot() +
#   geom_spatraster(data = dem_hs) +
#   geom_sf(data = st_as_sfc(landslides_bbox_), fill = NA, col = "red")

# ggsave("data/lsdtt/01/figure/dem_hs_crop.pdf", width = tw, height = tw / 2)

# https://lsdtopotools.github.io/LSDTT_documentation/LSDTT_chi_analysis.html
# xian <- rast(here("data", "Xian.bil"))


# dem_bil_wfilt <- rast(here("data", "lsdtt", fdr, "Wfilt", "cop30dem_Wfilt.bil"))
# dem_bil_04basin <- rast(here("data", "lsdtt", fdr, "cop30dem_04_AllBasins.bil"))
# dem_bil_smooth04basin <- rast(here("data", "lsdtt", fdr, "cop30dem_SMOOTH04_AllBasins.bil"))

# dem_bil_hs <- rast(here("data", "lsdtt", fdr, "cop30dem_SMOOTH_hs.bil"))
# dem_hs <- rast(here("data", "lsdtt", fdr, "cop30dem_hs.bil"))
# plot(dem_hs)

# https://dieghernan.github.io/tidyterra/articles/faqs.html
# ggplot() +
#   geom_spatraster(data = dem_bil) +
#   geom_sf(data = bnd, col = "red", fill = NA) +
#   scale_fill_viridis_c(na.value = "transparent", option = "D")
# ggsave("data/lsdtt/01/figure/dem.pdf", width = tw, height = tw / 2)
#
# ggplot() +
#   geom_spatraster(data = dem_bil_hs) +
#   geom_sf(data = bnd, col = "red", fill = NA) +
#   scale_fill_viridis_c(na.value = "transparent", option = "D")
# ggsave("data/lsdtt/01/figure/dem_hs.pdf", width = tw, height = tw / 2)



# dem_rng <- extract(dem, vect(st_geometry(landslides_c)), ID = FALSE)
# > summary(dem_rng)
# Band 1
# Min.   : 255
# 1st Qu.:1433
# Median :1989
# Mean   :2110
# 3rd Qu.:2682
# Max.   :7224


# {dem_min <- 400
# dem_max <- 7000
# dem_bil_smooth_filter <- dem_bil_smooth %>%
#   filter(cop30dem_SMOOTH > dem_min & cop30dem_SMOOTH < dem_max)
# ggplot() +
#   geom_spatraster(data = dem_bil_smooth_filter) +
#   scale_fill_viridis_c(limits = c(dem_min, dem_max), na.value = "transparent") +
#   geom_sf(data = landslides, fill = "red", col = "red") +
#   geom_sf(data = bnd, col = "red", fill = NA)
# ggsave("data/lsdtt/01/figure/dem_smooth_rm.pdf", width = tw, height = tw / 2)}

# {dem_bil_smoothbasin <- rast(here("data", "lsdtt", fdr, "cop30dem_SMOOTH_AllBasins.bil"))
#   ggplot() +
#     # geom_spatraster(data = dem_bil_hs)+
#     geom_spatraster(data = dem_bil_smoothbasin, na.rm = TRUE) +
#     geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) +
#     # geom_sf(data = landslides, fill = "red", col = "red") +
#     geom_sf(data = bnd, col = "red", fill = NA) +
#     scale_fill_continuous(na.value = "transparent") +
#     coord_sf(crs = 32645)
#   ggsave("data/lsdtt/01/figure/dem_bil_smoothbasin.pdf", width = tw, height = tw / 2)}


# ggplot() +
#   # geom_spatraster(data = dem_bil_hs)+
#   geom_spatraster(data = dem_bil_smooth04basin, na.rm = TRUE) +
#   geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) +
#   # geom_sf(data = landslides, fill = "red", col = "red") +
#   scale_fill_continuous(na.value = "transparent") +
#   coord_sf(crs = 32645)
# ggsave("data/lsdtt/01/figure/dem_bil_smooth04basin.pdf", width = tw, height = tw / 2)

# wfilt <- rast("data", "lsdtt", fdr, "cop30dem_Wfilt.bil")

# ggplot() +
#   geom_spatraster(data = wfilt)
# + geom_sf(data = landslides_c, fill = "red", col = "red",  size = 0.05, alpha = 0.1) + coord_sf(crs = 32645)
# ggsave("./wfilt.pdf", width = tw, height = tw / 2)