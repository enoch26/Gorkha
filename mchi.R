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
# https://www.wvview.org/os_sa/15b_Raster_Analysis_terra.html
# https://rspatial.github.io/terra/reference/focal.html
# https://stackoverflow.com/questions/76263598/error-with-custom-function-in-terrafocal-in-r
# https://lsdtopotools.github.io/LSDTT_documentation/LSDTT_chi_analysis.html

# contour
# https://arc2r.github.io/book/Density.html

CV_chess <- FALSE; CV_thin <- FALSE
# https://github.com/riatelab/maptiles
# get_providers()
source("read_data.R")


# param -------------------------------------------------------------------


win_size <- 3
iter <- as.integer(250 / win_size)

# pxl ---------------------------------------------------------------------


# > res(dem)
# [1] 0.02794453 0.02794453
# > dim(dem)
# [1] 5651 8875    2
# > ext(dem)
# SpatExtent : 224.811476916494, 472.819209698314, 3026.45808422482, 3184.37264174032 (xmin, xmax, ymin, ymax)

# fm_bbox(mesh_fm)
# Bounding box: (222.4811,475.1508) x (3024.144,3186.699)
# lets set a fraction of the resolution for computation
frac <- 1


# function ----------------------------------------------------------------


gg_spatraster <- function(data, file_name, ...) {
  ggplot() +
    gg(data = data) +
    scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = bnd, col = "red", fill = NA)
  ggsave(
    paste0("data/lsdtt/", fdr, "/figure/", file_name, ".png"),
    width = tw,
    height = tw / 2
  )
}
# 
# lnorm <- function(data, mu = 0, sigma = 1) {
#   # exp(-(log(data)-mu)^2/sigma^2)/(sigma*sqrt(2*pi)*data)
#   exp(-(log(data) - mu)^2 / sigma) / (data)
# }


# load data ---------------------------------------------------------------
if (FALSE) {
  pxl_mchi %<-% {
    fm_pixels(mesh_fm,
      dims = c(as.integer(8725 / frac), as.integer(5555 / frac)),
      xlim = c(224.7896, 472.8312), ylim = c(3026.455, 3184.377),
      mask = bnd_out,
      format = "sf"
    )
    # format = "terra") # not working for predict.bru yet
  }
}

## rf2ch -------------------------------------------------------------------

fdr <- "lanczos_"

rf2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_RELIEFTOCHAN.bil")) %>%
  project(crs_nepal$input) # %>%
# crop(bnd_out, mask = TRUE)
rf2ch$rf2ch_km <- values(rf2ch) / 1000
rf2ch$rf2ch_exp_inv <- 1 / exp(rf2ch$rf2ch_km)

## fd2ch -------------------------------------------------------------------
fd2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_FDTOCHAN.bil")) %>%
  project(crs_nepal$input)
fd2ch$fd2ch_km <- values(fd2ch$cop30dem_FDTOCHAN) / 1000
fd2ch$fd2ch_exp_inv <- 1 / exp(fd2ch$fd2ch_km)

## Mchi --------------------------------------------------------------------
# if(file.exists(here(paste0("data","lsdtt", fdr, "mchi.tif")))) {
#   pred_mchi <- rast(here(paste0("data","lsdtt", fdr, "mchi.tif")))
# } else {
# https://gis.stackexchange.com/questions/288570/find-nearest-point-along-polyline-using-sf-package-in-r

mchi <- read.csv(here("data", "lsdtt", fdr, "cop30dem_MChiSegmented.csv"), header = TRUE)

# mchi_tbl <- as_tibble(mchi %>% select(c("longitude", "latitude", "m_chi")), xy = TRUE)
# %>% select(longitude, latitude, m_chi)
mchi_sf <- st_as_sf(mchi, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = crs_nepal)
# %>% st_intersection(bnd_out)
# }

# lanczos
# summary(mchi_sf$m_chi)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# -0.483    96.611   205.110   286.761   379.690 15817.000
# lanczos_
# summary(mchi_sf$m_chi)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -12.28    39.55   157.80   250.37   338.32 12388.00

## terra rasterise ----------------------------------------------
if(FALSE){
mchi_vect <- as_spatvector(mchi_sf["m_chi"], geom = "geometry", crs = crs_nepal$input)
# generate points
# p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)

if(FALSE){
  mchi04 <- read.csv(here("data", "lsdtt", "lanczos_mn4", "cop30dem_MChiSegmented.csv"), header = TRUE)
  mchi05 <- read.csv(here("data", "lsdtt", fdr, "cop30dem_MChiSegmented.csv"), header = TRUE)
  mchi06 <- read.csv(here("data", "lsdtt", "lanczos_mn6", "cop30dem_MChiSegmented.csv"), header = TRUE)
  
  mchi_sf04 <- st_as_sf(mchi04, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal)
  mchi_sf05 <- st_as_sf(mchi05, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal)
  mchi_sf06 <- st_as_sf(mchi06, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal)

}


# rasterize points as a matrix
mchi_terra <- rasterize(mchi_vect, rf2ch, fun = mean, field = "m_chi")
### interpolate = TRUE ----------------------------------------------
mchi_terra_near <- interpNear(mchi_terra, mchi_vect,
  field = "m_chi",
  radius = .1,
  interpolate = TRUE
)

mchi_terra_near$log_mchi <- log(mchi_terra_near$mean)
mchi_terra_near$fd2ch_log_mchi <- mchi_terra_near$log_mchi * fd2ch$fd2ch_exp_inv
mchi_terra_near$fd2ch_mchi_1000 <- mchi_terra_near$mean / 1000 * fd2ch$fd2ch_exp_inv
  
}



# farridge = 0 ------------------------------------------------------------

rf2fr_zero <- rf2fr %>% crop(bnd, mask = TRUE) %>% filter(cop30dem_RELIEFTOFARRIDGE < 1e-10)
# too big to visualise anything
if(FALSE){
  ggplot() + geom_spatraster(data = rf2fr_zero, maxcell = 5e+07, col = "red") + scale_fill_continuous(na.value = "transparent") +
    geom_sf(data = mchi_sf, aes(color = log(m_chi+12.3)), geom = "tile",
            # alpha = .5,
            size = 0.2) + scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = bnd, col = "red", fill = NA) 
  ggsave(paste0("data/lsdtt/", fdr, "/figure/rf2fr_zero.pdf"), width = tw, height = tw / 2)
}


# zoom --------------------------------------------------------------------
# https://stackoverflow.com/questions/49200458/find-nearest-features-using-sf-in-r
# https://r-spatial.github.io/sf/reference/st_nearest_feature.html
# mchi_lds_pairs <- st_nearest_points(landslides_zm, mchi_sf_zm)
# summary(mchi_lds_pairs)
# https://gis.stackexchange.com/questions/349955/getting-a-new-column-with-distance-to-the-nearest-feature-in-r
# one way of doing it

basin <- rast(here("data", "lsdtt", fdr, "cop30dem_AllBasins.bil")) %>%
  project(crs_nepal$input)

ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  # crop(bnd_out, mask = TRUE) %>%
  clamp(lower = 1, values = TRUE)

# 14982 basin_info
long <- 85.5
lat <- 28.2

long <- 85.25
lat <- 28

long <- 84.6
lat <- 28

long <- 85.2
lat <- 28.2

# to fix glacier landscapes
long <- 85
lat <- 28.5


source("mchi_zm.R")



### interpNear --------------------------------------------------------------
mchi_terra_near$cop30dem_AllBasins <- basin$cop30dem_AllBasins
mchi_terra_near_zm <- crop(mchi_terra_near, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)

ggplot() +
  gg(data = mchi_terra_near_zm$log_mchi) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_near_log_mchi_zm.pdf"), width = tw, height = tw / 2)

ggplot() +
  gg(data = mchi_terra_near_zm$log_mchi) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_poly_zm, fill = "red", col = "red", size = 0.2, aes(alpha = Area_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_near_log_mchi_ploy_zm.pdf"), width = tw, height = tw / 2)

mchi_terra_near_zm$rf2ch_log_mchi <- mchi_terra_near_zm$log_mchi * rf2ch_zm$rf2ch_exp_inv
ggplot() +
  gg(data = mchi_terra_near_zm$rf2ch_log_mchi) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
# geom_sf(data = landslides_poly_zm, fill = "red", col = "red", size = 0.2, aes(alpha = Area_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_near_rf2ch_log_mchi_zm.pdf"), width = tw, height = tw / 2)

mchi_terra_near_zm$fd2ch_log_mchi <- mchi_terra_near_zm$log_mchi * fd2ch_zm$fd2ch_exp_inv
ggplot() +
  gg(data = mchi_terra_near_zm$fd2ch_log_mchi) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
# geom_sf(data = landslides_poly_zm, fill = "red", col = "red", size = 0.2, aes(alpha = Area_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_near_fd2ch_log_mchi_zm.pdf"), width = tw, height = tw / 2)

pdf("data/lsdtt/lanczos_/figure/rf2ch_hist.pdf")
hist(as.numeric(rf2ch_zm$rf2ch_km))
dev.off()
pdf("data/lsdtt/lanczos_/figure/rf2ch_lnorm_mchi_zm_hist.pdf")
hist(as.numeric(lnorm(rf2ch_zm$rf2ch_km, mu = 0, sigma = 1)))
dev.off()

# 0 and 2 seem the best option
mchi_terra_near_zm$lnorm_rf2ch <- lnorm(rf2ch_zm$rf2ch_km, mu = -1, sigma = 2)
ggplot() +
  gg(data = mchi_terra_near_zm$lnorm_rf2ch) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_near_lnorm_rf2ch_zm.pdf"), width = tw, height = tw / 2)

mchi_terra_near_zm$rf2ch_lnorm_mchi <- mchi_terra_near_zm$log_mchi * lnorm(rf2ch_zm$rf2ch_km, mu = -1, sigma = 2)
mchi_terra_near_zm$rf2ch_mchi <- mchi_terra_near_zm$mean * rf2ch_zm$rf2ch_exp_inv
mchi_terra_near_zm$fd2ch_mchi <- mchi_terra_near_zm$mean * fd2ch_zm$fd2ch_exp_inv

ggplot() +
  gg(data = mchi_terra_near_zm$mean) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_near_mchi_zm.pdf"), width = tw, height = tw / 2)




###########################################################################################################
# DEPRECATED --------------------------------------------------------------

# #stars rasterise ---------------------------------------------------------

# https://www.rdocumentation.org/packages/terra/versions/0.7-4/topics/rasterize
if (FALSE) {
  mchi_stars <- st_rasterize(
    mchi_sf["m_chi"],
    st_as_stars(st_bbox(bnd_out),
      crs = crs_nepal$input,
      dx = 0.02794453, dy = 0.02794453,
      values = NA_real_
    )
  )

  mchi_terra <- rast(mchi_stars)
}


if (to_plot) {
  ggplot(mchi %>% filter(m_chi > 0), aes(x = log(m_chi))) +
    geom_histogram(bins = 200)
  ggsave(paste0("data/lsdtt/", fdr, "/mchi_hist.png"), width = tw, height = tw / 2)
}

if (file.exists(here("data/mchi_sf.shp"))) {
  mchi_sf <- st_read(here("data/mchi_sf.shp"))
} else {
  st_write(mchi_sf, "data/mchi_sf.shp")
}

if (file.exists(here("data/mchi_sf_samp.shp"))) {
  mchi_sf_samp <- st_read(here("data/mchi_sf_samp.shp"))
} else {
  mchi_sf_samp <- mchi_sf[sample(1:nrow(mchi_sf), 50000), ]
  st_write(mchi_sf_samp, "data/mchi_sf_samp.shp")
}



# nth interesting > 0
# mchi_terra_focal_mean$mchi_geq0 <- mchi_terra_focal_mean %>% filter(focal_1000 > 0)
# ggplot() + geom_spatraster(data = mchi_terra_focal_mean["mchi_geq0"]) + scale_fill_viridis_c(na.value = "transparent")
# ggsave(paste0("data/lsdtt/", fdr, "mchi_geq0.png"), width = tw, height = tw / 2)


ggplot() +
  geom_spatraster(data = mchi_terra_focal_mean) +
  scale_fill_viridis_c(na.value = "transparent")
ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_mean_.png"), width = tw, height = tw / 2)
ggplot() +
  geom_spatraster(data = mchi_terra_focal_mean["focal_1000"]) +
  scale_fill_viridis_c(na.value = "transparent")
ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_mean1000_.png"), width = tw, height = tw / 2)


# rf2ch$rf2ch_mchi <- rf2ch$rf2ch_km*(mchi_terra_focal_mean$focal_1000)

rf2ch$rf2ch_adj <- rf2ch$rf2ch_km / exp(rf2ch$rf2ch_km)

ggplot() +
  geom_spatraster(data = rf2ch["rf2ch_mchi"]) +
  scale_fill_viridis_c(na.value = "transparent")
ggsave(paste0("data/lsdtt/", fdr, "rf2ch_mchi.png"), width = tw, height = tw / 2)

# mchi_sample <- mchi[sample(1:nrow(mchi), 100000),]
# mchi_sample <- sample_n(mchi, 10000)
# reset row numbers otherwise terra would still consider the original row numbers
# rownames(mchi_sample) <- NULL
# write.csv(mchi_sample, "data/mchi_sample.csv")
# mchi_sample <- read.csv("data/mchi_sample.csv")
# mchi_spatraster <- rast(mchi_sample, digit = 1, crs = "EPSG:4326") %>% project(crs_nepal$input)
# mchi_sf_sample <- mchi_sf[sample(1:nrow(mchi_sf), 1000),]


if (to_plot) {
  # mchi_sf_samp
  ggplot() +
    gg(
      data = mchi_sf_samp, aes(color = log(m_chi + 13)), geom = "tile",
      # alpha = .5,
      size = 0.2
    ) +
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)

  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi_samp.png"), width = tw, height = tw / 2)
  ggplot() +
    gg(
      data = mchi_sf_samp, aes(color = log(m_chi + 13)), geom = "tile",
      # alpha = .5,
      size = 0.2
    ) +
    geom_sf(data = landslides, fill = "red", col = "red") +
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)

  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi_samp_lds.png"), width = tw, height = tw / 2)
}


# inlabru mchi model -----------------------------------------------------------


if (FALSE) {
  matern <- inla.spde2.pcmatern(mesh_fm,
    prior.range = c(4, 0.1),
    prior.sigma = c(2, 0.1)
  )

  cmp_mchi <- ~ mchi(main = geometry, model = matern)

  fml_mchi <- m_chi ~ mchi

  lik_mchi <- bru_obs("Gaussian",
    formula = fml_mchi,
    data = mchi_sf_samp
  )

  fit_mchi <- bru(
    components = cmp_mchi, lik_mchi,
    options = list(
      # control.inla = list(int.strategy = "eb"),
      bru_verbose = 3, bru_max_iter = 50
    )
  )
  save(fit_mchi, file = "data/mchi.RData")
  load("data/mchi.RData")
  pred_mchi <- predict(fit_mchi,
    newdata = pxl_mchi,
    formula = ~mchi,
    n.samples = 100,
    seed = seed[1]
  )

  pred_mchi_vect <- as_spatvector(pred_mchi["mean"], geom = "geometry", crs = crs_nepal$input)
  # generate points
  # p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)

  # rasterize points as a matrix
  pred_mchi_terra <- rasterize(pred_mchi_vect, rf2ch, fun = mean, field = "mean") %>% project(crs_nepal$input)
  pred_mchi_terra$log_mchi <- log(pred_mchi_terra$mean)
  pred_mchi_terra$rf2ch_mchi <- pred_mchi_terra$mean * rf2ch$rf2ch_adj
  pred_mchi_terra$rf2ch_mchi_ <- pred_mchi_terra$mean * exp(-rf2ch$rf2ch_km)
  pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean) * exp(-(log(rf2ch$rf2ch_km + 0.005))^2 / 4) / (2 * rf2ch$rf2ch_km)
  pred_mchi_terra$rf2ch_log_mchi <- pred_mchi_terra$log_mchi * rf2ch$rf2ch_adj
  pred_mchi_terra$rf2ch_log_mchi_ <- pred_mchi_terra$log_mchi / exp(rf2ch$rf2ch_km)
}

# pred_mchi_terra$rf2ch_mchi <- pred_mchi_terra$mean * sqrt(rf2ch$rf2ch_adj)
# pred_mchi_terra$rf2ch_mchi_1000 <- pred_mchi_terra$rf2ch_mchi/1000
# pred_mchi_terra$rf2ch_mchi_1000_ <- pred_mchi_terra$rf2ch_mchi_/1000
if (file.exists(here("data/lsdtt/", fdr, "rf2ch_mchi_.tif"))) {
  pred_mchi_terra <- rast(paste0("data/lsdtt/", fdr, "/pred_mchi.tif"))
  pred_mchi_terra <- c(
    rast(here("data/lsdtt/", fdr, "rf2ch_mchi.tif")),
    rast(here("data/lsdtt/", fdr, "rf2ch_mchi_.tif")),
    rast(here("data/lsdtt/", fdr, "rf2ch_log_mchi.tif")),
    rast(here("data/lsdtt/", fdr, "rf2ch_log_mchi_.tif"))
  )
  pred_mchi_terra <- pred_mchi_terra %>% project(crs_nepal$input)
  pred_mchi_terra$mean <- pred_mchi_terra$rf2ch_mchi_ * exp(rf2ch$rf2ch_km)
} else {
  writeRaster(pred_mchi_terra$mean, file = paste0("data/lsdtt/", fdr, "/pred_mchi.tif"), overwrite = TRUE)
  writeRaster(pred_mchi_terra$rf2ch_mchi, file = paste0("data/lsdtt/", fdr, "/rf2ch_mchi.tif"), overwrite = TRUE)
  writeRaster(pred_mchi_terra$rf2ch_mchi_, file = paste0("data/lsdtt/", fdr, "/rf2ch_mchi_.tif"), overwrite = TRUE)
  writeRaster(pred_mchi_terra$rf2ch_log_mchi, file = paste0("data/lsdtt/", fdr, "/rf2ch_log_mchi.tif"), overwrite = TRUE)
  writeRaster(pred_mchi_terra$rf2ch_log_mchi_, file = paste0("data/lsdtt/", fdr, "/rf2ch_log_mchi_.tif"), overwrite = TRUE)
}
gg_spatraster(pred_mchi_terra$mean, "pred_mchi_mean")
gg_spatraster(pred_mchi_terra$log_mchi, "pred_log_mchi")
gg_spatraster(pred_mchi_terra$rf2ch_mchi, "pred_rf2ch_mchi")
gg_spatraster(pred_mchi_terra$rf2ch_mchi_, "pred_rf2ch_mchi_")
gg_spatraster(pred_mchi_terra$rf2ch_log_mchi, "pred_rf2ch_log_mchi")
gg_spatraster(pred_mchi_terra$rf2ch_log_mchi_, "pred_rf2ch_log_mchi_")

mchi_landslide <- extract(pred_mchi_terra["rf2ch_mchi_1000"], landslides_c)

# its a bit too much to take root
# pred_mchi_terra$rf2ch_mchi_rt <- (pred_mchi_terra$rf2ch_mchi)^(1/3)

# ggplot() + geom_spatraster(data = pred_mchi_terra["rf2ch_mchi_rt"],  maxcell = 1e+07) + scale_fill_viridis_c(na.value = "transparent")
# ggsave(paste0("data/lsdtt/", fdr, "pred_rf2ch_mchi_rt.png"), width = tw, height = tw / 2)

# pixl resolution too high cant even plot properly
if (to_plot) {
  pred_mchi_terra_agg <- terra::aggregate(pred_mchi_terra, fact = 10, na.rm = TRUE)
  pdf("data/lsdtt/pred_mchi.pdf")
  plot(pred_mchi_terra_agg$rf2ch_mchi_1000)
  dev.off()

  ggplot() +
    geom_spatraster(data = pred_mchi_terra_agg["rf2ch_mchi_1000"]) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    # geom_sf(data = landslides, fill = "red", col = "red") +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "pred_rf2ch_mchi_1000.png"), width = tw, height = tw / 2)

  ggplot() +
    geom_spatraster(data = pred_mchi_terra_agg["rf2ch_mchi_1000"]) +
    geom_sf(data = bnd, col = "red", fill = NA) +
    geom_sf(data = landslides, fill = "red", col = "red") +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "pred_rf2ch_mchi_1000_lds.png"), width = tw, height = tw / 2)
}

if (FALSE) {
  if (to_plot) {
    ggplot() +
      gg(pred_mchi, geom = "tile") +
      geom_sf(data = bnd, col = "red", fill = NA) +
      # gg(data = mchi_sf, aes(color = m_chi), geom = "tile",
      #    # alpha = .5,
      #    size = 0.2) +
      scale_fill_viridis_c()
    ggsave(paste0("data/lsdtt/", fdr, "pred_mchi.png"), width = tw, height = tw / 2)

    ggplot() +
      gg(pred_mchi, geom = "tile") +
      geom_sf(data = bnd, col = "red", fill = NA) +
      geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c()
    ggsave(paste0("data/lsdtt/", fdr, "pred_mchi_lds.png"), width = tw, height = tw / 2)
  }

  # pred_mchi_tbl <- tibble::as_tibble(pred_mchi["mean"], xy = TRUE)
  pred_mchi_terra <- rast(pred_mchi_tbl, crs = crs_nepal$input)


  if (to_plot) {
    ggplot() +
      geom_spatraster(pred_mchi_terra) +
      scale_fill_viridis_c(na.value = "transparent")
    ggsave(paste0("data/lsdtt/", fdr, "pred_mchi_terra.png"), width = tw, height = tw / 2)
  }
}

if (FALSE) {
  # focal mean ---------------------------------------------------------------
  mchi_terra_focal_mean <- mchi_terra

  for (i in 1:iter) {
    mchi_terra_focal_mean <- focal(mchi_terra_focal_mean,
      w = win_size, fun = "mean",
      na.policy = "all", na.rm = T
    )
    # na.policy = "only", na.rm = T)
  }
  mchi_terra_focal_mean$focal_1000 <- mchi_terra_focal_mean$focal_mean / 1000
  if (to_plot) {
    gg_spatraster(mchi_terra_focal_mean["mean"], "mchi_focal_mean")
    gg_spatraster(mchi_terra_focal_mean["focal_1000"], "mchi_focal_1000")
  }

  # focal max ---------------------------------------------------------------
  # not looking great
  if (FALSE) {
    mchi_terra_focal_max <- mchi_terra
    for (i in 1:iter) {
      mchi_terra_focal_max <- focal(mchi_terra_focal_max,
        w = win_size, fun = "max",
        # na.policy = "all", na.rm = T)
        na.policy = "only", na.rm = T
      )
    }
    mchi_terra_focal_max$focal_1000 <- mchi_terra_focal_max$focal_max / 1000
    if (to_plot) {
      gg_spatraster(mchi_terra_focal_max["max"], "mchi_focal_max")
      gg_spatraster(mchi_terra_focal_max["focal_1000"], "mchi_focal_1000")
    }
  }

  # focal min ---------------------------------------------------------------
  mchi_terra_focal_min <- mchi_terra
  for (i in 1:iter) {
    mchi_terra_focal_min <- focal(mchi_terra_focal_min,
      w = win_size, fun = "min",
      na.policy = "all", na.rm = T
    )
    # na.policy = "only", na.rm = T)
  }
  mchi_terra_focal_min$focal_1000 <- mchi_terra_focal_min$focal_min / 1000
  if (to_plot) {
    gg_spatraster(mchi_terra_focal_min["min"], "mchi_focal_min")
    gg_spatraster(mchi_terra_focal_min["focal_1000"], "mchi_focal_1000")
  }

  # focal median ---------------------------------------------------------------
  mchi_terra_focal_median <- mchi_terra
  for (i in 1:iter) {
    mchi_terra_focal_median <- focal(mchi_terra_focal_median,
      w = win_size, fun = "median",
      na.policy = "all", na.rm = T
    )
    # na.policy = "only", na.rm = T)
  }
  mchi_terra_focal_median$focal_1000 <- mchi_terra_focal_median$focal_median / 1000
  mchi_terra_focal_median$log_mchi <- log(mchi_terra_focal_median$focal_median)
  if (to_plot) {
    gg_spatraster(mchi_terra_focal_median["median"], "mchi_focal_median")
    gg_spatraster(mchi_terra_focal_median["focal_1000"], "mchi_focal_1000")
    gg_spatraster(mchi_terra_focal_median["log_mchi"], "log_mchi_focal_median")
  }
}
# Gaussian Kernel -------------------------------------------------------------------
if (FALSE) {
  norm_kernel <- gkernel(matrix(1, 2, 2), norm = TRUE) # radius 3*30 / 2 = 45 m
  stack_times <- 3 # TODO should be larger than mesh size, or do subdivide mesh into 9
  for (i in 1:(stack_times)) {
    norm_kernel <- gkernel(norm_kernel, norm = TRUE)
  }
  iter <- as.integer(250 / stack_times)
  mchi_terra_normkern <- mchi_terra
  for (i in 1:iter) {
    mchi_terra_normkern <- focal(mchi_terra_normkern,
      w = norm_kernel, fun = "sum",
      na.policy = "all", na.rm = T
    )
    # na.policy = "only", na.rm = T)
  }
  mchi_terra_normkern$focal_1000 <- mchi_terra_normkern$focal_sum / 1000
  if (to_plot) {
    gg_spatraster(mchi_terra_normkern["focal_sum"], "mchi_normkern")
    gg_spatraster(mchi_terra_normkern["focal_1000"], "mchi_normkern_1000")
  }
  if (FALSE) {
    mchi_terra_normkern$focal_rt <- (mchi_terra_normkern$focal_sum)^(1 / 3)
    gg_spatraster(data = mchi_terra_normkern["focal_rt"], "mchi_normkern_rt")
  }
}


## na.policy = only --------------------------------------------------------


mchi_terra_normkern_only <- mchi_terra
for (i in 1:iter) {
  mchi_terra_normkern_only <- focal(mchi_terra_normkern_only,
    w = norm_kernel, fun = "sum",
    # na.policy = "all", na.rm = T)
    na.policy = "only", na.rm = T
  )
}
mchi_terra_normkern_only$focal_1000 <- mchi_terra_normkern_only$focal_sum / 1000
if (to_plot) {
  gg_spatraster(mchi_terra_normkern_only["focal_sum"], "mchi_normkern_only")
  gg_spatraster(mchi_terra_normkern_only["focal_1000"], "mchi_normkern_only_1000")
}
if (FALSE) {
  mchi_terra_normkern_only$focal_rt <- (mchi_terra_normkern_only$focal_sum)^(1 / 3)
  gg_spatraster(data = mchi_terra_normkern_only["focal_rt"], "mchi_normkern_only_rt")
}

# focalMat Gauss mean----------------------------------------------------------
if (FALSE) {
  # d = 3 not great
  gauss_w <- focalMat(mchi_terra, d = 9, type = "Gauss")
  gauss_w <- gauss_w / max(gauss_w)

  mchi_terra_focal_gauss_max <- focal(mchi_terra,
    w = gauss_w, fun = "max",
    # na.policy = "all", na.rm = T)
    na.policy = "only", na.rm = T
  )

  mchi_terra_focal_gauss <- focal(mchi_terra,
    w = gauss_w, fun = "mean",
    # na.policy = "all", na.rm = T)
    na.policy = "only", na.rm = T
  )
  mchi_terra_focal_gauss$focal_1000 <- mchi_terra_focal_gauss$focal_mean / 1000
  mchi_terra_focal_gauss$log_m_chi <- log(mchi_terra_focal_gauss$focal_mean)
  gg_spatraster(mchi_terra_focal_gauss["mean"], "mchi_focal_gauss")
  gg_spatraster(mchi_terra_focal_gauss["focal_1000"], "mchi_focal_gauss_1000")
  gg_spatraster(mchi_terra_focal_gauss["log_m_chi"], "log_mchi_focal_gauss")
}



### normkern ----------------------------------------------------------------


mchi_terra_normkern$cop30dem_AllBasins <- basin$cop30dem_AllBasins
mchi_terra_normkern$mchi_rf2ch <- log(mchi_terra_normkern$focal_sum) * rf2ch$rf2ch_exp_inv
mchi_terra_normkern_zm <- crop(mchi_terra_normkern, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
mchi_terra_normkern_zm$log_m_chi <- log(mchi_terra_normkern_zm$focal_sum)
# https://stackoverflow.com/questions/36313319/r-ggplot2-use-raster-as-greyscale-basemap
if (to_plot) {
  ggplot() +
    gg(data = mchi_terra_normkern_zm["mchi_rf2ch"]) +
    scale_fill_viridis_c(na.value = "transparent") +
    geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
    geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5)
  ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_normkern_zm.pdf"), width = tw, height = tw / 2)
}


### pred inlabru ------------------------------------------------------------
# pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean) * exp(-(log(rf2ch$rf2ch_km+0.005))^2/1.5)/(1.5*rf2ch$rf2ch_km)
# pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean) * exp(-(log(rf2ch$rf2ch_km+0.005))^2/1.25)/(1.25*rf2ch$rf2ch_km)
# pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean)^2 * exp(-(log(rf2ch$rf2ch_km+0.005))^2)/(rf2ch$rf2ch_km)
pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean)^2 * exp(-(log(rf2ch$rf2ch_km + 0.01))^2 / 2.1) / (2.1 * rf2ch$rf2ch_km)
pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean) * exp(-(log(rf2ch$rf2ch_km + 0.01))^2 / 2.1) / (2.1 * rf2ch$rf2ch_km)
pred_mchi_terra$rf2ch_mchi_logn <- log(pred_mchi_terra$mean)^2 * exp(-(log(rf2ch$rf2ch_km + 0.005))^2 / 2) / (2 * rf2ch$rf2ch_km)

pred_mchi_terra$cop30dem_AllBasins <- basin$cop30dem_AllBasins
pred_mchi_terra_zm <- crop(pred_mchi_terra, basin_zm_sf) %>% filter(cop30dem_AllBasins == basin_info$cop30dem_AllBasins)
ggplot() +
  gg(data = pred_mchi_terra_zm$rf2ch_mchi_logn) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, aes(alpha = logarea_m2))
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_pred_rf2ch_logn_mchi_zm.pdf"), width = tw, height = tw / 2)


ggplot() +
  gg(data = pred_mchi_terra_zm$rf2ch_mchi_) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5)
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_pred_rf2ch_mchi_zm_.pdf"), width = tw, height = tw / 2)

ggplot() +
  gg(data = pred_mchi_terra_zm$rf2ch_log_mchi) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5)
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_pred_rf2ch_log_mchi_zm.pdf"), width = tw, height = tw / 2)

ggplot() +
  gg(data = pred_mchi_terra_zm$rf2ch_log_mchi_) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5)
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_pred_rf2ch_log_mchi_zm_.pdf"), width = tw, height = tw / 2)

ggplot() +
  gg(data = pred_mchi_terra_zm$rf2ch_log_mchi) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = basin_zm_sf, col = "red", fill = NA) +
  geom_sf(data = landslides_zm, fill = "red", col = "red", size = 0.2, alpha = .5)
ggsave(paste0("data/lsdtt/", fdr, "/figure/basin_pred_rf2ch_log_mchi_zm.pdf"), width = tw, height = tw / 2)

### interpolate = FALSE ----------------------------------------------
mchi_terra_near_interpF <- interpNear(mchi_terra, mchi_vect,
                                      field = "m_chi",
                                      radius = c(5, 5),
                                      interpolate = FALSE
)

mchi_terra_near_interpF$mchi_1000 <- mchi_terra_near_interpF$mean / 1000
mchi_terra_near_interpF$log_mchi <- log(mchi_terra_near_interpF$mean)
gg_spatraster(mchi_terra_near_interpF$log_mchi, "log_mchi_near_interpF")
gg_spatraster(mchi_terra_near_interpF$mchi_1000, "mchi_near_interpF_1000")

if (FALSE) {
  writeRaster(mchi_terra_near$fd2ch_log_mchi, here("data", "lsdtt", fdr, "/mchi_near_fd2ch_log_mchi.tif"), overwrite = TRUE)
}

if (to_plot) {
  # mchi_terra_near
  gg_spatraster(mchi_terra_near$log_mchi, "mchi_near")
  gg_spatraster(mchi_terra_near$fd2ch_log_mchi, "mchi_near_fd2ch_log_mchi")
  # mchi_sf
  ggplot() +
    gg(
      data = mchi_sf, aes(color = log(m_chi + .5)), geom = "tile",
      # alpha = .5,
      size = 0.2
    ) +
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi.png"), width = tw, height = tw / 2)
  ggplot() +
    gg(
      data = mchi_sf, aes(color = log(m_chi + .5)), geom = "tile",
      # alpha = .5,
      size = 0.2
    ) +
    geom_sf(data = landslides, fill = "red", col = "red") +
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi_lds.png"), width = tw, height = tw / 2)
}

