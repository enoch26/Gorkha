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

source("read_data.R")

# rf2ch -------------------------------------------------------------------
rf2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_RELIEFTOCHAN.bil")) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)
rf2ch$rf2ch_km <- values(rf2ch) / 1000

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

pxl_mchi %<-% {
  fm_pixels(mesh_fm, dims = c(as.integer(8725/frac), as.integer(5555/frac)), 
            xlim = c(224.7896, 472.8312), ylim = c(3026.455, 3184.377),
            mask = bnd_out, 
            format = "sf")
  # format = "terra") # not working for predict.bru yet 
}

fdr <- "lanczos_"

gg_spatraster <- function(data, file_name, ...) {
  ggplot() + gg(data = data) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave(
    paste0("data/lsdtt/", fdr, "/figure/", file_name, ".png"),
    width = tw,
    height = tw / 2
  )
}




# Mchi --------------------------------------------------------------------
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

# stars rasterise ---------------------------------------------------------

# https://www.rdocumentation.org/packages/terra/versions/0.7-4/topics/rasterize
if(FALSE){
  mchi_stars <- st_rasterize(mchi_sf["m_chi"],
                                  st_as_stars(st_bbox(bnd_out),
                                              crs = crs_nepal$input,
                                              dx = 0.02794453, dy = 0.02794453,
                                              values = NA_real_))
  
  mchi_terra <- rast(mchi_stars)
}


# terra rasterise ----------------------------------------------
mchi_vect <- as_spatvector(mchi_sf["m_chi"], geom = "geometry", crs = crs_nepal$input)
# generate points
# p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)

# rasterize points as a matrix
mchi_terra <- rasterize(mchi_vect, rf2ch, fun = mean, field = "m_chi")

mchi_terra_focal_mean <- mchi_terra
win_size <- 9
iter <- as.integer(100/win_size)
for (i in 1:iter) {
  mchi_terra_focal_mean <- focal(mchi_terra_focal_mean, w = win_size, fun = "mean",
                                 na.policy = "only", na.rm = T)
}
mchi_terra_focal_mean$focal_1000 <- mchi_terra_focal_mean$focal_mean/1000
gg_spatraster(mchi_terra_focal_mean["focal_1000"], "mchi_focal_1000")
gg_spatraster(mchi_terra_focal_mean["mean"], "mchi_focal_mean")


# Gaussian Kernel -------------------------------------------------------------------

norm_kernel <- gkernel(matrix(1, 2, 2), norm = TRUE) # radius 3*30 / 2 = 45 m
stack_times <- 3 # TODO should be larger than mesh size, or do subdivide mesh into 9
for (i in 1:(stack_times)) {
  norm_kernel <- gkernel(norm_kernel, norm = TRUE)
}
iter <- as.integer(100/stack_times)
mchi_terra_focal_gauss <- mchi_terra
for (i in 1:iter) {
  mchi_terra_focal_gauss <- focal(mchi_terra_focal_gauss, w = norm_kernel, fun = "sum",
                                 na.policy = "only", na.rm = T)
}

mchi_terra_focal_gauss$focal_1000 <- mchi_terra_focal_gauss$focal_sum/1000
gg_spatraster(mchi_terra_focal_gauss["focal_1000"], "mchi_gauss_1000")


mchi_terra_focal_gauss$focal_rt <- (mchi_terra_focal_gauss$focal_sum)^(1/3)
ggplot() + geom_spatraster(data = mchi_terra_focal_gauss["focal_rt"]) + scale_fill_viridis_c(na.value = "transparent")
ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_rt.png"), width = tw, height = tw / 2)

mchi_terra_focal_max <- focal(mchi_terra, w = 55, fun = "max",
                              na.policy = "only", na.rm = T)



if(to_plot){
  ggplot() + geom_spatraster(data = mchi_terra) + scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "mchi_terra.png"), width = tw, height = tw / 2)
  ggplot() + geom_spatraster(data = mchi_terra_focal_mean) + scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_mean.png"), width = tw, height = tw / 2)
  ggplot() + geom_spatraster(data = mchi_terra_focal_max) + scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_max.png"), width = tw, height = tw / 2)
}



if(to_plot){
  ggplot(mchi %>% filter(m_chi > 0), aes(x = log(m_chi))) + geom_histogram(bins = 200)
  ggsave(paste0("data/lsdtt/", fdr, "/mchi_hist.png"), width = tw, height = tw / 2)
}

if(file.exists(here("data/mchi_sf.shp"))) {
  mchi_sf <- st_read(here("data/mchi_sf.shp"))
} else {
st_write(mchi_sf, "data/mchi_sf.shp")
}

if(file.exists(here("data/mchi_sf_samp.shp"))) {
  mchi_sf_samp <- st_read(here("data/mchi_sf_samp.shp"))
} else {
mchi_sf_samp <- mchi_sf[sample(1:nrow(mchi_sf), 50000),]
st_write(mchi_sf_samp, "data/mchi_sf_samp.shp")
}



# nth interesting > 0
# mchi_terra_focal_mean$mchi_geq0 <- mchi_terra_focal_mean %>% filter(focal_1000 > 0) 
# ggplot() + geom_spatraster(data = mchi_terra_focal_mean["mchi_geq0"]) + scale_fill_viridis_c(na.value = "transparent")
# ggsave(paste0("data/lsdtt/", fdr, "mchi_geq0.png"), width = tw, height = tw / 2)


ggplot() + geom_spatraster(data = mchi_terra_focal_mean) + scale_fill_viridis_c(na.value = "transparent")
ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_mean_.png"), width = tw, height = tw / 2)
ggplot() + geom_spatraster(data = mchi_terra_focal_mean["focal_1000"]) + scale_fill_viridis_c(na.value = "transparent")
ggsave(paste0("data/lsdtt/", fdr, "mchi_focal_mean1000_.png"), width = tw, height = tw / 2)


rf2ch$rf2ch_mchi <- rf2ch$rf2ch_km*(mchi_terra_focal_mean$focal_1000)

rf2ch$rf2ch_adj <- rf2ch$rf2ch_km/exp(rf2ch$rf2ch_km)

ggplot() + geom_spatraster(data = rf2ch["rf2ch_mchi"]) + scale_fill_viridis_c(na.value = "transparent") 
ggsave(paste0("data/lsdtt/", fdr, "rf2ch_mchi.png"), width = tw, height = tw / 2)

# mchi_sample <- mchi[sample(1:nrow(mchi), 100000),] 
# mchi_sample <- sample_n(mchi, 10000)
# reset row numbers otherwise terra would still consider the original row numbers
# rownames(mchi_sample) <- NULL
# write.csv(mchi_sample, "data/mchi_sample.csv")
# mchi_sample <- read.csv("data/mchi_sample.csv")
# mchi_spatraster <- rast(mchi_sample, digit = 1, crs = "EPSG:4326") %>% project(crs_nepal$input)
# mchi_sf_sample <- mchi_sf[sample(1:nrow(mchi_sf), 1000),]
  
if(to_plot){
  # mchi_sf
  ggplot() + gg(data = mchi_sf, aes(color = m_chi/1000), geom = "tile", 
                # alpha = .5, 
                size = 0.2) + 
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi.png"), width = tw, height = tw / 2)
  ggplot() + 
    gg(data = mchi_sf, aes(color = m_chi/1000), geom = "tile", 
       # alpha = .5, 
       size = 0.2) + 
    geom_sf(data = landslides, fill = "red", col = "red") +
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi_lds.png"), width = tw, height = tw / 2)
  
  # mchi_sf_samp
  ggplot() + gg(data = mchi_sf_samp, aes(color = log(m_chi+13)), geom = "tile", 
                # alpha = .5, 
                size = 0.2) + 
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi_samp.png"), width = tw, height = tw / 2)
  ggplot() + 
    gg(data = mchi_sf_samp, aes(color = log(m_chi+13)), geom = "tile", 
       # alpha = .5, 
       size = 0.2) + 
    geom_sf(data = landslides, fill = "red", col = "red") +
    scale_color_viridis_c() +
    geom_sf(data = bnd, col = "red", fill = NA)
  
  ggsave(paste0("data/lsdtt/", fdr, "/figure/mchi_samp_lds.png"), width = tw, height = tw / 2)
}

  
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
  
  pred_mchi <- predict(fit_mchi,
                           newdata = pxl_mchi,
                           formula = ~ mchi,
                           n.samples = 100,
                           seed = seed[1]
  )
  
  pred_mchi_vect <- as_spatvector(pred_mchi["mean"], geom = "geometry", crs = crs_nepal$input)
  # generate points
  # p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)
  
  # rasterize points as a matrix
  pred_mchi_terra <- rasterize(pred_mchi_vect, rf2ch, fun = mean, field = "mean")
  
  
  # pred_mchi_terra$rf2ch_mchi <- pred_mchi_terra$mean * sqrt(rf2ch$rf2ch_adj)
  pred_mchi_terra$rf2ch_mchi <- pred_mchi_terra$mean * rf2ch$rf2ch_adj
  pred_mchi_terra$rf2ch_mchi_ <- pred_mchi_terra$mean / exp(rf2ch$rf2ch_km)
  pred_mchi_terra$rf2ch_mchi_1000 <- pred_mchi_terra$rf2ch_mchi/1000
  pred_mchi_terra$rf2ch_mchi_1000_ <- pred_mchi_terra$rf2ch_mchi_/1000
  if(file.exists(here("data/lsdtt/", fdr, "rf2ch_mchi_.tif"))) {
    file.remove(here("data/lsdtt/", fdr, "rf2ch_mchi_.tif"))
  }else{
  writeRaster(pred_mchi_terra["rf2ch_mchi_1000"], file = paste0("data/lsdtt/", fdr, "/rf2ch_mchi.tif"))
  writeRaster(pred_mchi_terra["rf2ch_mchi_1000_"], file = paste0("data/lsdtt/", fdr, "/rf2ch_mchi_.tif"))
    
  }
  
  mchi_landslide <- extract(pred_mchi_terra["rf2ch_mchi_1000"], landslides_c)
  
  # its a bit too much to take root
  # pred_mchi_terra$rf2ch_mchi_rt <- (pred_mchi_terra$rf2ch_mchi)^(1/3)
  
  # ggplot() + geom_spatraster(data = pred_mchi_terra["rf2ch_mchi_rt"],  maxcell = 1e+07) + scale_fill_viridis_c(na.value = "transparent")
  # ggsave(paste0("data/lsdtt/", fdr, "pred_rf2ch_mchi_rt.png"), width = tw, height = tw / 2)
  
  pred_mchi_terra_agg <- terra::aggregate(pred_mchi_terra, fact = 10, na.rm = TRUE)
  pdf("data/lsdtt/pred_mchi.pdf")
  plot(pred_mchi_terra_agg$rf2ch_mchi_1000)
  dev.off()
  
  ggplot() + geom_spatraster(data = pred_mchi_terra_agg["rf2ch_mchi_1000"]) + 
    geom_sf(data = bnd, col = "red", fill = NA) + 
    # geom_sf(data = landslides, fill = "red", col = "red") +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "pred_rf2ch_mchi_1000.png"), width = tw, height = tw / 2)
  
  ggplot() + geom_spatraster(data = pred_mchi_terra_agg["rf2ch_mchi_1000"]) + 
    geom_sf(data = bnd, col = "red", fill = NA) + 
    geom_sf(data = landslides, fill = "red", col = "red") +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave(paste0("data/lsdtt/", fdr, "pred_rf2ch_mchi_1000_lds.png"), width = tw, height = tw / 2)
  
  

# deprecated --------------------------------------------------------------

  
  if(to_plot){
    ggplot() + gg(pred_mchi, geom = "tile") + 
      geom_sf(data = bnd, col = "red", fill = NA) + 
      # gg(data = mchi_sf, aes(color = m_chi), geom = "tile", 
      #    # alpha = .5, 
      #    size = 0.2) + 
      scale_fill_viridis_c()
    ggsave(paste0("data/lsdtt/", fdr, "pred_mchi.png"), width = tw, height = tw / 2)
    
    ggplot() + gg(pred_mchi, geom = "tile") + 
      geom_sf(data = bnd, col = "red", fill = NA) + 
      geom_sf(data = landslides, fill = "red", col = "red") +
      scale_fill_viridis_c()
    ggsave(paste0("data/lsdtt/", fdr, "pred_mchi_lds.png"), width = tw, height = tw / 2)
  }

  # pred_mchi_tbl <- tibble::as_tibble(pred_mchi["mean"], xy = TRUE)
  pred_mchi_terra <- rast(pred_mchi_tbl, crs = crs_nepal$input)
  
  
    if(to_plot){
    ggplot() + geom_spatraster(pred_mchi_terra) + scale_fill_viridis_c(na.value = "transparent")
    ggsave(paste0("data/lsdtt/", fdr, "pred_mchi_terra.png"), width = tw, height = tw / 2)
  }

  


