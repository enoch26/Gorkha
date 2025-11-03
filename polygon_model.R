library(INLA)
library(inlabru)
library(sf)
library(terra)
load("./lds_toy.Rdata")
pga_nuwakot <- rast("./pga_nuwakot.tif")
log_ksn_nuwakot <- rast("./log_ksn_nuwakot.tif")
rainfall_nuwakot <- rast("./rainfall_nuwakot.tif")
fd2ch_nuwakot <- rast("./fd2ch_nuwakot.tif")

cmp <- ~ Intercept(1) +
  log_pga(pga_nuwakot["pga_mean"], model = "linear") +
  log_ksn(log_ksn_nuwakot["cop30dem_channel_tagged_pixels"], model = "linear") +
  rainfall(rainfall_nuwakot["precip_2015"], model = "linear") +
  fd2ch(fd2ch_nuwakot["fd2ch_km"], model = "linear")
  
fml <- geometry ~   Intercept + log_pga + log_ksn + rainfall + fd2ch
landslides_c_nuwakot$sum_weight <- sum(landslides_c_nuwakot$Area_m2)
landslides_c_nuwakot_　<- landslides_c_nuwakot %>%
  mutate(scaled_weight = Area_m2 / sum_weight)

# TODO is it possible to weigh each point according to area of landslide polygon?
lik_c <- {
  bru_obs(
    formula = fml,
    family = "cp",
    data = landslides_c_nuwakot_, # landslide centroids, sf point
    domain = list(
      geometry = mesh_hex
    ),
    samplers = nuwakot_bnd_sf
    # weights = scaled_weight
  )
}

fit_c <- {
  bru(
    components = cmp, lik_c,
    options = list(
      bru_verbose = 3, bru_max_iter = 100
    )
  )
}

lds_ips <- fm_int(domain = list(
  geometry = mesh_hex
), samplers = landslides_nuwakot)

lds_ips_ <- lds_ips %>% group_by(.block) %>% 
  mutate(renormal_weight = weight / sum(weight)) %>% 
  # mutate(weight_sum = sum(weight), renormal_weight = weight / weight_sum) %>% 
  ungroup() 

lik <- {
  bru_obs(
    formula = fml,
    family = "cp",
    data = lds_ips_, # landslide polygons ips 
    domain = list(
      geometry = mesh_hex
    ),
    samplers = nuwakot_bnd_sf,
    weights = renormal_weight
  )
}

fit <- {
  bru(
    components = cmp, lik,
    options = list(
      bru_verbose = 3, bru_max_iter = 100
    )
  )
}


# predict comparison ------------------------------------------------------
library(ggplot2)
tw <- 15.55528
pxl <- fm_pixels(mesh_hex, format = "sf", mask = nuwakot_bnd_sf, dims = c(1000, 1000))
field_c <- predict(fit_c, pxl, ~ Intercept + log_pga + log_ksn + rainfall + fd2ch)
field <- predict(fit, pxl, ~ Intercept + log_pga + log_ksn + rainfall + fd2ch)


scl <- scale_fill_viridis_c(limits = range(c(field[["q0.025"]], field[["mean"]], field[["q0.975"]])), name = "lambda")
for(i in c("q0.025", "mean", "q0.975")){
p1 <- ggplot() +
  gg(nuwakot_bnd_sf) +
  gg(data = field_c[i], geom = "tile") +
  gg(data = landslides_nuwakot, fill = "red") + ggtitle("centroid") + scl
# ggsave("figures/pred_hex_nuwakot_c.png", width = tw, height = tw)

p2 <- ggplot() +
  gg(nuwakot_bnd_sf) +
  gg(data = field[i], geom = "tile") + 
  gg(data = landslides_nuwakot, fill = "red") + ggtitle("polygon") + scl
# ggsave("figures/pred_hex_nuwakot.png", width = tw, height = tw)

library(patchwork)
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(paste0("figures/pred_hex_nuwakot_c_p", i, ".png"), width = tw, height = tw/2)

}

for(i in c("sd")){
  scl <- scale_fill_viridis_c(limits = range(c(field[[i]])), name = "sd")
  
p1 <- ggplot() +
  gg(nuwakot_bnd_sf) +
  gg(data = field_c[i], geom = "tile") +
  gg(data = landslides_nuwakot, fill = "red") + ggtitle("centroid") + scl
# ggsave("figures/pred_hex_nuwakot_c.png", width = tw, height = tw)

p2 <- ggplot() +
  gg(nuwakot_bnd_sf) +
  gg(data = field[i], geom = "tile") + 
  gg(data = landslides_nuwakot, fill = "red") + ggtitle("polygon") + scl
# ggsave("figures/pred_hex_nuwakot.png", width = tw, height = tw)

library(patchwork)
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(paste0("figures/pred_hex_nuwakot_c_p", i, ".png"), width = tw, height = tw/2)

}
# ignore ------------------------------------------------------------------


if(FALSE){
  # Bagmati <- nepal_nc %>% filter(Province == "Bagmati")
  # landslides_bagmati <- st_intersection(landslides, Bagmati)
  
  nuwakot <- nepal_nc %>% filter(DISTRICT == "NUWAKOT")
  landslides_c_nuwakot <- st_intersection(landslides_c %>% st_transform(st_crs(nuwakot)), nuwakot)
  landslides_nuwakot <- st_intersection(landslides %>% st_transform(st_crs(nuwakot)), nuwakot)
  # landslides_nuwakot_buffer <- st_buffer(landslides_nuwakot, dist = .5)
  nuwakot_bnd <- st_union(st_as_sfc(nuwakot))
  
  pga_nuwakot <- pga_mean_raster$pga_mean_exp %>% 
    crop(nuwakot_bnd, extend = TRUE)
  writeRaster(pga_nuwakot, "./pga_nuwakot.tif", overwrite=TRUE)
  
  log_ksn_nuwakot <- log_ksn_tag$cop30dem_channel_tagged_pixels %>% 
    crop(nuwakot_bnd, extend = TRUE)
  writeRaster(log_ksn_nuwakot, "./log_ksn_nuwakot.tif", overwrite=TRUE)
  
  rainfall_nuwakot <- rainfall$precip_2015 %>% 
    crop(nuwakot_bnd, extend = TRUE)
  writeRaster(rainfall_nuwakot, "./rainfall_nuwakot.tif", overwrite=TRUE)
  
  
  fd2ch_nuwakot <- fd2ch["fd2ch_km"] %>% 
    crop(nuwakot_bnd, extend = TRUE)
  writeRaster(fd2ch_nuwakot, "./fd2ch_nuwakot.tif", overwrite=TRUE)

  
  # nuwakot_bnd_out <- fm_nonconvex_hull(st_union(st_as_sfc(nuwakot)), 0.1)
  
  hex_points <- fm_hexagon_lattice(bnd = nuwakot_bnd, edge_len = .5)
  
  mesh_hex <- fm_mesh_2d_inla(
    loc = hex_points,
    boundary = fm_extensions(nuwakot_bnd, c(.5,1)),
    max.edge = c(.4, .9),
    crs = fm_crs(nuwakot_bnd)
  )
  # ggplot() + geom_spatraster(data = pga_nuwakot) + gg(data = mesh_hex) + gg(data = landslides_nuwakot, aes(fill = log(Area_m2)))
  ggplot() + gg(data = mesh_hex) + gg(data = landslides_nuwakot, aes(fill = log(Area_m2)))
  ggsave("figures/mesh_hex_nuwakot.png", width = tw, height = tw)
  nuwakot_bnd_sf <- st_as_sf(nuwakot_bnd)
  st_geometry(nuwakot_bnd_sf) <- "geometry"
  
  
  save(mesh_hex, landslides_nuwakot, landslides_c_nuwakot, nuwakot_bnd_sf, pga_nuwakot, file = "lds_toy.Rdata")
  
}

if(FALSE){
  Koshi <- nepal_nc %>% filter(Province == "Koshi")
  landslides_koshi <- st_intersection(landslides, Koshi)
  
  koshi_bnd <- st_union(st_as_sfc(Koshi))
  # koshi_bnd_out <- fm_nonconvex_hull(st_union(st_as_sfc(Koshi)), 0.1)
  
  hex_points <- fm_hexagon_lattice(bnd = koshi_bnd, edge_len = 10)
  
  mesh_hex <- fm_mesh_2d_inla(
    loc = hex_points,
    boundary = fm_extensions(koshi_bnd, c(3,10)),
    max.edge = c(9, 20)
  )
  
  ggplot() + gg(data = mesh_hex) + gg(data = landslides_koshi, aes(color = log(Area_m2)))
  ggsave("figures/mesh_hex_koshi.png", width = tw/2, height = tw/2)
  
  # fm_extensions(bnd, c(
  #   nepal_lattice_sfc$edge_len,
  #   10 * nepal_lattice_sfc$edge_len
  # ))
  
  fml 
}

