cmp_test <- ~ Intercept(1) +
  pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
  relief(dem_terrain_focal2["relief"], model = "linear") +
  dem(dem["dem_km"], model = "linear")

fml_test_a <- geometry ~ Intercept + pga_mean_raster + relief + dem


lik_test_a %<-% {
  bru_obs(
    formula = fml_test_a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}

fit_test_a %<-% {
  bru(
    components = cmp_test, lik_test_a,
    options = list(
      bru_verbose = 3, bru_max_iter = 100
    )
  )
}
saveRDS(fit_test_a, file = here("RDS", "fit_test_a.RDS"))

cmp_test <- ~ Intercept(1) +
  pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
  relief(dem_terrain_focal2["relief"], model = "linear") +
  dem(dem["dem_km"], model = "linear")
fml_test_b <- logarea_m2 ~ Intercept + pga_mean_raster + relief + dem
lik_test_b %<-% {
  bru_obs(
    formula = fml_test_b,
    family = "Gaussian",
    data = landslides_c
  )
}

fit_test_b %<-% {
  bru(
    components = cmp_test, lik_test_b,
    options = list(
      bru_verbose = 3, bru_max_iter = 100
    )
  )
}
saveRDS(fit_test_b, file = here("RDS", "fit_test_b.RDS"))


fp_test_a %<-% {
  # pred_pxl_test_a <-
  predict(fit_test_a,
    newdata = pxl,
    formula = ~ list(
      lambda = exp(Intercept + pga_mean_raster + relief + dem),
      loglambda = Intercept + pga_mean_raster + relief + dem,
      pga_mean_raster = pga_mean_raster,
      relief = relief,
      dem = dem
    ),
    n.samples = 100, seed = seed[1]
  )
}

fp_test_b %<-% {
  # pred_pxl_test_b <-
  predict(fit_test_b,
    newdata = pxl,
    formula = ~ list(
      mu = Intercept + pga_mean_raster + relief + dem,
      pga_mean_raster = pga_mean_raster,
      relief = relief,
      dem = dem
    ),
    n.samples = 100, seed = seed[1]
  )
}

if (to_plot) {
  p_lst_test_a <- list()
  for (j in 1:length(fp_test_a)) {
    p_lst_test_a[[j]] <- ggplot() +
      gg(fp_test_a[[j]]["mean"], geom = "tile") +
      scale_fill_viridis_c(option = "C") +
      geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
      geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
      ggtitle(paste0(names(fp_test_a[j])))
  }
  wrap_plots(p_lst_test_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") +
    plot_annotation(paste0(fit_test_a$bru_info$lhoods[[1]]$formula[3]))
  print(paste(fit_test_a$bru_info$lhoods[[1]]$formula[c(2, 1, 3)]))
  ggsave(paste0("figures/fp_test_a.pdf"), width = tw, height = 1.25 * tw)
}

if (to_plot) {
  p_lst_test_a <- list()
  for (j in 1:length(fp_test_a)) {
    p_lst_test_a[[j]] <- ggplot() +
      gg(fp_test_a[[j]]["mean"], geom = "tile") +
      scale_fill_viridis_c(option = "C") +
      geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
      geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
      ggtitle(paste0(names(fp_test_a[j])))
  }
  wrap_plots(p_lst_test_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") +
    plot_annotation(paste0(fit_test_a$bru_info$lhoods[[1]]$formula[3]))
  print(paste0(fit_test_a$bru_info$lhoods[[1]]$formula[c(2, 1, 3)]))
  ggsave(paste0("figures/fp_test_a_contour.pdf"), width = tw, height = 1.25 * tw)
}

if (to_plot) {
  p_lst_test_b <- list()
  for (j in 1:length(fp_test_b)) {
    p_lst_test_b[[j]] <- ggplot() +
      gg(fp_test_b[[j]]["mean"], geom = "tile") +
      scale_fill_viridis_c(option = "C") +
      geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
      geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
      ggtitle(paste0(names(fp_test_b[j])))
  }
  wrap_plots(p_lst_test_b, ncol = 2) + # TODO fail to share legend scale, guides = "collect") +
    plot_annotation(paste0(fit_test_b$bru_info$lhoods[[1]]$formula[3]))
  print(paste(fit_test_b$bru_info$lhoods[[1]]$formula[c(2, 1, 3)]))
  ggsave(paste0("figures/fp_test_b.pdf"), width = tw, height = 1.25 * tw)
}

if (to_plot) {
  p_lst_test_b <- list()
  for (j in 1:length(fp_test_b)) {
    p_lst_test_b[[j]] <- ggplot() +
      gg(fp_test_b[[j]]["mean"], geom = "tile") +
      scale_fill_viridis_c(option = "C") +
      geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
      geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
      ggtitle(paste0(names(fp_test_b[j])))
  }
  wrap_plots(p_lst_test_b, ncol = 2) + # TODO fail to share legend scale, guides = "collect") +
    plot_annotation(paste0(fit_test_b$bru_info$lhoods[[1]]$formula[3]))
  print(paste0(fit_test_b$bru_info$lhoods[[1]]$formula[c(2, 1, 3)]))
  ggsave(paste0("figures/fp_test_b_contour.pdf"), width = tw, height = 1.25 * tw)
}

# tbd ---------------------------------------------------------------------


# ggplot() +
#   gg(fp_test_a[[1]]["mean"], geom = "tile") +
#   geom_sf(data = cllines) +
#   scale_fill_viridis_c(option = "C") +
#   geom_sf(data = bnd, col = "red", fill = NA)
# ggsave(paste0("figures/fp_test_a_lambda_kde2d_contour.pdf"), width = tw, height = tw)

ggplot() +
  gg(fp_test_a[[1]]["mean"], geom = "tile") +
  # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
  scale_fill_viridis_c(option = "C", alpha = 0.5) +
  geom_sf(data = lds_contour, fill = NA) + 
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave(paste0("figures/fp_test_a_lambda_den2d_contour.pdf"), width = tw, height = tw)

ggplot() +
  gg(fp_test_a[[1]]["mean"], geom = "tile") +
  scale_fill_viridis_c(option = "C",  alpha = 0.5) +
  geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave(paste0("figures/fp_test_a_lambda_eks_contour.pdf"), width = tw, height = tw)

ggplot() +
  gg(fp_test_a[[2]]["mean"], geom = "tile") +
  # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
  scale_fill_viridis_c(option = "C") +
  geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave(paste0("figures/fp_test_a_loglambda_eks_contour.pdf"), width = tw, height = tw)

ggplot() +
  gg(fp_test_a[[2]]["mean"], geom = "tile") +
  # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
  scale_fill_viridis_c(option = "C") +
  geom_sf(data = lds_contour, fill = NA) +
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave(paste0("figures/fp_test_a_loglambda_den2d_contour.pdf"), width = tw, height = tw)

# without contour and landslides location ---------------------------------


ggplot() +
  gg(fp_test_a[[1]]["mean"], geom = "tile") +
  scale_fill_viridis_c(option = "C") +
  geom_sf(data = nepal_bnd, col = "blue", fill = NA) +
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave(paste0("figures/fp_test_a_lambda.pdf"), width = tw, height = tw)
ggplot() +
  gg(fp_test_a[[2]]["mean"], geom = "tile") +
  scale_fill_viridis_c(option = "C") +
  geom_sf(data = nepal_bnd, col = "blue", fill = NA) +
  geom_sf(data = bnd, col = "red", fill = NA)
ggsave(paste0("figures/fp_test_a_loglambda.pdf"), width = tw, height = tw)
