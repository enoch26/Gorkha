# Modelling ---------------------------------------------------------------
library(future)
min_log_ksn_tag_rw2 <- minmax(log_ksn_tag$cop30dem_channel_tagged_pixels)[1]
max_log_ksn_tag_rw2 <- minmax(log_ksn_tag$cop30dem_channel_tagged_pixels)[2]
knots <- seq(0, max_log_ksn_tag_rw2, length = 25)
mesh_ksn <- fm_mesh_1d(knots, interval = c(0, max_log_ksn_tag_rw2), degree = 2, boundary = "free")
ksn_tag_mapper <- bru_mapper(mesh_ksn, indexed = TRUE)

min_sqrt_ksn_tag <- minmax(sqrt_ksn_tag$cop30dem_channel_tagged_pixels)[1]
max_sqrt_ksn_tag <- minmax(sqrt_ksn_tag$cop30dem_channel_tagged_pixels)[2]
knots <- seq(0, max_sqrt_ksn_tag, length = 25)
mesh_sqrt_ksn <- fm_mesh_1d(knots, interval = c(0, max_sqrt_ksn_tag), degree = 2, boundary = "free")
sqrt_ksn_tag_mapper <- bru_mapper(mesh_sqrt_ksn, indexed = TRUE)

min_rf2ch <- minmax(rf2ch$rf2ch_km)[1]
max_rf2ch <- minmax(rf2ch$rf2ch_km)[2]
knots <- seq(min_rf2ch, max_rf2ch, length = 25)
mesh_rf2ch <- fm_mesh_1d(knots, interval = c(0, max_rf2ch), degree = 2, boundary = "free")
rf2ch_mapper <- bru_mapper(mesh_rf2ch, indexed = TRUE)

min_fd2ch <- minmax(fd2ch$fd2ch_km)[1]
max_fd2ch <- minmax(fd2ch$fd2ch_km)[2]
knots <- seq(min_fd2ch, max_fd2ch, length = 25)
mesh_fd2ch <- fm_mesh_1d(knots, interval = c(0, max_fd2ch), degree = 2, boundary = "free")
fd2ch_mapper <- bru_mapper(mesh_fd2ch, indexed = TRUE)

min_rainfall_rw2 <- minmax(rainfall$precip_2015)[1]
max_rainfall_rw2 <- minmax(rainfall$precip_2015)[2]
knots <- seq(min_rainfall_rw2, max_rainfall_rw2, length = 25)
mesh_rainfall <- fm_mesh_1d(knots, interval = c(min_rainfall_rw2, max_rainfall_rw2), degree = 2, boundary = "free")
rainfall_mapper <- bru_mapper(mesh_rainfall, indexed = TRUE)

# mw78
min_pga_mean_raster_rw2 <- minmax(pga_mean_raster["pga_mean_exp"])[1]
max_pga_mean_raster_rw2 <- minmax(pga_mean_raster["pga_mean_exp"])[2]
knots <- seq(min_pga_mean_raster_rw2, max_pga_mean_raster_rw2, length = 25)
mesh_pga_mean <- fm_mesh_1d(knots, interval = c(min_pga_mean_raster_rw2, max_pga_mean_raster_rw2), degree = 2, boundary = "free")
pga_mean_mapper <- bru_mapper(mesh_pga_mean, indexed = TRUE)

min_log_pga_mean_raster_rw2 <- minmax(pga_mean_raster["pga_mean"])[1]
max_log_pga_mean_raster_rw2 <- minmax(pga_mean_raster["pga_mean"])[2]
knots <- seq(min_log_pga_mean_raster_rw2, max_log_pga_mean_raster_rw2, length = 25)
mesh_log_pga_mean <- fm_mesh_1d(knots, interval = c(min_log_pga_mean_raster_rw2, max_log_pga_mean_raster_rw2), degree = 2, boundary = "free")
log_pga_mean_mapper <- bru_mapper(mesh_log_pga_mean, indexed = TRUE)

# merge
min_pga_mean_raster_merge_rw2 <- minmax(pga_mean_raster_merge["pga_mean_exp"])[1]
max_pga_mean_raster_merge_rw2 <- minmax(pga_mean_raster_merge["pga_mean_exp"])[2]
knots <- seq(min_pga_mean_raster_merge_rw2, max_pga_mean_raster_merge_rw2, length = 25)
mesh_pga_mean_merge <- fm_mesh_1d(knots, interval = c(min_pga_mean_raster_merge_rw2, max_pga_mean_raster_merge_rw2), degree = 2, boundary = "free")
pga_mean_merge_mapper <- bru_mapper(mesh_pga_mean_merge, indexed = TRUE)

min_log_pga_mean_raster_merge_rw2 <- minmax(pga_mean_raster_merge["pga_mean"])[1]
max_log_pga_mean_raster_merge_rw2 <- minmax(pga_mean_raster_merge["pga_mean"])[2]
knots <- seq(min_log_pga_mean_raster_merge_rw2, max_log_pga_mean_raster_merge_rw2, length = 25)
mesh_log_pga_mean_merge <- fm_mesh_1d(knots, interval = c(min_log_pga_mean_raster_merge_rw2, max_log_pga_mean_raster_merge_rw2), degree = 2, boundary = "free")
log_pga_mean_merge_mapper <- bru_mapper(mesh_log_pga_mean_merge, indexed = TRUE)
# mw73
min_pga_mean_raster_mw73_rw2 <- minmax(pga_mean_raster_mw73["pga_mean_exp"])[1]
max_pga_mean_raster_mw73_rw2 <- minmax(pga_mean_raster_mw73["pga_mean_exp"])[2]
knots <- seq(min_pga_mean_raster_mw73_rw2, max_pga_mean_raster_mw73_rw2, length = 25)
mesh_pga_mean_mw73 <- fm_mesh_1d(knots, interval = c(min_pga_mean_raster_mw73_rw2, max_pga_mean_raster_mw73_rw2), degree = 2, boundary = "free")
pga_mean_mw73_mapper <- bru_mapper(mesh_pga_mean_mw73, indexed = TRUE)

min_log_pga_mean_raster_mw73_rw2 <- minmax(pga_mean_raster_mw73["pga_mean"])[1]
max_log_pga_mean_raster_mw73_rw2 <- minmax(pga_mean_raster_mw73["pga_mean"])[2]
knots <- seq(min_log_pga_mean_raster_mw73_rw2, max_log_pga_mean_raster_mw73_rw2, length = 25)
mesh_log_pga_mean_mw73 <- fm_mesh_1d(knots, interval = c(min_log_pga_mean_raster_mw73_rw2, max_log_pga_mean_raster_mw73_rw2), degree = 2, boundary = "free")
log_pga_mean_mw73_mapper <- bru_mapper(mesh_log_pga_mean_mw73, indexed = TRUE)

min_log_pga_mean_raster_merge_rw2 <- minmax(pga_mean_raster_merge["pga_mean"])[1]
max_log_pga_mean_raster_merge_rw2 <- minmax(pga_mean_raster_merge["pga_mean"])[2]
knots <- seq(min_log_pga_mean_raster_merge_rw2, max_log_pga_mean_raster_merge_rw2, length = 25)
mesh_log_pga_mean_merge <- fm_mesh_1d(knots, interval = c(min_log_pga_mean_raster_merge_rw2, max_log_pga_mean_raster_merge_rw2), degree = 2, boundary = "free")
log_pga_mean_merge_mapper <- bru_mapper(mesh_log_pga_mean_merge, indexed = TRUE)

if(FALSE){
  min_twi <- minmax(twi["logtwi"])[1]
  max_twi <- minmax(twi["logtwi"])[2]
  knots <- seq(min_twi, max_twi, length = 25)
  mesh_twi <- fm_mesh_1d(knots, interval = c(min_twi, max_twi), degree = 2, boundary = "free")
  twi_mapper <- bru_mapper(mesh_twi, indexed = TRUE)
}

hyper_rw <- list(prec = list(prior = "pc.prec", param = c(1, 0.1)))
# plan(multicore, workers = 20)
plan(multicore, workers = 10)


# PC prior for aspect -----------------------------------------------------

if (FALSE) {
  matern_asp %<-% {
    inla.spde2.pcmatern(mesh_asp,
      prior.range = c(2 * pi / seg, 0.1),
      prior.sigma = c(pi / (seg * 2), 0.1)
    )
  }
}
# https://haakonbakkagit.github.io/btopic134.html
## Dynamic hyperparameter, with a prior for log precision that has median 0.1
hyper_iid <- list(theta1 = list(prior = "pcprec", param = c(0.1, 0.5)))

# componenet --------------------------------------------------------------

# Notes: Finn fix bru_fill_missing for integration points
#
# bru_fill_missing(
#   geology["ROCK_TYPES"],
#   ips[1:10,],
#   eval_spatial(geology["ROCK_TYPES"], ips[1:10,])
# )

#   geology_ref(bru_fill_missing(
#     geology["ROCK_types_ref"],
#     .data.,
#     eval_spatial(geology["ROCK_types_ref"], .data.),
#     model = "linear"
#   ))
#
#
#   geology_ref(bru_fill_missing(
#     geology["ROCK_TYPES"],
#     ips[1:10,],
#     eval_spatial(geology["ROCK_TYPES"], ips[1:10,])
#     ),
#     model = "linear"
#   )

# extraconstr arguments for sum-to-zero constraints for the rock types with landslides
A_geo <- matrix(as.integer(levels(as.factor(geology$ROCK_TYPES)) %in% geology_ref), nrow = 1)
e_geo <- rep(0, 1)
A_lu <- matrix(as.integer(levels(as.factor(landcover$CODE1)) %in% landcover_ref), nrow = 1)
e_lu <- rep(0, 1)

cmp_ <- ~ Intercept(1) +
  log_pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
  pga_mean_raster(pga_mean_raster["pga_mean_exp"], model = "linear") +
  log_pga_mean_raster_rw2(pga_mean_raster["pga_mean"],
    model = "rw2",
    mapper = log_pga_mean_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  pga_mean_raster_rw2(pga_mean_raster["pga_mean_exp"],
    model = "rw2",
    mapper = pga_mean_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  log_pga_mean_raster_merge(pga_mean_raster_merge["pga_mean"], model = "linear") +
  pga_mean_raster_merge(pga_mean_raster_merge["pga_mean_exp"], model = "linear") +
  log_pga_mean_raster_merge_rw2(pga_mean_raster_merge["pga_mean"],
    model = "rw2",
    mapper = log_pga_mean_merge_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  pga_mean_raster_merge_rw2(pga_mean_raster["pga_mean_exp"],
    model = "rw2",
    mapper = pga_mean_merge_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  log_pga_mean_raster_mw73(pga_mean_raster_mw73["pga_mean"], model = "linear") +
  pga_mean_raster_mw73(pga_mean_raster_mw73["pga_mean_exp"], model = "linear") +
  log_pga_mean_raster_mw73_rw2(pga_mean_raster_mw73["pga_mean"],
    model = "rw2",
    mapper = log_pga_mean_mw73_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  pga_mean_raster_mw73_rw2(pga_mean_raster["pga_mean_exp"],
    model = "rw2",
    mapper = pga_mean_mw73_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  # pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
  landcover(
    bru_fill_missing(
      landcover["CODE1"],
      .data.,
      eval_spatial(landcover["CODE1"], .data.)
    ),
    model = "iid", constr = F,
    extraconstr = list(A = A_lu, e = e_lu),
    hyper = hyper_iid
  ) +
  su_rf(su_hima["relief_avg"], model = "linear") +
  su_twi(su_hima["twi_avg"], model = "linear") +
  su_elev(su_hima["elev_avg"], model = "linear") +
  su_slope(su_hima["slope_avg"], model = "linear") +
  su_annual_rain(su_hima["annual_rain"], model = "linear") +
  su_clusterA(su_hima["cluster_su_A"], model = "factor_contrast") +
  su_clusterB(su_hima["cluster_su_B"], model = "factor_contrast") +
  rainfall(rainfall$precip_2015, model = "linear") +
  rainfall_rw2(rainfall$precip_2015,
            model = "rw2",
            mapper = rainfall_mapper, scale.model = TRUE, constr = TRUE,
            hyper = hyper_rw
  ) +
  landcover_(
    bru_fill_missing(
      landcover["CODE1"],
      .data.,
      eval_spatial(landcover["CODE1"], .data.)
    ),
    model = "iid", constr = T,
    hyper = hyper_iid
  ) +
  geology(
    bru_fill_missing(
      geology["ROCK_TYPES"],
      .data.,
      eval_spatial(geology["ROCK_TYPES"], .data.)
    ),
    model = "iid", constr = F,
    extraconstr = list(A = A_geo, e = e_geo),
    hyper = hyper_iid
  ) +
  geology_(
    bru_fill_missing(
      geology["ROCK_TYPES"],
      .data.,
      eval_spatial(geology["ROCK_TYPES"], .data.)
    ),
    model = "iid", constr = T,
    # extraconstr = list(A = A, e = e),
    hyper = hyper_iid
  ) +
  geology_ref(
    bru_fill_missing(
      geology["ROCK_TYPES_ref"],
      .data.,
      eval_spatial(geology["ROCK_TYPES_ref"], .data.)
    ),
    model = "linear", prec.linear = 1e-6
  ) +
  landcover_ref(
    bru_fill_missing(
      landcover["CODE1_ref"],
      .data.,
      eval_spatial(landcover["CODE1_ref"], .data.)
    ),
    model = "linear", prec.linear = 1e-6
  ) +
  log_ksn_tag(log_ksn_tag, model = "linear") +
  log_ksn_tag_rw2(log_ksn_tag,
    model = "rw2",
    mapper = ksn_tag_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  sqrt_ksn_tag(sqrt_ksn_tag,
    model = "rw2",
    mapper = sqrt_ksn_tag_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  rf2ch(rf2ch["rf2ch_km"], model = "linear") +
  rf2ch_inv(1 / exp(rf2ch["rf2ch_km"]), model = "linear") +
  rf2ch_rw2(rf2ch["rf2ch_km"],
    model = "rw2",
    mapper = rf2ch_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  rf2ch_inv_rw2(1 / exp(rf2ch["rf2ch_km"]),
    model = "rw2",
    mapper = rf2ch_mapper, scale.model = TRUE, constr = TRUE,
    hyper = hyper_rw
  ) +
  fd2ch(fd2ch["fd2ch_km"], model = "linear") +
  fd2ch_inv(1 / exp(fd2ch["fd2ch_km"]), model = "linear") +
  rf2fr(rf2fr["rf2fr_km"], model = "linear") +
  rf2fr_inv(1 / exp(rf2fr["rf2fr_km"]), model = "linear") +
  fd2fr(fd2fr["fd2fr_km"], model = "linear") +
  fd2fr_inv(1 / exp(fd2fr["fd2fr_km"]), model = "linear") +
  # twi(twi["logtwi"], model = "linear") +
  # twi(twi["logtwi"], model = "rw2",
  #     mapper = twi_mapper, scale.model = TRUE, constr = TRUE)
  # mchi_(pred_mchi_terra_$rf2ch_mchi_1000_, model = "linear") +
  # mchi_near(mchi_terra_near$fd2ch_log_mchi, model = "linear") +
  # mchi_near_(eval_spatial(mchi_terra_near$fd2ch_log_mchi, geometry), model = "const") +
  # mchi_near_1000(mchi_terra_near$fd2ch_mchi_1000, model = "linear") +
  # relief(dem_terrain_focal["relief"], model = "linear") +
  # relief2(dem_terrain_focal2["relief"], model = "linear") +
  # beta_mchi(
  #   1,
  #   mean.linear = 0,
  #   prec.linear = 1,
  #   marginal = bru_mapper_marginal(qexp, rate = 1)
  # ) +
  # cov_uncertainty(main = geometry, mapper = bru_get_mapper(matern), model = matern)
  # bru_get_mapper(matern)
  # model = "generic0", Cmatrix = fit_qqsd01 # I do not have the Cmatrix
  # mchi_field(main = geometry, model = matern)
  # slope(dem_terrain_mask["slope"], model = "linear") +
  # asp(dem_terrain_mask["aspect"], model = matern_asp) +
  #   asp_gp(dem_terrain_mask["asp_gp"], model = "rw2", cyclic=TRUE) +
  dem_inv(1/dem["dem_km"], model = "linear") +
  dem(dem["dem_km"], model = "linear") +
  slope(dem_terrain_mask["slope"], model = "linear") 

# crv_planform(crv_planform["crv_planform"], model = "linear") +
# crv_profile(crv_profile["crv_profile"], model = "linear")

# formula -----------------------------------------------------------------
fml7a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + log_ksn_tag + rainfall
fml7b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + log_ksn_tag + rainfall

fml8a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + su_annual_rain
fml8b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + su_annual_rain

fml9a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + su_rf + su_annual_rain
fml9b <- logarea_m2 ~ Intercept + log_pga_mean_raster  + landcover_ + geology_ + su_rf + su_annual_rain

fml10a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + su_twi + su_annual_rain
fml10b <- logarea_m2 ~ Intercept + log_pga_mean_raster  + landcover_ + geology_ + su_twi + su_annual_rain

fml11a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + su_slope + su_annual_rain
fml11b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + su_slope + su_annual_rain

fml12a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + su_clusterA + su_annual_rain
fml12b <- logarea_m2 ~ Intercept + log_pga_mean_raster +landcover_ + geology_ + su_clusterA + su_annual_rain


lik7a %<-% {
  bru_obs(
    formula = fml7a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik7b %<-% {
  bru_obs(
    formula = fml7b,
    family = "Gaussian",
    data = landslides_c
  )
}
lik8a %<-% {
  bru_obs(
    formula = fml8a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik8b %<-% {
  bru_obs(
    formula = fml8b,
    family = "Gaussian",
    data = landslides_c
  )
}

# if(UP){
lik9a %<-% {
  bru_obs(
    formula = fml9a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik9b %<-% {
  bru_obs(
    formula = fml9b,
    family = "Gaussian",
    data = landslides_c
  )
}
# }

lik10a %<-% {
  bru_obs(
    formula = fml10a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik10b %<-% {
  bru_obs(
    formula = fml10b,
    family = "Gaussian",
    data = landslides_c
  )
}
lik11a %<-% {
  bru_obs(
    formula = fml11a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik11b %<-% {
  bru_obs(
    formula = fml11b,
    family = "Gaussian",
    data = landslides_c
  )
}




  lik12a %<-% {
    bru_obs(
      formula = fml12a,
      family = "cp",
      data = landslides_c,
      domain = list(
        geometry = mesh_fm
      ),
      samplers = bnd
    )
  }
  lik12b %<-% {
    bru_obs(
      formula = fml12b,
      family = "Gaussian",
      data = landslides_c
    )
  }
# fit --------------------------------------------------------------------
if (file.exists(here("RDS", trainset, paste0("fit7a", nm_chess, ".RDS")))) {
  fit7a %<-% {
    readRDS(here("RDS", trainset, paste0("fit7a", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit7a %<-% {
    bru(
      components = cmp_, lik7a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit7a, file = here("RDS", trainset, paste0("fit7a_su", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", trainset, paste0("fit7b_su", nm_chess, ".RDS")))) {
  fit7b %<-% {
    readRDS(here("RDS", trainset, paste0("fit7b_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit7b %<-% {
    bru(
      components = cmp_, lik7b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit7b, file = here("RDS", trainset, paste0("fit7b_su", nm_chess, ".RDS")))
}

# fit2 --------------------------------------------------------------------

if (file.exists(here("RDS", trainset, paste0("fit8a_su", nm_chess, ".RDS")))) {
  fit8a %<-% {
    readRDS(here("RDS", trainset, paste0("fit8a_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit8a %<-% {
    # fit8a <-
    bru(
      components = cmp_, lik8a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit8a, file = here("RDS", trainset, paste0("fit8a_su", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", trainset, paste0("fit8b_su", nm_chess, ".RDS")))) {
  fit8b %<-% {
    readRDS(here("RDS", trainset, paste0("fit8b_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit8b %<-% {
    bru(
      components = cmp_, lik8b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit8b, file = here("RDS", trainset, paste0("fit8b_su", nm_chess, ".RDS")))
}



### fit3 --------------------------------------------------------------------

# if(UP){
if (file.exists(here("RDS", trainset, paste0("fit9a_su", nm_chess, ".RDS")))) {
  fit9a %<-% {
    readRDS(here("RDS", trainset, paste0("fit9a_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit9a %<-% {
    bru(
      components = cmp_, lik9a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit9a, file = here("RDS", trainset, paste0("fit9a_su", nm_chess, ".RDS")))
}



if (file.exists(here("RDS", trainset, paste0("fit9b_su", nm_chess, ".RDS")))) {
  fit9b %<-% {
    readRDS(here("RDS", trainset, paste0("fit9b_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit9b %<-% {
    bru(
      components = cmp_, lik9b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit9b, file = here("RDS", trainset, paste0("fit9b_su", nm_chess, ".RDS")))
}
# }

### fit4 --------------------------------------------------------------------


if (file.exists(here("RDS", trainset, paste0("fit10a_su", nm_chess, ".RDS")))) {
  fit10a %<-% {
    readRDS(here("RDS", trainset, paste0("fit10a_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit10a %<-% {
    bru(
      components = cmp_, lik10a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit10a, file = here("RDS", trainset, paste0("fit10a_su", nm_chess, ".RDS")))
}


if (file.exists(here("RDS", trainset, paste0("fit10b_su", nm_chess, ".RDS")))) {
  fit10b %<-% {
    readRDS(here("RDS", trainset, paste0("fit10b_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit10b %<-% {
    bru(
      components = cmp_, lik10b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit10b, file = here("RDS", trainset, paste0("fit10b_su", nm_chess, ".RDS")))
}

### fit5 --------------------------------------------------------------------


if (file.exists(here("RDS", trainset, paste0("fit11a_su", nm_chess, ".RDS")))) {
  fit11a %<-% {
    readRDS(here("RDS", trainset, paste0("fit11a_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit11a %<-% {
    bru(
      components = cmp_, lik11a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit11a, file = here("RDS", trainset, paste0("fit11a_su", nm_chess, ".RDS")))
}



if (file.exists(here("RDS", trainset, paste0("fit11b_su", nm_chess, ".RDS")))) {
  fit11b %<-% {
    readRDS(here("RDS", trainset, paste0("fit11b_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit11b %<-% {
    bru(
      components = cmp_, lik11b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit11b, file = here("RDS", trainset, paste0("fit11b_su", nm_chess, ".RDS")))
}

### fit6 --------------------------------------------------------------------

if (file.exists(here("RDS", trainset, paste0("fit12a_su", nm_chess, ".RDS")))) {
  fit12a %<-% {
    readRDS(here("RDS", trainset, paste0("fit12a_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit12a %<-% {
    bru(
      components = cmp_, lik12a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit12a, file = here("RDS", trainset, paste0("fit12a_su", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", trainset, paste0("fit12b_su", nm_chess, ".RDS")))) {
  fit12b %<-% {
    readRDS(here("RDS", trainset, paste0("fit12b_su", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit12b %<-% {
    bru(
      components = cmp_, lik12b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit12b, file = here("RDS", trainset, paste0("fit12b_su", nm_chess, ".RDS")))
}



# run model with future ---------------------------------------------------


plan(sequential)
