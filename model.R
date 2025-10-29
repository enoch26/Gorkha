# Modelling ---------------------------------------------------------------
library(future)

# create RDS folder to store results

if (!file.exists(here("RDS"))) {
  dir.create(here("RDS"))
}

if (!file.exists(here("RDS", trainset))) {
  dir.create(here("RDS", trainset))
}


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
# plan(multicore, workers = 10)


# PC prior for aspect -----------------------------------------------------

if (FALSE) {
  matern_asp %<-% {
    inla.spde2.pcmatern(mesh_asp,
      prior.range = c(2 * pi / seg, 0.1),
      prior.sigma = c(pi / (seg * 2), 0.1)
    )
  }
}
hyper_iid <- list(theta1 = list(prior = "pcprec", param = c(0.1, 0.5)))

# component --------------------------------------------------------------
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
  dem_inv(1/dem["dem_km"], model = "linear") +
  dem(dem["dem_km"], model = "linear") +
  slope(dem_terrain_mask["slope"], model = "linear") 

# formula -----------------------------------------------------------------
# fml1a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + log_ksn_tag_rw2
# fml1b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + log_ksn_tag_rw2

# 14072025 in the thesis
fml1a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + log_ksn_tag + rainfall_rw2
fml1b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + log_ksn_tag + rainfall

fml2a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + sqrt_ksn_tag + rainfall_rw2
fml2b <- logarea_m2 ~ Intercept + log_pga_mean_raster+ landcover_ + geology_ + sqrt_ksn_tag + rainfall

fml3a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + log_ksn_tag_rw2 + rainfall_rw2
fml3b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + log_ksn_tag_rw2 + rainfall

fml4a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + slope + rainfall_rw2
fml4b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + slope + rainfall

fml5a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + log_ksn_tag_rw2 + rf2ch_inv + rainfall_rw2
# fml5b <- logarea_m2 ~ Intercept + log_pga_mean_raster +landcover_ + geology_ + rf2ch + rainfall
fml5b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + log_ksn_tag_rw2 + rainfall + rf2ch

fml6a <- geometry ~ Intercept + pga_mean_raster_rw2 + landcover + geology + log_ksn_tag_rw2 + fd2ch_inv + rainfall_rw2
# fml6b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + fd2ch + rainfall
fml6b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landcover_ + geology_ + log_ksn_tag_rw2 + rainfall + fd2ch

lik1a %<-% {
  bru_obs(
    formula = fml1a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik1b %<-% {
  bru_obs(
    formula = fml1b,
    family = "Gaussian",
    data = landslides_c
  )
}
lik2a %<-% {
  bru_obs(
    formula = fml2a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik2b %<-% {
  bru_obs(
    formula = fml2b,
    family = "Gaussian",
    data = landslides_c
  )
}

lik3a %<-% {
  bru_obs(
    formula = fml3a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik3b %<-% {
  bru_obs(
    formula = fml3b,
    family = "Gaussian",
    data = landslides_c
  )
}

lik4a %<-% {
  bru_obs(
    formula = fml4a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik4b %<-% {
  bru_obs(
    formula = fml4b,
    family = "Gaussian",
    data = landslides_c
  )
}
lik5a %<-% {
  bru_obs(
    formula = fml5a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik5b %<-% {
  bru_obs(
    formula = fml5b,
    family = "Gaussian",
    data = landslides_c
  )
}

lik6a %<-% {
  bru_obs(
    formula = fml6a,
    family = "cp",
    data = landslides_c,
    domain = list(
      geometry = mesh_fm
    ),
    samplers = bnd
  )
}
lik6b %<-% {
  bru_obs(
    formula = fml6b,
    family = "Gaussian",
    data = landslides_c
  )
}


# fit1 --------------------------------------------------------------------

if (file.exists(here("RDS", trainset, paste0("fit1a", nm_chess, ".RDS")))) {
  fit1a %<-% {
    readRDS(here("RDS", trainset, paste0("fit1a", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit1a %<-% {
    bru(
      components = cmp_, lik1a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit1a, file = here("RDS", trainset, paste0("fit1a", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", trainset, paste0("fit1b", nm_chess, ".RDS")))) {
  fit1b %<-% {
    readRDS(here("RDS", trainset, paste0("fit1b", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit1b %<-% {
    bru(
      components = cmp_, lik1b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit1b, file = here("RDS", trainset, paste0("fit1b", nm_chess, ".RDS")))
}

# fit2 --------------------------------------------------------------------

if (file.exists(here("RDS", trainset, paste0("fit2a", nm_chess, ".RDS")))) {
  fit2a %<-% {
    readRDS(here("RDS", trainset, paste0("fit2a", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit2a %<-% {
    # fit2a <-
    bru(
      components = cmp_, lik2a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit2a, file = here("RDS", trainset, paste0("fit2a", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", trainset, paste0("fit2b", nm_chess, ".RDS")))) {
  fit2b %<-% {
    readRDS(here("RDS", trainset, paste0("fit2b", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit2b %<-% {
    bru(
      components = cmp_, lik2b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit2b, file = here("RDS", trainset, paste0("fit2b", nm_chess, ".RDS")))
}



### fit3 --------------------------------------------------------------------

if (JU) {
  if (file.exists(here("RDS", trainset, paste0("fit3a", nm_chess, ".RDS")))) {
    fit3a %<-% {
      readRDS(here("RDS", trainset, paste0("fit3a", nm_chess, ".RDS")))
    }
  } else {
    # system.time({
    fit3a %<-% {
      bru(
        components = cmp_, lik_mchi, lik3a,
        options = list(
          bru_verbose = 3, bru_max_iter = 100
        )
      )
    }
    # })
    saveRDS(fit3a, file = here("RDS", trainset, paste0("fit3a", nm_chess, ".RDS")))
  }



  if (file.exists(here("RDS", trainset, paste0("fit3b", nm_chess, ".RDS")))) {
    fit3b %<-% {
      readRDS(here("RDS", trainset, paste0("fit3b", nm_chess, ".RDS")))
    }
  } else {
    # system.time({
    fit3b %<-% {
      bru(
        components = cmp_, lik_mchi, lik3b,
        options = list(
          bru_verbose = 3, bru_max_iter = 100
        )
      )
    }
    # })
    saveRDS(fit3b, file = here("RDS", trainset, paste0("fit3b", nm_chess, ".RDS")))
  }
}

# if(UP){
if (file.exists(here("RDS", trainset, paste0("fit3a", nm_chess, ".RDS")))) {
  fit3a %<-% {
    readRDS(here("RDS", trainset, paste0("fit3a", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit3a %<-% {
    bru(
      components = cmp_, lik3a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit3a, file = here("RDS", trainset, paste0("fit3a", nm_chess, ".RDS")))
}



if (file.exists(here("RDS", trainset, paste0("fit3b", nm_chess, ".RDS")))) {
  fit3b %<-% {
    readRDS(here("RDS", trainset, paste0("fit3b", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit3b %<-% {
    bru(
      components = cmp_, lik3b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit3b, file = here("RDS", trainset, paste0("fit3b", nm_chess, ".RDS")))
}
# }

### fit4 --------------------------------------------------------------------


if (file.exists(here("RDS", trainset, paste0("fit4a", nm_chess, ".RDS")))) {
  fit4a %<-% {
    readRDS(here("RDS", trainset, paste0("fit4a", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit4a %<-% {
    bru(
      components = cmp_, lik4a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit4a, file = here("RDS", trainset, paste0("fit4a", nm_chess, ".RDS")))
}


if (file.exists(here("RDS", trainset, paste0("fit4b", nm_chess, ".RDS")))) {
  fit4b %<-% {
    readRDS(here("RDS", trainset, paste0("fit4b", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit4b %<-% {
    bru(
      components = cmp_, lik4b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit4b, file = here("RDS", trainset, paste0("fit4b", nm_chess, ".RDS")))
}

### fit5 --------------------------------------------------------------------


if (file.exists(here("RDS", trainset, paste0("fit5a", nm_chess, ".RDS")))) {
  fit5a %<-% {
    readRDS(here("RDS", trainset, paste0("fit5a", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit5a %<-% {
    bru(
      components = cmp_, lik5a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit5a, file = here("RDS", trainset, paste0("fit5a", nm_chess, ".RDS")))
}



if (file.exists(here("RDS", trainset, paste0("fit5b", nm_chess, ".RDS")))) {
  fit5b %<-% {
    readRDS(here("RDS", trainset, paste0("fit5b", nm_chess, ".RDS")))
  }
} else {
  # system.time({
  fit5b %<-% {
    bru(
      components = cmp_, lik5b,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  # })
  saveRDS(fit5b, file = here("RDS", trainset, paste0("fit5b", nm_chess, ".RDS")))
}

### fit6 --------------------------------------------------------------------

  if (file.exists(here("RDS", trainset, paste0("fit6a", nm_chess, ".RDS")))) {
    fit6a %<-% {
      readRDS(here("RDS", trainset, paste0("fit6a", nm_chess, ".RDS")))
    }
  } else {
    # system.time({
    fit6a %<-% {
      bru(
        components = cmp_, lik6a,
        options = list(
          bru_verbose = 3, bru_max_iter = 100
        )
      )
    }
    # })
    saveRDS(fit6a, file = here("RDS", trainset, paste0("fit6a", nm_chess, ".RDS")))
  }

  if (file.exists(here("RDS", trainset, paste0("fit6b", nm_chess, ".RDS")))) {
    fit6b %<-% {
      readRDS(here("RDS", trainset, paste0("fit6b", nm_chess, ".RDS")))
    }
  } else {
    # system.time({
    fit6b %<-% {
      bru(
        components = cmp_, lik6b,
        options = list(
          bru_verbose = 3, bru_max_iter = 100
        )
      )
    }
    # })
    saveRDS(fit6b, file = here("RDS", trainset, paste0("fit6b", nm_chess, ".RDS")))
  }


plan(sequential)

