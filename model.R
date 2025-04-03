# Modelling ---------------------------------------------------------------
library(future)
# TODO interaction term btw rf2ch and pga or rocktype
# https://github.com/inlabru-org/inlabru/discussions/173
# https://saturncloud.io/docs/examples/r/future/qs-r-future/
# https://medium.com/civis-analytics/programming-with-futures-in-r-401e906db384
# https://furrr.futureverse.org/articles/gotchas.html

# TODO Not including the problematic types from the sumtozero constraint might also help. One can do that with constr=FALSE and supplying an extraconstr argument.

# TODO INLA 1D mesh with RW2 
# https://github.com/inlabru-org/inlabru/discussions/155
# https://groups.google.com/g/r-inla-discussion-group/c/CiA4l9zhCMw
max_ksn_tag <- minmax(log_ksn_tag$cop30dem_channel_tagged_pixels)[2]
knots <- seq(0, max_ksn_tag, length = 25)
mesh_ksn <- fm_mesh_1d(knots, interval = c(0, max_ksn_tag), degree = 2, boundary = "free")
ksn_tag_mapper <- bru_mapper(mesh_ksn, indexed = TRUE)

min_rf2ch <- minmax(rf2ch$rf2ch_km)[1]
max_rf2ch <- minmax(rf2ch$rf2ch_km)[2]
knots <- seq(min_rf2ch, max_rf2ch, length = 25)
mesh_rf2ch <- fm_mesh_1d(knots, interval = c(0, max_rf2ch), degree = 2, boundary = "free")
rf2ch_mapper <- bru_mapper(mesh_rf2ch, indexed = TRUE)

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

min_twi <- minmax(twi["logtwi"])[1]
max_twi <- minmax(twi["logtwi"])[2]
knots <- seq(min_twi, max_twi, length = 25)
mesh_twi <- fm_mesh_1d(knots, interval = c(min_twi, max_twi), degree = 2, boundary = "free")
twi_mapper <- bru_mapper(mesh_twi, indexed = TRUE)


hyper_rw <- list(prec = list(prior='pc.prec', param=c(1, 0.1))) 
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
#   nepal_geo["ROCK_TYPES"],
#   ips[1:10,],
#   eval_spatial(nepal_geo["ROCK_TYPES"], ips[1:10,])
# )

#   nepal_geo_ref(bru_fill_missing(
#     nepal_geo["ROCK_types_ref"],
#     .data.,
#     eval_spatial(nepal_geo["ROCK_types_ref"], .data.),
#     model = "linear"
#   ))
#
#
#   nepal_geo_ref(bru_fill_missing(
#     nepal_geo["ROCK_TYPES"],
#     ips[1:10,],
#     eval_spatial(nepal_geo["ROCK_TYPES"], ips[1:10,])
#     ),
#     model = "linear"
#   )

# extraconstr arguments for sum-to-zero constraints for the rock types with landslides
A_geo <- matrix(as.integer(levels(as.factor(nepal_geo$ROCK_TYPES)) %in% nepal_geo_ref), nrow = 1)
e_geo <- rep(0, 1)
A_lu <- matrix(as.integer(levels(as.factor(landuse$CODE1)) %in% landuse_ref), nrow = 1)
e_lu <- rep(0, 1)

# pga_sd_raster should be incorporate as unceratinty term instead
# pga_sd_raster(pga_std_raster["pga_std"], model = "generic0",
#                 Cmatrix = Diagonal(ncell(pga_mean_raster),
#                                    as.numeric(values(pga_std_raster$pga_std)))^{-2}) +


cmp_ <- ~ Intercept(1) +
  log_pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
  pga_mean_raster(pga_mean_raster["pga_mean_exp"], model = "linear") +
  log_pga_mean_raster_rw2(pga_mean_raster["pga_mean"], model = "rw2",
                  mapper = log_pga_mean_mapper, scale.model = TRUE, constr = TRUE,
  hyper = hyper_rw) +
  pga_mean_raster_rw2(pga_mean_raster["pga_mean_exp"], model = "rw2",
                      mapper = pga_mean_mapper, scale.model = TRUE, constr = TRUE,
                      hyper = hyper_rw) +
  # pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
  landuse(
    bru_fill_missing(
      landuse["CODE1"],
      .data.,
      eval_spatial(landuse["CODE1"], .data.)
    ),
    model = "iid", constr = F,
    extraconstr = list(A = A_lu, e = e_lu),
    hyper = hyper_iid
  ) +
  landuse_(
    bru_fill_missing(
      landuse["CODE1"],
      .data.,
      eval_spatial(landuse["CODE1"], .data.)
    ),
    model = "iid", constr = T,
    hyper = hyper_iid
  ) +
  nepal_geo(
    bru_fill_missing(
      nepal_geo["ROCK_TYPES"],
      .data.,
      eval_spatial(nepal_geo["ROCK_TYPES"], .data.)
    ),
    model = "iid", constr = F,
    extraconstr = list(A = A_geo, e = e_geo),
    hyper = hyper_iid
  ) +
  nepal_geo_(
    bru_fill_missing(
      nepal_geo["ROCK_TYPES"],
      .data.,
      eval_spatial(nepal_geo["ROCK_TYPES"], .data.)
    ),
    model = "iid", constr = T,
    # extraconstr = list(A = A, e = e),
    hyper = hyper_iid
  ) +
  nepal_geo_ref(
    bru_fill_missing(
      nepal_geo["ROCK_TYPES_ref"],
      .data.,
      eval_spatial(nepal_geo["ROCK_TYPES_ref"], .data.)
    ),
    model = "linear", prec.linear = 1e-6
  ) +
  landuse_ref(
    bru_fill_missing(
      landuse["CODE1_ref"],
      .data.,
      eval_spatial(landuse["CODE1_ref"], .data.)
    ),
    model = "linear", prec.linear = 1e-6
  ) +
  log_ksn_tag(log_ksn_tag, model = "linear") +
  ksn_tag(log_ksn_tag, model = "rw2", 
          mapper = ksn_tag_mapper, scale.model = TRUE, constr = TRUE,
          hyper = hyper_rw) +
  rf2ch(rf2ch["rf2ch_km"], model = "linear") +
  rf2ch_inv(1 / exp(rf2ch["rf2ch_km"]), model = "linear") +
  rf2ch_rw2(rf2ch["rf2ch_km"], model = "rw2",
            mapper = rf2ch_mapper, scale.model = TRUE, constr = TRUE,
            hyper = hyper_rw) +
  rf2ch_inv_rw2(1 / exp(rf2ch["rf2ch_km"]), model = "rw2",
                mapper = rf2ch_mapper, scale.model = TRUE, constr = TRUE,
                hyper = hyper_rw) +
  fd2ch(fd2ch["fd2ch_km"], model = "linear") +
  fd2ch_inv(1 / exp(fd2ch["fd2ch_km"]), model = "linear") +
  rf2fr(rf2fr["rf2fr_km"], model = "linear") +
  rf2fr_inv(1 / exp(rf2fr["rf2fr_km"]), model = "linear") +
  fd2fr(fd2fr["fd2fr_km"], model = "linear") +
  fd2fr_inv(1 / exp(fd2fr["fd2fr_km"]), model = "linear") +
  twi(twi["logtwi"], model = "linear")
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
# dem(dem["dem_km"], model = "linear") +
# crv_planform(crv_planform["crv_planform"], model = "linear") +
# crv_profile(crv_profile["crv_profile"], model = "linear")

# formula -----------------------------------------------------------------
fml1a <- geometry ~ Intercept + pga_mean_raster_rw2 + landuse + nepal_geo + ksn_tag 
fml1b <- logarea_m2 ~ Intercept + pga_mean_raster_rw2 + landuse_ + nepal_geo_ + ksn_tag 

fml2a <- geometry ~ Intercept + log_pga_mean_raster + landuse + nepal_geo + ksn_tag 
fml2b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag 

# not better than fml1
# fml2a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + rf2ch + rf2fr
# fml2b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + rf2ch + rf2fr
# fml2a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + fd2ch + fd2fr
# fml2b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + fd2ch + fd2fr

fml3a <- geometry ~ Intercept + pga_mean_raster_rw2 + landuse + nepal_geo + ksn_tag + rf2ch_inv 
fml3b <- logarea_m2 ~ Intercept + pga_mean_raster_rw2 + landuse_ + nepal_geo_ + ksn_tag + rf2ch_inv

fml4a <- geometry ~ Intercept + log_pga_mean_raster + landuse + nepal_geo + ksn_tag + rf2ch_inv
fml4b <- logarea_m2 ~ Intercept + log_pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + rf2ch_inv

# not as good as rf2ch
# fml4a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + fd2ch
# fml4b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + fd2ch

fml5a <- geometry ~ Intercept + pga_mean_raster_rw2 + landuse + nepal_geo + log_ksn_tag
fml5b <- logarea_m2 ~ Intercept + pga_mean_raster_rw2 + landuse_ + nepal_geo_ + log_ksn_tag 
 
# only add extra noises for sheer ksn model, make it worse
# fml4a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + twi
# fml4b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + twi
# not better than fml3
# fml4a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + rf2ch_inv + rf2fr_inv
# fml4b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + rf2ch_inv + rf2fr_inv

# fml4a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + fd2ch_inv + fd2fr_inv
# fml4b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + ksn_tag + fd2ch_inv + fd2fr_inv


# fml5a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + dem + asp
# fml5b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + dem + asp
# fml6a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + asp + crv_planform + crv_profile
# fml6b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + asp + crv_planform + crv_profile
# fml7a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + asp + crv_planform + crv_profile
# fml7b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + asp + crv_planform + crv_profile
# fml8a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + dem + asp + crv_planform + crv_profile
# fml8b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + dem + asp +crv_planform + crv_profile
# fml9a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + slope
# fml9b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse + nepal_geo + slope
# fml10a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch + dem
# fml10b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch + dem
# fml11a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch2 + dem
# fml11b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch2 + dem

# without intercept
# fml2a <- geometry ~ pga_mean_raster + nepal_geo
# fml2b <- logarea_m2 ~ pga_mean_raster + nepal_geo
# fml3a <- geometry ~ pga_mean_raster + landuse + nepal_geo
# fml3b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo
# fml4a <- geometry ~ pga_mean_raster + landuse + nepal_geo + rf2ch
# fml4b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + rf2ch
# fml5a <- geometry ~ pga_mean_raster + landuse + nepal_geo + rf2ch2
# fml5b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + rf2ch2
# fml6a <- geometry ~ pga_mean_raster + landuse + nepal_geo + rf2ch + asp
# fml6b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + rf2ch + asp
# fml7a <- geometry ~ pga_mean_raster + landuse + nepal_geo + rf2ch2 + asp
# fml7b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + rf2ch2 + asp
# fml8a <- geometry ~ pga_mean_raster + landuse + nepal_geo + slope
# fml8b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + slope
# fml9a <- geometry ~ pga_mean_raster + landuse + nepal_geo + slope + asp
# fml9b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + slope + asp
# fml10a <- geometry ~ pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch + dem
# fml10b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch + dem
# fml11a <- geometry ~ pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch2 + dem
# fml11b <- logarea_m2 ~ pga_mean_raster + landuse + nepal_geo + nepal_geo_ref + rf2ch2 + dem

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

if (JU) {
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
}

# if(UP){
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
# }

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
if (FALSE) {
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
}
# lik9a %<-% {
#   bru_obs(
#     formula = fml9a,
#     family = "cp",
#     data = landslides_c,
#     domain = list(
#       geometry = mesh_fm
#     ),
#     samplers = bnd
#   )
# }
# lik9b %<-% {
#   bru_obs(
#     formula = fml9b,
#     family = "Gaussian",
#     data = landslides_c
#   )
# }
# lik10a %<-% {
#   bru_obs(
#     formula = fml10a,
#     family = "cp",
#     data = landslides_c,
#     domain = list(
#       geometry = mesh_fm
#     ),
#     samplers = bnd
#   )
# }
#
# lik10b %<-% {
#   bru_obs(
#     formula = fml10b,
#     family = "Gaussian",
#     data = landslides_c
#   )
# }
#
# lik11a %<-% {
#   bru_obs(
#     formula = fml11a,
#     family = "cp",
#     data = landslides_c,
#     domain = list(
#       geometry = mesh_fm
#     ),
#     samplers = bnd
#   )
# }
# lik11b %<-% {
#   bru_obs(
#     formula = fml11b,
#     family = "Gaussian",
#     data = landslides_c
#   )
# }
# fit1 --------------------------------------------------------------------
# https://grantmcdermott.com/ds4e/parallel.html

if (file.exists(here("RDS", paste0("fit1a", nm_chess, ".RDS")))) {
  fit1a %<-% {
    readRDS(here("RDS", paste0("fit1a", nm_chess, ".RDS")))
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
  saveRDS(fit1a, file = here("RDS", paste0("fit1a", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", paste0("fit1b", nm_chess, ".RDS")))) {
  fit1b %<-% {
    readRDS(here("RDS", paste0("fit1b", nm_chess, ".RDS")))
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
  saveRDS(fit1b, file = here("RDS", paste0("fit1b", nm_chess, ".RDS")))
}

# fit2 --------------------------------------------------------------------

if (file.exists(here("RDS", paste0("fit2a", nm_chess, ".RDS")))) {
  fit2a %<-% {
    readRDS(here("RDS", paste0("fit2a", nm_chess, ".RDS")))
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
  saveRDS(fit2a, file = here("RDS", paste0("fit2a", nm_chess, ".RDS")))
}

if (file.exists(here("RDS", paste0("fit2b", nm_chess, ".RDS")))) {
  fit2b %<-% {
    readRDS(here("RDS", paste0("fit2b", nm_chess, ".RDS")))
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
  saveRDS(fit2b, file = here("RDS", paste0("fit2b", nm_chess, ".RDS")))
}



### fit3 --------------------------------------------------------------------

if (JU) {
  if (file.exists(here("RDS", paste0("fit3a", nm_chess, ".RDS")))) {
    fit3a %<-% {
      readRDS(here("RDS", paste0("fit3a", nm_chess, ".RDS")))
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
    saveRDS(fit3a, file = here("RDS", paste0("fit3a", nm_chess, ".RDS")))
  }



  if (file.exists(here("RDS", paste0("fit3b", nm_chess, ".RDS")))) {
    fit3b %<-% {
      readRDS(here("RDS", paste0("fit3b", nm_chess, ".RDS")))
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
    saveRDS(fit3b, file = here("RDS", paste0("fit3b", nm_chess, ".RDS")))
  }
}

# if(UP){
if (file.exists(here("RDS", paste0("fit3a", nm_chess, ".RDS")))) {
  fit3a %<-% {
    readRDS(here("RDS", paste0("fit3a", nm_chess, ".RDS")))
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
  saveRDS(fit3a, file = here("RDS", paste0("fit3a", nm_chess, ".RDS")))
}



if (file.exists(here("RDS", paste0("fit3b", nm_chess, ".RDS")))) {
  fit3b %<-% {
    readRDS(here("RDS", paste0("fit3b", nm_chess, ".RDS")))
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
  saveRDS(fit3b, file = here("RDS", paste0("fit3b", nm_chess, ".RDS")))
}
# }

### fit4 --------------------------------------------------------------------


if (file.exists(here("RDS", paste0("fit4a", nm_chess, ".RDS")))) {
  fit4a %<-% {
    readRDS(here("RDS", paste0("fit4a", nm_chess, ".RDS")))
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
  saveRDS(fit4a, file = here("RDS", paste0("fit4a", nm_chess, ".RDS")))
}


if (file.exists(here("RDS", paste0("fit4b", nm_chess, ".RDS")))) {
  fit4b %<-% {
    readRDS(here("RDS", paste0("fit4b", nm_chess, ".RDS")))
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
  saveRDS(fit4b, file = here("RDS", paste0("fit4b", nm_chess, ".RDS")))
}

  ### fit5 --------------------------------------------------------------------


  if (file.exists(here("RDS", paste0("fit5a", nm_chess, ".RDS")))) {
    fit5a %<-% {
      readRDS(here("RDS", paste0("fit5a", nm_chess, ".RDS")))
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
    saveRDS(fit5a, file = here("RDS", paste0("fit5a", nm_chess, ".RDS")))
  }



  if (file.exists(here("RDS", paste0("fit5b", nm_chess, ".RDS")))) {
    fit5b %<-% {
      readRDS(here("RDS", paste0("fit5b", nm_chess, ".RDS")))
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
    saveRDS(fit5b, file = here("RDS", paste0("fit5b", nm_chess, ".RDS")))
  }

  ### fit6 --------------------------------------------------------------------
if (FALSE) {


  if (file.exists(here("RDS", paste0("fit6a", nm_chess, ".RDS")))) {
    fit6a %<-% {
      readRDS(here("RDS", paste0("fit6a", nm_chess, ".RDS")))
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
    saveRDS(fit6a, file = here("RDS", paste0("fit6a", nm_chess, ".RDS")))
  }

  if (file.exists(here("RDS", paste0("fit6b", nm_chess, ".RDS")))) {
    fit6b %<-% {
      readRDS(here("RDS", paste0("fit6b", nm_chess, ".RDS")))
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
    saveRDS(fit6b, file = here("RDS", paste0("fit6b", nm_chess, ".RDS")))
  }



  ### fit7 --------------------------------------------------------------------


  if (file.exists(here("RDS", paste0("fit7a", nm_chess, ".RDS")))) {
    fit7a %<-% {
      readRDS(here("RDS", paste0("fit7a", nm_chess, ".RDS")))
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
    saveRDS(fit7a, file = here("RDS", paste0("fit7a", nm_chess, ".RDS")))
  }


  if (file.exists(here("RDS", paste0("fit7b", nm_chess, ".RDS")))) {
    fit7b %<-% {
      readRDS(here("RDS", paste0("fit7b", nm_chess, ".RDS")))
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
    saveRDS(fit7b, file = here("RDS", paste0("fit7b", nm_chess, ".RDS")))
  }


  ### fit8 --------------------------------------------------------------------


  if (file.exists(here("RDS", paste0("fit8a", nm_chess, ".RDS")))) {
    fit8a %<-% {
      readRDS(here("RDS", paste0("fit8a", nm_chess, ".RDS")))
    }
  } else {
    # system.time({
    fit8a %<-% {
      bru(
        components = cmp_, lik8a,
        options = list(
          bru_verbose = 3, bru_max_iter = 100
        )
      )
    }
    # })
    saveRDS(fit8a, file = here("RDS", paste0("fit8a", nm_chess, ".RDS")))
  }



  if (file.exists(here("RDS", paste0("fit8b", nm_chess, ".RDS")))) {
    fit8b %<-% {
      readRDS(here("RDS", paste0("fit8b", nm_chess, ".RDS")))
    }
  } else {
    system.time({
      fit8b %<-% {
        bru(
          components = cmp_, lik8b,
          options = list(
            bru_verbose = 3, bru_max_iter = 100
          )
        )
      }
    })
    saveRDS(fit8b, file = here("RDS", paste0("fit8b", nm_chess, ".RDS")))
  }
}

### fit9 --------------------------------------------------------------------


# if (file.exists(here("RDS", paste0("fit9a", nm_chess, ".RDS")))) {
#   fit9a %<-% {
#     readRDS(here("RDS", paste0("fit9a", nm_chess, ".RDS")))
#   }
# } else {
#   # system.time({
#   fit9a %<-% {
#     bru(
#       components = cmp_, lik9a,
#       options = list(
#         bru_verbose = 3, bru_max_iter = 100
#       )
#     )
#   }
#   # })
#   saveRDS(fit9a, file = here("RDS", paste0("fit9a", nm_chess, ".RDS")))
# }
#
#
#
# if (file.exists(here("RDS", paste0("fit9b", nm_chess, ".RDS")))) {
#   fit9b %<-% {
#     readRDS(here("RDS", paste0("fit9b", nm_chess, ".RDS")))
#   }
# } else {
#   # system.time({
#   fit9b %<-% {
#     bru(
#       components = cmp_, lik9b,
#       options = list(
#         bru_verbose = 3, bru_max_iter = 100
#       )
#     )
#   }
#   # })
#   saveRDS(fit9b, file = here("RDS", paste0("fit9b", nm_chess, ".RDS")))
# }


# # fit10 -------------------------------------------------------------------
#
# if (file.exists(here("RDS", paste0("fit10a", nm_chess, ".RDS")))) {
#   fit10a %<-% {
#     readRDS(here("RDS", paste0("fit10a", nm_chess, ".RDS")))
#   }
# } else {
#   # system.time({
#   fit10a %<-% {
#     bru(
#       components = cmp_, lik10a,
#       options = list(
#         bru_verbose = 3, bru_max_iter = 100
#       )
#     )
#   }
#   # })
#   saveRDS(fit10a, file = here("RDS", paste0("fit10a", nm_chess, ".RDS")))
# }
#
#
# if (file.exists(here("RDS", paste0("fit10b", nm_chess, ".RDS")))) {
#   fit10b %<-% {
#     readRDS(here("RDS", paste0("fit10b", nm_chess, ".RDS")))
#   }
# } else {
#   # system.time({
#   fit10b %<-% {
#     bru(
#       components = cmp_, lik10b,
#       options = list(
#         bru_verbose = 3, bru_max_iter = 100
#       )
#     )
#   }
#   # })
#   saveRDS(fit10b, file = here("RDS", paste0("fit10b", nm_chess, ".RDS")))
# }
#
# # fit11 -------------------------------------------------------------------
#
# if (file.exists(here("RDS", paste0("fit11a", nm_chess, ".RDS")))) {
#   fit11a %<-% {
#     readRDS(here("RDS", paste0("fit11a", nm_chess, ".RDS")))
#   }
# } else {
#   # system.time({
#   fit11a %<-% {
#     bru(
#       components = cmp_, lik11a,
#       options = list(
#         bru_verbose = 3, bru_max_iter = 100
#       )
#     )
#   }
#   # })
#   saveRDS(fit11a, file = here("RDS", paste0("fit11a", nm_chess, ".RDS")))
# }
#
#
#
# if (file.exists(here("RDS", paste0("fit11b", nm_chess, ".RDS")))) {
#   fit11b %<-% {
#     readRDS(here("RDS", paste0("fit11b", nm_chess, ".RDS")))
#   }
# } else {
#   # system.time({
#   fit11b %<-% {
#     bru(
#       components = cmp_, lik11b,
#       options = list(
#         bru_verbose = 3, bru_max_iter = 100
#       )
#     )
#   }
#   # })
#   saveRDS(fit11b, file = here("RDS", paste0("fit11b", nm_chess, ".RDS")))
# }
#

# run model with future ---------------------------------------------------


plan(sequential)


# Intensity Prediction ----------------------------------------------------


# pts <- st_sf(
#   geometry = st_sample(bnd,
#     type = "regular", # regular here because we want to have a accurate and smooth field realisation
#     size = 300 * 300
#   ), # either total size, or a numeric vector with sample sizes for each feature geometry.
#   crs = fm_crs(bnd)
# )


# model not working -------------------------------------------------------



# The joint mode is not where you wanna have, therefore it does not work
# if (JU) {
#   fml3a <- geometry ~ Intercept + landuse + nepal_geo + beta_mchi * mchi_field
#   fml3b <- geometry ~ Intercept + landuse + nepal_geo_ + beta_mchi * mchi_field
# }
# Have to specify the Cmatrix to make it work
# if (UP) {
#   fml3a <- geometry ~ Intercept + landuse + nepal_geo + beta_mchi * (mchi_near_ + cov_uncertainty)
#   fml3b <- logarea_m2 ~ Intercept + landuse_ + nepal_geo_ + beta_mchi * (mchi_near_ + cov_uncertainty)
# }
if(FALSE){
  
  if (JU) {
    # this defo not work Finn said
    mchi <- read.csv(here("data", "lsdtt", fdr, "cop30dem_MChiSegmented.csv"), header = TRUE)
    mchi_sf <- st_as_sf(mchi, coords = c("longitude", "latitude"), crs = 4326) %>%
      st_transform(crs = crs_nepal) %>%
      st_intersection(bnd_out)
    
    matern <- inla.spde2.pcmatern(mesh_fm,
                                  prior.range = c(4, 0.1),
                                  prior.sigma = c(2, 0.1)
    )
    
    cmp_mchi <- ~ mchi_field(main = geometry, model = matern)
    
    fml_mchi <- m_chi ~ mchi_field
    
    lik_mchi <- bru_obs("Gaussian",
                        formula = fml_mchi,
                        data = mchi_sf
    )
  }
}
