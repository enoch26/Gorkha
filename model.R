# Modelling ---------------------------------------------------------------
library(future)
# TODO interaction term btw rf2ch and pga or rocktype
# https://github.com/inlabru-org/inlabru/discussions/173
# https://saturncloud.io/docs/examples/r/future/qs-r-future/
# https://medium.com/civis-analytics/programming-with-futures-in-r-401e906db384
# https://furrr.futureverse.org/articles/gotchas.html

# TODO Not including the problematic types from the sumtozero constraint might also help. One can do that with constr=FALSE and supplying an extraconstr argument.

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


if(JU){
  mchi <- read.csv(here("data", "lsdtt", fdr, "cop30dem_MChiSegmented.csv"), header = TRUE)
  mchi_sf <- st_as_sf(mchi, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs = crs_nepal) %>% st_intersection(bnd_out)
  
  matern <- inla.spde2.pcmatern(mesh_fm,
                                prior.range = c(4, 0.1),
                                prior.sigma = c(2, 0.1)
  )  
  
  cmp_mchi<- ~ mchi_field(main = geometry, model = matern)
  
  fml_mchi <- m_chi ~ mchi_field
  
  lik_mchi <- bru_obs("Gaussian",
                      formula = fml_mchi,
                      data = mchi_sf
  )
}


cmp_ <- ~ Intercept(1) +
  pga_mean_raster(pga_mean_raster["pga_mean_exp"], model = "linear") +
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
  mchi(pred_mchi_terra$rf2ch_mchi_1000, model = "linear") +
  mchi_(pred_mchi_terra_$rf2ch_mchi_1000_, model = "linear")
  # relief(dem_terrain_focal["relief"], model = "linear") +
  # relief2(dem_terrain_focal2["relief"], model = "linear") +
  # rf2ch(rf2ch["rf2ch_km"], model = "linear") +
  # twi(twi["twi"], model = "linear") +
  # beta_mchi(
  #   1,
  #   mean.linear = 0,
  #   prec.linear = 1,
  #   marginal = bru_mapper_marginal(qexp, rate = 1)
  # ) +
  # mchi_field(main = geometry, model = matern)
# slope(dem_terrain_mask["slope"], model = "linear") +
# asp(dem_terrain_mask["aspect"], model = matern_asp) +
#   asp_gp(dem_terrain_mask["asp_gp"], model = "rw2", cyclic=TRUE) +
# dem(dem["dem_km"], model = "linear") +
# crv_planform(crv_planform["crv_planform"], model = "linear") +
# crv_profile(crv_profile["crv_profile"], model = "linear")

# formula -----------------------------------------------------------------


fml1a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + mchi
fml1b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + mchi
fml2a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + mchi_
fml2b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + mchi_
if(JU){
  fml3a <- geometry ~ Intercept + landuse + nepal_geo + beta_mchi * mchi_field
  fml3b <- logarea_m2 ~ Intercept + landuse_ + nepal_geo_ + beta_mchi * mchi_field
}
# fml4a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + crv_planform + crv_profile
# fml4b <- logarea_m2 ~ Intercept + pga_mean_raster + landuse_ + nepal_geo_ + crv_planform + crv_profile
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

if(JU){
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

if (FALSE) {
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

if (file.exists(here("RDS", "fit1a.RDS"))) {
  fit1a %<-% {
    readRDS(here("RDS", "fit1a.RDS"))
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
  saveRDS(fit1a, file = here("RDS", "fit1a.RDS"))
}

if (file.exists(here("RDS", "fit1b.RDS"))) {
  fit1b %<-% {
    readRDS(here("RDS", "fit1b.RDS"))
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
  saveRDS(fit1b, file = here("RDS", "fit1b.RDS"))
}

# fit2 --------------------------------------------------------------------

if (file.exists(here("RDS", "fit2a.RDS"))) {
  fit2a %<-% {
    readRDS(here("RDS", "fit2a.RDS"))
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
  saveRDS(fit2a, file = here("RDS", "fit2a.RDS"))
}

if (file.exists(here("RDS", "fit2b.RDS"))) {
  fit2b %<-% {
    readRDS(here("RDS", "fit2b.RDS"))
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
  saveRDS(fit2b, file = here("RDS", "fit2b.RDS"))
}



### fit3 --------------------------------------------------------------------

if(JU){
  if (file.exists(here("RDS", "fit3a.RDS"))) {
    fit3a %<-% {
      readRDS(here("RDS", "fit3a.RDS"))
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
    saveRDS(fit3a, file = here("RDS", "fit3a.RDS"))
  }
  
  
  
  if (file.exists(here("RDS", "fit3b.RDS"))) {
    fit3b %<-% {
      readRDS(here("RDS", "fit3b.RDS"))
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
    saveRDS(fit3b, file = here("RDS", "fit3b.RDS"))
  }
}

if (FALSE) {
  ### fit4 --------------------------------------------------------------------


  if (file.exists(here("RDS", "fit4a.RDS"))) {
    fit4a %<-% {
      readRDS(here("RDS", "fit4a.RDS"))
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
    saveRDS(fit4a, file = here("RDS", "fit4a.RDS"))
  }


  if (file.exists(here("RDS", "fit4b.RDS"))) {
    fit4b %<-% {
      readRDS(here("RDS", "fit4b.RDS"))
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
    saveRDS(fit4b, file = here("RDS", "fit4b.RDS"))
  }


  ### fit5 --------------------------------------------------------------------


  if (file.exists(here("RDS", "fit5a.RDS"))) {
    fit5a %<-% {
      readRDS(here("RDS", "fit5a.RDS"))
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
    saveRDS(fit5a, file = here("RDS", "fit5a.RDS"))
  }



  if (file.exists(here("RDS", "fit5b.RDS"))) {
    fit5b %<-% {
      readRDS(here("RDS", "fit5b.RDS"))
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
    saveRDS(fit5b, file = here("RDS", "fit5b.RDS"))
  }

  ### fit6 --------------------------------------------------------------------


  if (file.exists(here("RDS", "fit6a.RDS"))) {
    fit6a %<-% {
      readRDS(here("RDS", "fit6a.RDS"))
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
    saveRDS(fit6a, file = here("RDS", "fit6a.RDS"))
  }

  if (file.exists(here("RDS", "fit6b.RDS"))) {
    fit6b %<-% {
      readRDS(here("RDS", "fit6b.RDS"))
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
    saveRDS(fit6b, file = here("RDS", "fit6b.RDS"))
  }



  ### fit7 --------------------------------------------------------------------


  if (file.exists(here("RDS", "fit7a.RDS"))) {
    fit7a %<-% {
      readRDS(here("RDS", "fit7a.RDS"))
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
    saveRDS(fit7a, file = here("RDS", "fit7a.RDS"))
  }


  if (file.exists(here("RDS", "fit7b.RDS"))) {
    fit7b %<-% {
      readRDS(here("RDS", "fit7b.RDS"))
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
    saveRDS(fit7b, file = here("RDS", "fit7b.RDS"))
  }


  ### fit8 --------------------------------------------------------------------


  if (file.exists(here("RDS", "fit8a.RDS"))) {
    fit8a %<-% {
      readRDS(here("RDS", "fit8a.RDS"))
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
    saveRDS(fit8a, file = here("RDS", "fit8a.RDS"))
  }



  if (file.exists(here("RDS", "fit8b.RDS"))) {
    fit8b %<-% {
      readRDS(here("RDS", "fit8b.RDS"))
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
    saveRDS(fit8b, file = here("RDS", "fit8b.RDS"))
  }
}

### fit9 --------------------------------------------------------------------


# if (file.exists(here("RDS", "fit9a.RDS"))) {
#   fit9a %<-% {
#     readRDS(here("RDS", "fit9a.RDS"))
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
#   saveRDS(fit9a, file = here("RDS", "fit9a.RDS"))
# }
#
#
#
# if (file.exists(here("RDS", "fit9b.RDS"))) {
#   fit9b %<-% {
#     readRDS(here("RDS", "fit9b.RDS"))
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
#   saveRDS(fit9b, file = here("RDS", "fit9b.RDS"))
# }


# # fit10 -------------------------------------------------------------------
#
# if (file.exists(here("RDS", "fit10a.RDS"))) {
#   fit10a %<-% {
#     readRDS(here("RDS", "fit10a.RDS"))
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
#   saveRDS(fit10a, file = here("RDS", "fit10a.RDS"))
# }
#
#
# if (file.exists(here("RDS", "fit10b.RDS"))) {
#   fit10b %<-% {
#     readRDS(here("RDS", "fit10b.RDS"))
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
#   saveRDS(fit10b, file = here("RDS", "fit10b.RDS"))
# }
#
# # fit11 -------------------------------------------------------------------
#
# if (file.exists(here("RDS", "fit11a.RDS"))) {
#   fit11a %<-% {
#     readRDS(here("RDS", "fit11a.RDS"))
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
#   saveRDS(fit11a, file = here("RDS", "fit11a.RDS"))
# }
#
#
#
# if (file.exists(here("RDS", "fit11b.RDS"))) {
#   fit11b %<-% {
#     readRDS(here("RDS", "fit11b.RDS"))
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
#   saveRDS(fit11b, file = here("RDS", "fit11b.RDS"))
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
