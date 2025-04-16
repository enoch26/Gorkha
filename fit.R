fmlb <- logarea_m2 ~ Intercept + log_pga_mean_raster_rw2 + landcover_ + nepal_geo_ + rf2ch
# sqrt_ksn_tag
likb <- {
  bru_obs(
    formula = fmlb,
    family = "Gaussian",
    data = landslides_c
  )
}

fitb <- {
  bru(
    components = cmp_, likb,
    options = list(
      bru_verbose = 3, bru_max_iter = 100
    )
  )
}

fb_score <- {
  score(fitb, newdata = landslides_c_test, obs = "logarea_m2",n.samples = 1000, seed = seed)
}