cmp_test <- ~ ~ Intercept(1) +
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
  pga_mean_raster(pga_mean_raster["pga_mean"], model = "linear") +
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
  ksn_tag(log_ksn_tag,
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
  fd2fr_inv(1 / exp(fd2fr["fd2fr_km"]), model = "linear") 

# fita --------------------------------------------------------------------


if(FALSE){
  
  ls_a_score <- c(ls_a_score, "fit_test_a_score")
  mod_names_a <- c(mod_names_a, "fit_test_a")
  ls_a_crps <- c(ls_a_crps, "crps_fit_test_a")
  # fml_test_a <- geometry ~ Intercept + pga_mean_raster + landcover + geology + ksn_tag + rf2ch
  # fml_test_a <- geometry ~ Intercept + pga_mean_raster + landcover + geology + ksn_tag + rf2ch
  # fml_test_a <- geometry ~ Intercept + pga_mean_raster + landcover + geology + log_ksn_tag + rf2ch_rw2
  # |Model            |  AE_mean|  SE_mean|   DS_mean|  LS_mean| LS_mean_upper| LS_mean_lower| CRPS_mean|
  #   |:----------------|--------:|--------:|---------:|--------:|-------------:|-------------:|---------:|
  #   |fit1a            | 3.504912| 60.72082|  9.986448| 3.580087|           NaN|           NaN|  2.810710|
  #   |fit2a            | 3.427898| 58.84535| 10.224347| 3.586594|           NaN|           NaN|  2.740668|
  #   |fit3a            | 3.426205| 58.76790| 10.187398| 3.589914|           NaN|           NaN|  2.738588|
  #   |fit4a            | 4.069085| 72.69412| 12.300831| 3.579935|           NaN|           NaN|  3.318716|
  #   |fit5a            | 3.414972| 58.20205| 10.024009| 3.590472|           NaN|           NaN|  2.725711|
  #   |fit6a            | 3.386370| 57.49160|  9.948855| 3.589183|           NaN|           NaN|  2.698701|
  #   |fit_test_a_score | 3.634488| 64.74378| 10.450140| 3.563239|           NaN|           NaN|  2.924942|
  
  # fml_test_a <- geometry ~ Intercept + pga_mean_raster + landcover + geology + log_ksn_tag + rf2ch_inv
  # |Model      |  AE_mean|  SE_mean|   DS_mean|  LS_mean| LS_mean_upper| LS_mean_lower| CRPS_mean|
  #   |:----------|--------:|--------:|---------:|--------:|-------------:|-------------:|---------:|
  #   |fit1a      | 3.504912| 60.72082|  9.986448| 3.580087|           NaN|           NaN|  2.810710|
  #   |fit2a      | 3.427898| 58.84535| 10.224347| 3.586594|           NaN|           NaN|  2.740668|
  #   |fit3a      | 3.426205| 58.76790| 10.187398| 3.589914|           NaN|           NaN|  2.738588|
  #   |fit4a      | 4.069085| 72.69412| 12.300831| 3.579935|           NaN|           NaN|  3.318716|
  #   |fit5a      | 3.414972| 58.20205| 10.024009| 3.590472|           NaN|           NaN|  2.725711|
  #   |fit6a      | 3.386370| 57.49160|  9.948855| 3.589183|           NaN|           NaN|  2.698701|
  #   |fit_test_a | 3.634488| 64.74378| 10.450140| 3.563239|           NaN|           NaN|  2.924942|
    
  
  fml_test_a <- geometry ~ Intercept + pga_mean_raster + landcover + geology + ksn_tag + fd2ch
  # |Model      |  AE_mean|  SE_mean|   DS_mean|  LS_mean| LS_mean_upper| LS_mean_lower| CRPS_mean|
  #   |:----------|--------:|--------:|---------:|--------:|-------------:|-------------:|---------:|
  #   |fit1a      | 3.504912| 60.72082|  9.986448| 3.580087|           NaN|           NaN|  2.810710|
  #   |fit2a      | 3.427898| 58.84535| 10.224347| 3.586594|           NaN|           NaN|  2.740668|
  #   |fit3a      | 3.426205| 58.76790| 10.187398| 3.589914|           NaN|           NaN|  2.738588|
  #   |fit4a      | 4.069085| 72.69412| 12.300831| 3.579935|           NaN|           NaN|  3.318716|
  #   |fit5a      | 3.414972| 58.20205| 10.024009| 3.590472|           NaN|           NaN|  2.725711|
  #   |fit6a      | 3.386370| 57.49160|  9.948855| 3.589183|           NaN|           NaN|  2.698701|
  #   |fit_test_a | 3.485965| 61.01978| 10.365467| 3.586056|           NaN|           NaN|  2.793577|
  

  fml_test_a <- geometry ~ Intercept + landcover + geology + fd2ch
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
  
  fit_test_a <- {
    bru(
      components = cmp_, lik_test_a,
      options = list(
        bru_verbose = 3, bru_max_iter = 100
      )
    )
  }
  saveRDS(fit_test_a, file = here("RDS", "fit_test_a.RDS"))

  fit_test_a_score %<-% {
    score(fit_test_a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
  }
  
  crps_fit_test_a %<-% crps_(fit_test_a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
  
  scores_test <- bind_rows(
    bind_cols(f1a_score$mean_score, CRPS_mean = mean(crps1a)),
    bind_cols(f2a_score$mean_score, CRPS_mean = mean(crps2a)),
    bind_cols(f3a_score$mean_score, CRPS_mean = mean(crps3a)),
    bind_cols(f4a_score$mean_score, CRPS_mean = mean(crps4a)),
    bind_cols(f5a_score$mean_score, CRPS_mean = mean(crps5a)),
    bind_cols(f6a_score$mean_score, CRPS_mean = mean(crps6a)),
    bind_cols(fit_test_a_score$mean_score, CRPS_mean = mean(crps_fit_test_a))
  ) %>%
    bind_cols(data.frame(
      Model = mod_names_a
    ), .)
  
  print(knitr::kable(scores_test))
  

  df <- tibble::tibble(
    Model = rep(mod_names_a, each = nrow(cv_grid)),
    RMSE = as.vector(sapply(ls_a_score, function(x) {
      sqrt(get(x)$pred$obs_prob$SE)
    })),
    DS = as.vector(sapply(ls_a_score, function(x) {
      get(x)$pred$obs_prob$DS
    })),
    AE = as.vector(sapply(ls_a_score, function(x) {
      get(x)$pred$obs_prob$AE
    })),
    LS = as.vector(sapply(ls_a_score, function(x) {
      get(x)$pred$obs_prob$LS
    })),
    CRPS = as.vector(sapply(ls_a_crps, function(x) {
      unlist(get(x))
    })),
    geometry = rep(cv_grid$geometry, times = length(c(mod_names_a)))
  )
  
  df_ <- df %>%
    left_join(
      df %>%
        filter(Model == "fit1a") %>% # choose a ref baseline model
        select(geometry,
               AE_ref = AE,
               RMSE_ref = RMSE,
               DS_ref = DS,
               LS_ref = LS,
               CRPS_ref = CRPS
        ),
      by = c("geometry")
    ) %>%
    sf::st_as_sf()
  
  for (i in (c("AE","RMSE", "DS", "LS", "CRPS"))) {
    
    sfd <- scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      limits = get(paste0(i, "_diff_range")),
      values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5))
    )
    
    p1 <- ggplot() +
      gg(df_ %>% filter(Model == "fit1a"), lwd = 0, aes(fill = .data[[i]], col = .data[[i]])) +
      scale_fill_distiller(
        type = "seq",
        palette = "Reds",
        direction = 1
      ) +
      ggtitle(paste0(i, " scores")) +
      guides(col = "none", fill = guide_legend(i))
    
    
    p2 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit2a"),
        lwd = 0, aes( fill = .data[[i]] - .data[[paste0(i, "_ref")]], col =  .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit2a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref")))
    
    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit3a"),
        lwd = 0, aes( fill = .data[[i]] - .data[[paste0(i, "_ref")]], col =  .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit3a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref")))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit4a"),
        lwd = 0, aes( fill = .data[[i]] - .data[[paste0(i, "_ref")]], col =  .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit4a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref")))
    
    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit5a"),
        lwd = 0, aes( fill = .data[[i]] - .data[[paste0(i, "_ref")]], col =  .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit5a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref")))
    p6 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit6a"),
        lwd = 0, aes( fill = .data[[i]] - .data[[paste0(i, "_ref")]], col =  .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit6a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref")))
    p_test <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit_test_a"),
        lwd = 0, aes( fill = .data[[i]] - .data[[paste0(i, "_ref")]], col =  .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit_test ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref")))
    
    patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, p_test, nrow = 2, guides = "collect")
    ggsave(paste0("figures/model/cv/", i, cv_thin_resol[1], "_diff.png"), width = tw/1.2, height = tw / 3)
  }
  
  
  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    ggplot() +
      stat_ecdf(data = df_ %>% filter(Model == "fit1a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit1a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit2a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit2a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit3a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit3a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit4a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit4a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit5a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit5a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit6a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit6a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit_test_a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit_test", alpha = .5)) +
      # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
      # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      guides(alpha = "none") +
      xlab(i) + ylab(paste0("Empirical ", i, " CDF")) 
    ggsave(paste0("figures/model/cv/", i, "ecdf_", cv_thin_resol[1], ".pdf"), width = tw/2, height = tw/5)
  }
  }




# fitb --------------------------------------------------------------------
mod_names_b <- c(mod_names_b, "fit_test_b")
ls_b_score <- ls(pattern = "b_score")

fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + 
  landcover_ + geology_ + rf2ch + rf2fr
fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + 
  landcover_ + geology_ + fd2ch + fd2fr
fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + 
  landcover_ + geology_ + fd2fr # 1.17
fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + 
  landcover_ + geology_ + rf2ch_rw2 # 1.15
fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + 
  landcover_ + geology_ + fd2ch_rw2 # 1.15

fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + 
  landcover_ + geology_ + fd2ch_inv # 1.1372
fml_test_b <- logarea_m2 ~ Intercept + ksn_tag + 
  landcover_ + geology_ # 2.542961


fml_test_b <- logarea_m2 ~ Intercept + log_pga_mean_raster + rf2ch + 
  landcover_ + geology_ # 2.461936 

fml_test_b <- logarea_m2 ~ Intercept + fd2ch + 
  landcover_ + geology_ # 2.451588

fml_test_b <- logarea_m2 ~ Intercept + rf2ch + 
  landcover_ + geology_  # 2.453241

fml_test_b <- logarea_m2 ~ Intercept + rf2ch_rw2 + 
  landcover_ + geology_ # 2.464714

fml_test_b <- logarea_m2 ~ Intercept + dem + 
  landcover_ + geology_ # 2.466098

lik_test_b %<-% {
  bru_obs(
    formula = fml_test_b,
    family = "Gaussian",
    data = landslides_c
  )
}

fit_test_b %<-% {
  bru(
    components = cmp_, lik_test_b,
    options = list(
      bru_verbose = 3, bru_max_iter = 100
    )
  )
}
summary(fit_test_b)
# saveRDS(fit_test_b, file = here("RDS", "fit_test_b.RDS"))

fit_test_b_score <- {
  score(fit_test_b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
}




 dfb <- tibble::tibble(
    Model = rep(mod_names_b, each = nrow(landslides_c_test)),
    RMSE = as.vector(sapply(ls_b_score, function(x) {
      sqrt(get(x)$pred$obs_prob$SE)
    })),
    DS = as.vector(sapply(ls_b_score, function(x) {
      get(x)$pred$obs_prob$DS
    })),
    AE = as.vector(sapply(ls_b_score, function(x) {
      get(x)$pred$obs_prob$AE
    })),
    LS = as.vector(sapply(ls_b_score, function(x) {
      get(x)$pred$obs_prob$LS
    })),
    geometry = rep(1:nrow(landslides_c_test),times = length(mod_names_b))
  )

  dfb_ <- dfb %>%
    left_join(
    # bind_cols(
      # tidyr::uncount(
      dfb %>%
        filter(Model == "fit1b") %>% # choose a ref baseline model
        select(
          geometry,
          AE_ref = AE,
          RMSE_ref = RMSE,
          DS_ref = DS,
          LS_ref = LS
        ), 
      # length(mod_names_b)),
      by = c("geometry")
    )
  # %>% sf::st_as_sf()

i <- "LS"
ggplot() +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit1b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit1b", alpha = .5)) +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit2b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit2b", alpha = .5)) +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit3b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit3b", alpha = .5)) +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit4b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit4b", alpha = .5)) +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit5b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit5b", alpha = .5)) +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit6b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit6b", alpha = .5)) +
  stat_ecdf(data = dfb_ %>% filter(Model == "fit_test_b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "testb", alpha = .5)) +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  xlab(i) + ylab(paste0("Empirical ", i, " CDF"))
ggsave(paste0("figures/model/cv/", i, "ecdf_b", cv_thin_resol[1], "test.pdf"), width = tw/2, height = tw/5)


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
  # for (j in 1:length(fp_test_a)) {
  #   p_lst_test_a[[j]] <- ggplot() +
  #     gg(fp_test_a[[j]]["mean"], geom = "tile") +
  #     scale_fill_viridis_c(option = "C") +
  #     geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
  #     geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
  #     ggtitle(paste0(names(fp_test_a[j])))
  # }
  # wrap_plots(p_lst_test_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") +
  #   plot_annotation(paste0(fit_test_a$bru_info$lhoods[[1]]$formula[3]))
  # print(paste0(fit_test_a$bru_info$lhoods[[1]]$formula[c(2, 1, 3)]))
  # ggsave(paste0("figures/fp_test_a_contour.pdf"), width = tw, height = 1.25 * tw)
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
