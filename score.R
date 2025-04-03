# for CV ------------------------------------------------------------------
# https://stackoverflow.com/questions/75584181/how-to-layer-two-geom-sf-layers-in-ggplot-with-two-different-scale-fill-gradient
# https://inlabru-org.github.io/inlabru/articles/zip_zap_models.html

# TODO uncertainty SD of scores
# TODO merge count and count_test to fm_int(...) to compute log score with formula
# compute for grid_sf -----------------------------------------------------
if (CV_chess) {

  plan(multicore, workers = 3)

  if (file.exists(here("data", paste0("cv_newdata_chess", cv_chess_resol[1], ".RDS")))) {
    cv_newdata <- readRDS(here("data", paste0("cv_newdata_chess", cv_chess_resol[1], ".RDS")))
  } else {
    cv_newdata <- fm_int(mesh_fm, cv_grid$black)
    reorder <- order(cv_newdata$.block)
    cv_newdata <- cv_newdata[reorder, , drop = FALSE]
    saveRDS(cv_newdata, here("data", paste0("cv_newdata_chess", cv_chess_resol[1], ".RDS")))
  }
  f1a_score %<-% {
    score(fit1a, newdata = cv_newdata, cv_grid = cv_grid$black, obs = "count_test", seed = seed)
  }
  f2a_score %<-% {
    score(fit2a, newdata = cv_newdata, cv_grid = cv_grid$black, obs = "count_test", seed = seed)
  }
  f3a_score %<-% {
    score(fit3a, newdata = cv_newdata, cv_grid = cv_grid$black, obs = "count_test", seed = seed)
  }
  f4a_score %<-% {
    score(fit4a, newdata = cv_newdata, cv_grid = cv_grid$black, obs = "count_test", seed = seed)
  }
  f5a_score %<-% {
    score(fit5a, newdata = cv_newdata, cv_grid = cv_grid$black, obs = "count_test", seed = seed)
  }

  scores <- bind_rows(
    f1a_score$mean,
    f2a_score$mean,
    f3a_score$mean,
    f4a_score$mean,
    f5a_score$mean
  ) %>%
    bind_cols(data.frame(
      Model = c("fit1a", "fit2a", "fit3a", "fit4a", "fit5a")
    ), .)
  sink("figures/model/cv/score_chess.txt", append = TRUE)
  print("grid resolution :")
  print(paste0(cv_chess_resol, "km"))

  print(c(
    fit1a$bru_info$lhoods[[1]]$formula,
    fit2a$bru_info$lhoods[[1]]$formula,
    fit3a$bru_info$lhoods[[1]]$formula,
    fit4a$bru_info$lhoods[[1]]$formula,
    fit5a$bru_info$lhoods[[1]]$formula
  ))

  knitr::kable(scores)
  # |Model |  AE_mean|  SE_mean|  DS_mean|  LS_mean|
  #   |:-----|--------:|--------:|--------:|--------:|
  #   |fit1a | 48.43963| 11558.43| 149.0732| 40.94054|
  #   |fit2a | 48.69979| 11615.23| 143.3550| 41.04155|
  #   |fit3a | 48.60772| 11591.66| 146.9907| 41.02303|
  #   |fit4a | 48.82699| 11405.06| 103.4382| 40.15882|
  #   |fit5a | 49.17747| 11781.01| 148.8701| 41.40812|
    
  sink()
  
  df <- tibble::tibble(
    Model = rep(c("fit1a", "fit2a", "fit3a", "fit4a", "fit5a"), each = nrow(cv_grid$black)),
    SE = c(
      f1a_score$pred$obs_prob$SE,
      f2a_score$pred$obs_prob$SE,
      f3a_score$pred$obs_prob$SE,
      f4a_score$pred$obs_prob$SE,
      f5a_score$pred$obs_prob$SE
    ),
    DS = c(
      f1a_score$pred$obs_prob$DS,
      f2a_score$pred$obs_prob$DS,
      f3a_score$pred$obs_prob$DS,
      f4a_score$pred$obs_prob$DS,
      f5a_score$pred$obs_prob$DS
    ),
    AE = c(
      f1a_score$pred$obs_prob$AE,
      f2a_score$pred$obs_prob$AE,
      f3a_score$pred$obs_prob$AE,
      f4a_score$pred$obs_prob$AE,
      f5a_score$pred$obs_prob$AE
    ),
    LS = c(
      f1a_score$pred$obs_prob$LS,
      f2a_score$pred$obs_prob$LS,
      f3a_score$pred$obs_prob$LS,
      f4a_score$pred$obs_prob$LS,
      f5a_score$pred$obs_prob$LS
    ),
    geometry = rep(cv_grid$black$geometry, times = 5)
  )
  
  df_ <- df %>%
    left_join(
      df %>%
        filter(Model == "fit1a") %>% # choose a ref baseline model
        select(geometry,
               AE_ref = AE,
               SE_ref = SE,
               DS_ref = DS,
               LS_ref = LS
        ),
      by = c("geometry")
    ) %>%
    sf::st_as_sf()
  
  
  AE_range <- range(df$AE)
  SE_range <- range(df$SE)
  DS_range <- range(df$DS)
  LS_range <- range(df$LS)
  
  # df %>% select(-geometry) %>% group_by(Model) %>%
  #   summarise(AE_range = range(AE),
  #             SE_range = range(SE),
  #             DS_range = range(DS),
  #             LS_range = range(LS))
  
  for (i in (c("AE", "SE", "DS", "LS"))) {
    sfd <- scale_fill_distiller(type = "div", palette = "RdBu")
    # , limits = c(-5, 5)) +
    
    p1 <- ggplot() +
      gg(df_ %>% filter(Model == "fit1a"), aes(fill = DS)) +
      scale_fill_distiller(
        type = "seq",
        palette = "Reds",
        limits = DS_range,
        direction = 1
      ) +
      ggtitle(paste0(i, " scores")) +
      guides(fill = guide_legend(i))
    
    p2 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit2a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit2a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))
    
    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit3a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit3a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit4a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit4a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))
    
    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit5a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit5a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))
    
    patchwork::wrap_plots(p1, p2, p3, p4, p5, nrow = 3)
    ggsave(paste0("figures/model/cv/", i, "_chess",cv_chess_resol[1], "_diff.pdf"), width = tw, height = tw / 2)

  plan(sequential)
  }
}


# deprecated
if (FALSE) {
  
  fml <- fit1a$bru_info$lhoods[[1]]$formula
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  # log score
  formula <- as.formula(paste0(" ~ dpois(", obs, ", lambda = exp(", ff, "))"))

  # logsumexp mapper
  agg_nc <- bru_mapper_logsumexp(rescale = FALSE) # Poisson then its logsumexp
  # agg_nc <- bru_mapper_aggregate(rescale = FALSE)
  formula_ <- as.formula(paste0(" ~ ibm_eval(agg_nc,
                                      input = list(
                                        block = .block,
                                        weights = weight
                                      ),
                                      state = ", ff, ", log = FALSE)")) # because I predict lambda instead of log lambda

  pred <- {
    predict(fit1a, fm_int(mesh_fm, bnd_grid_sf),
      formula = formula_, n.samples = 10
    )
  }
}

# AE SE DS Log scores ---------------------------------------------------------------

if (CV_thin) {
  
  # TODO a for loop for 10 20 30 cv_thin_resol
  if(FALSE){
    cv_thin_resol <- c(20,20)
    cv_grid <- cv_partition(bnd,
                            resolution = cv_thin_resol,
                            chess = FALSE
    )
    
    cv_grid$count <- lengths(st_intersects(cv_grid, landslides_c))
    cv_grid$count_test <- lengths(st_intersects(cv_grid, landslides_c_test))
  }
  
  plan(multicore, workers = 3)

  # cv_newdata <- fm_int(mesh, cv_grid)
  # 30x30
  # user   system  elapsed
  # 1275.954   97.213 1381.935
  # reorder <- order(cv_newdata$.block)
  # cv_newdata <- cv_newdata[reorder, , drop  = FALSE]

  # plan(sequential)
  # profvis::profvis(fm_int(mesh_fm, cv_grid))
  if (file.exists(here("data", paste0("cv_newdata", cv_thin_resol[1], ".RDS")))) {
    cv_newdata <- readRDS(here("data", paste0("cv_newdata", cv_thin_resol[1], ".RDS")))
  } else {
    # it takes 1257 ish seconds for 30 x 30 km
    cv_newdata <- fm_int(mesh_fm, cv_grid)
    reorder <- order(cv_newdata$.block)
    cv_newdata <- cv_newdata[reorder, , drop = FALSE]
    saveRDS(cv_newdata, here("data", paste0("cv_newdata", cv_thin_resol[1], ".RDS")))
  }

  f1a_score %<-% {
    score(fit1a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", seed = seed)
  }
  f2a_score %<-% {
    score(fit2a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", seed = seed)
  }
  f3a_score %<-% {
    score(fit3a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", seed = seed)
  }
  f4a_score %<-% {
    score(fit4a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", seed = seed)
  }
  f5a_score %<-% {
    score(fit5a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", seed = seed)
  }

  scores <- bind_rows(
    f1a_score$mean,
    f2a_score$mean,
    f3a_score$mean,
    f4a_score$mean,
    f5a_score$mean
  ) %>%
    bind_cols(data.frame(
      Model = c("fit1a", "fit2a", "fit3a", "fit4a", "fit5a")
    ), .)
  
  
  sink("figures/model/cv/score_thin.txt", append = TRUE)
  print("grid resolution :")
  print(paste0(cv_thin_resol, "km"))

  print(c(
    fit1a$bru_info$lhoods[[1]]$formula,
    fit2a$bru_info$lhoods[[1]]$formula,
    fit3a$bru_info$lhoods[[1]]$formula,
    fit4a$bru_info$lhoods[[1]]$formula,
    fit5a$bru_info$lhoods[[1]]$formula
  ))

  knitr::kable(scores)
  
  # |Model |  AE_mean|  SE_mean|  DS_mean|  LS_mean|
  #   |:-----|--------:|--------:|--------:|--------:|
  #   |fit1a | 24.37915| 2353.090| 34.30276| 15.33691|
  #   |fit2a | 24.35120| 2353.692| 32.93723| 15.36613|
  #   |fit3a | 24.43777| 2368.820| 34.58073| 15.43489|
  #   |fit4a | 24.47052| 2368.274| 34.71571| 15.46664|
  #   |fit5a | 25.35734| 2489.714| 34.92244| 15.95969|
  sink()



  df <- tibble::tibble(
    Model = rep(c("fit1a", "fit2a", "fit3a", "fit4a", "fit5a"), each = nrow(cv_grid)),
    SE = c(
      f1a_score$pred$obs_prob$SE,
      f2a_score$pred$obs_prob$SE,
      f3a_score$pred$obs_prob$SE,
      f4a_score$pred$obs_prob$SE,
      f5a_score$pred$obs_prob$SE
    ),
    DS = c(
      f1a_score$pred$obs_prob$DS,
      f2a_score$pred$obs_prob$DS,
      f3a_score$pred$obs_prob$DS,
      f4a_score$pred$obs_prob$DS,
      f5a_score$pred$obs_prob$DS
    ),
    AE = c(
      f1a_score$pred$obs_prob$AE,
      f2a_score$pred$obs_prob$AE,
      f3a_score$pred$obs_prob$AE,
      f4a_score$pred$obs_prob$AE,
      f5a_score$pred$obs_prob$AE
    ),
    LS = c(
      f1a_score$pred$obs_prob$LS,
      f2a_score$pred$obs_prob$LS,
      f3a_score$pred$obs_prob$LS,
      f4a_score$pred$obs_prob$LS,
      f5a_score$pred$obs_prob$LS
    ),
    geometry = rep(cv_grid$geometry, times = 5)
  )

  df_ <- df %>%
    left_join(
      df %>%
        filter(Model == "fit1a") %>% # choose a ref baseline model
        select(geometry,
          AE_ref = AE,
          SE_ref = SE,
          DS_ref = DS,
          LS_ref = LS
        ),
      by = c("geometry")
    ) %>%
    sf::st_as_sf()


  AE_range <- range(df$AE)
  SE_range <- range(df$SE)
  DS_range <- range(df$DS)
  LS_range <- range(df$LS)

  # df %>% select(-geometry) %>% group_by(Model) %>%
  #   summarise(AE_range = range(AE),
  #             SE_range = range(SE),
  #             DS_range = range(DS),
  #             LS_range = range(LS))

  # TODO align scale?
  # se_sc <- scale_fill_viridis_c(
  #   limits = c(-200, 225),
  #   name = "SE",
  #   na.value = "transparent"
  # )
  for (i in (c("AE", "SE", "DS", "LS"))) {
    sfd <- scale_fill_distiller(type = "div", palette = "RdBu")
    # , limits = c(-5, 5)) +

    p1 <- ggplot() +
      gg(df_ %>% filter(Model == "fit1a"), aes(fill = DS)) +
      scale_fill_distiller(
        type = "seq",
        palette = "Reds",
        limits = DS_range,
        direction = 1
      ) +
      ggtitle(paste0(i, " scores")) +
      guides(fill = guide_legend(i))

    p2 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit2a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit2a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))

    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit3a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit3a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit4a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit4a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))

    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit5a"),
        aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit5a ", i, " score difference")) +
      guides(fill = guide_legend(paste0(i, "-", i, "_ref")))

    patchwork::wrap_plots(p1, p2, p3, p4, p5, nrow = 3)
    ggsave(paste0("figures/model/cv/", i, cv_thin_resol[1], "_diff.pdf"), width = tw, height = tw / 2)
  }

  plan(sequential)


  if (FALSE) {



    # TODO PIT

    df <- data.frame(
      count = rep(pxl$count, times = 5),
      pred_mean = c(
        f1a_score$pred$expect$mean,
        f2a_score$pred$expect$mean,
        f3a_score$pred$expect$mean,
        f4a_score$pred$expect$mean,
        f5a_score$pred$expect$mean
      ),
      pred_var = c(
        f1a_score$pred$expect$mean + f1a_score$pred$expect$sd^2,
        f2a_score$pred$expect$mean + f2a_score$pred$expect$sd^2,
        f3a_score$pred$expect$mean + f3a_score$pred$expect$sd^2,
        f4a_score$pred$expect$mean + f4a_score$pred$expect$sd^2,
        f5a_score$pred$expect$mean + f5a_score$pred$expect$sd^2
      ),
      pred_median = c(
        f1a_score$pred$expect$median,
        f2a_score$pred$expect$median,
        f3a_score$pred$expect$median,
        f4a_score$pred$expect$median,
        f5a_score$pred$expect$median
      ),
      log_score = c(
        f1a_score$pred$obs_prob$LS,
        f2a_score$pred$obs_prob$LS,
        f3a_score$pred$obs_prob$LS,
        f4a_score$pred$obs_prob$LS,
        f5a_score$pred$obs_prob$LS
      ),
      # pit = c(
      #   fit1a$cpo$pit * c(NA_real_, 1)[1 + (pxl$count > 0)],
      #   fit2a$cpo$pit * c(NA_real_, 1)[1 + (pxl$count > 0)],
      #   fit3a$cpo$pit * c(NA_real_, 1)[1 + (pxl$count > 0)],
      #   fit4a$cpo$pit * c(NA_real_, 1)[1 + (pxl$count > 0)],
      #   fit5a$cpo$pit * c(NA_real_, 1)[1 + (pxl$count > 0)]
      # ),
      Model = rep(c("fit1a", "fit2a", "fit3a", "fit4a", "fit5a"), each = nrow(pxl$count_test))
    )

    df <- df %>%
      mutate(
        AE = abs(count - pred_median),
        SE = (count - pred_mean)^2,
        DS = (count - pred_mean)^2 / pred_var + log(pred_var),
        LG = log_score
      )
    # summary(df %>% filter(DS < quantile(DS,.995)))
    scores <- df %>%
      # filter(DS > quantile(DS,.995)) %>%
      group_by(Model) %>%
      summarise(
        MAE = mean(AE),
        RMSE = sqrt(mean(SE)),
        MDS = mean(DS),
        MLG = mean(LG)
      ) %>%
      left_join(
        data.frame(
          Model = c("fit1a", "fit2a", "fit3a", "fit4a", "fit5a"),
          Order = 1:5
        ),
        by = "Model"
      ) %>%
      arrange(Order) %>%
      select(-Order)
    knitr::kable(scores)

    df <- df %>%
      tibble::as_tibble() %>%
      cbind(geometry = c(
        pxl$geometry,
        pxl$geometry,
        pxl$geometry,
        pxl$geometry,
        pxl$geometry
      ))
    df_ <- df %>%
      left_join(
        df %>%
          filter(Model == "fit1a") %>%
          select(geometry,
            AE_fit1a = AE,
            SE_fit1a = SE,
            DS_fit1a = DS,
            LG_fit1a = LG
          ),
        by = c("geometry")
      ) %>%
      sf::st_as_sf()

    # quantile(f1a_score$pred$obs_prob$DS, .999)

    p1 <- ggplot() +
      gg(data = log_ksn_tag$cop30dem_channel_tagged_pixels) +
      gg(data = f1a_score$pred$obs_prob["DS"] %>% filter(DS > 250), size = 0.1, col = "red") +
      # gg(df_ %>% filter(Model == "fit1a"), aes(fill = DS), geom = "tile") +
      # scale_fill_distiller(
      #   type = "seq",
      #   palette = "Reds",
      #   limits = c(250, 5259.8),
      #   # trans = "log",
      #   direction = 1,
      #   na.value = "transparent"
      # ) +
      # geom_sf(
      #   data = landslides_c_test,
      #   color = "firebrick",
      #   size = .01,
      #   pch = 4,
      #   alpha = 0.2
      # ) +
      ggtitle("Poisson Dawid-Sebastiani scores") +
      guides(fill = guide_legend("DS"))
    p1
    ggsave("fit1a_ds.pdf")

    p2 <- ggplot() +
      geom_fm(data = px_mesh) +
      gg(df_ %>% filter(Model == "ZIP"),
        aes(fill = DS - DS_Poisson),
        geom = "tile"
      ) +
      scale_fill_distiller(type = "div", palette = "RdBu", limits = c(-5, 5)) +
      geom_sf(data = nests, color = "firebrick", size = 1, pch = 4, alpha = 0.2) +
      ggtitle("ZIP Dawid-Sebastiani score difference") +
      guides(fill = guide_legend("DS-DS_poi"))
    p3 <- ggplot() +
      geom_fm(data = px_mesh) +
      gg(df_ %>% filter(Model == "ZAP"),
        aes(fill = DS - DS_Poisson),
        geom = "tile"
      ) +
      scale_fill_distiller(type = "div", palette = "RdBu", limits = c(-5, 5)) +
      geom_sf(data = nests, color = "firebrick", size = 1, pch = 4, alpha = 0.2) +
      ggtitle("ZAP Dawid-Sebastiani score difference") +
      guides(fill = guide_legend("DS-DS_poi"))

    patchwork::wrap_plots(p1, p2, p3, nrow = 1)
  }
}

if (FALSE) {
  fit1a_dse <- {
    dse_score(fit1a, pxl, n.samples = 100)
  }
  fit2a_dse <- {
    dse_score(fit2a, pxl, n.samples = 100)
  }
  fit3a_dse <- {
    dse_score(fit3a, pxl, n.samples = 100)
  }
  fit4a_dse <- {
    dse_score(fit4a, pxl, n.samples = 100)
  }
  dse_ls <- c("fit1a_dse", "fit2a_dse", "fit3a_dse", "fit4a_dse")


  for (i in dse_ls) {
    ggplot() +
      gg(data = get("pred", get(i))["SE_score"], geom = "tile") +
      scale_fill_viridis_c(trans = "log") +
      ggtitle("Squared Error Score")
    ggsave(paste0(i, "_se.pdf"), width = tw, height = tw / 2)
  }
  for (i in dse_ls) {
    ggplot() +
      gg(data = get("pred", get(i))["DS_score"], geom = "tile") +
      scale_fill_viridis_c(trans = "log") +
      ggtitle("Dawid-Sebastiani Score")
    ggsave(paste0(i, "_ds.pdf"), width = tw, height = tw / 2)
  }

  # TODO match each newdata log Area sizes
  if (FALSE) {
    fit1b_dse %<-% {
      dse_score(fit1b, pxl, n.samples = 100)
    }
    fit2b_dse %<-% {
      dse_score(fit2b, pxl, n.samples = 100)
    }
    fit3b_dse %<-% {
      dse_score(fit3b, pxl, n.samples = 100)
    }
    fit4b_dse %<-% {
      dse_score(fit4b, pxl, n.samples = 100)
    }
  }
}



if (FALSE) {
  pred_ <- predict(fit1a, newdata = pxl, formula = formula_, n.samples = 10)

  obs <- "count"
  post_E <- get("mean", pred_)
  post_Var <- post_E + (pred_$sd)^2
  # TODO this only applies to Poisson lambda/ Gaussian mean, count needa add post_E to the variance, other cases need to do case by case, forget it.
  pred_$SE_score <- SE_score <- (pxl$count - post_E)^2
  pred_$DS_score <- DS_score <- SE_score / post_Var + log(post_Var)

  SE_score_mean <- mean(SE_score, na.rm = TRUE)
  DS_score_mean <- mean(DS_score, na.rm = TRUE)
}


# fm_int
# formula_ <- as.formula(paste0(" ~ sum(weight * exp(", ff, "))")) # the whole region

# pred %<-% {
#   predict(fit1a,  fm_int(mesh_fm, bnd_grid_sf),
#           formula = formula_, n.samples = 1)
# }

# > class(pred["mean"])
# [1] "bru_prediction" "data.frame"
