# for CV ------------------------------------------------------------------
# https://stackoverflow.com/questions/75584181/how-to-layer-two-geom-sf-layers-in-ggplot-with-two-different-scale-fill-gradient
# https://inlabru-org.github.io/inlabru/articles/zip_zap_models.html
# power for the colour scale, see sfd
pwr <- 8
sc <- seq(0, 1, length.out = 20)

mod_names_a <- c(ls(pattern = "fit.a"), ls(pattern = "fit..a"))
mod_names_b <- c(ls(pattern = "fit.b"), ls(pattern = "fit..b"))
# pred_names_a <- ls(pattern = "fp.a")
# pred_names_b <- ls(pattern = "fp.b")

# TODO uncertainty SD of scores
# TODO merge count and count_test to fm_int(...) to compute log score with formula
# compute for grid_sf CV_chess -----------------------------------------------------
if (CV_chess) {
  # plan(multicore, workers = 6)
  cv_grid_test <- cv_grid[[test]]

  cv_grid_test$.block <- 1:nrow(cv_grid_test)

  if (file.exists(here("data", paste0("cv_newdata_", train, cv_chess_resol[1], ".RDS")))) {
    cv_newdata <- readRDS(here("data", paste0("cv_newdata_", train, cv_chess_resol[1], ".RDS")))
  } else {
    cv_newdata <- fm_int(mesh_fm, cv_grid_test)
    reorder <- order(cv_newdata$.block)
    cv_newdata <- cv_newdata[reorder, , drop = FALSE]
    saveRDS(cv_newdata, here("data", paste0("cv_newdata_", train, cv_chess_resol[1], ".RDS")))
  }

  # cv_newdata <- cv_newdata %>% left_join(data.frame(cv_grid_test %>% st_drop_geometry() %>%
  #                                                     select(.block, count_test)),
  #                                        by = ".block")



  ## fita --------------------------------------------------------------------


  if (file.exists(here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))) {
    df_ <- readRDS(here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))
  } else {
    f7a_score <- {
      score(fit7a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f8a_score <- {
      score(fit8a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f9a_score <- {
      score(fit9a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f10a_score <- {
      score(fit10a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f11a_score <- {
      score(fit11a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f12a_score <- {
      score(fit12a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }

    plan(sequential)
    plan(multicore, workers = 6)

    crps7a <- crps_(fit7a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps8a <- crps_(fit8a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps9a <- crps_(fit9a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps10a <- crps_(fit10a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps11a <- crps_(fit11a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps12a <- crps_(fit12a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)

    scores_a <- bind_rows(
      bind_cols(f7a_score$mean_score, CRPS_mean = mean(crps7a)),
      bind_cols(f8a_score$mean_score, CRPS_mean = mean(crps8a)),
      bind_cols(f9a_score$mean_score, CRPS_mean = mean(crps9a)),
      bind_cols(f10a_score$mean_score, CRPS_mean = mean(crps10a)),
      bind_cols(f11a_score$mean_score, CRPS_mean = mean(crps11a)),
      bind_cols(f12a_score$mean_score, CRPS_mean = mean(crps12a))
    ) %>%
      bind_cols(data.frame(
        Model = mod_names_a
      ), .)
    sink(paste0("figures/model/cv/score_", train, ".txt"), append = TRUE)
    print(Sys.time())
    print(paste0("train dataset: ", train))
    print("grid resolution :")
    print(paste0(cv_chess_resol, "km"))
    sink()
    sink(paste0("figures/model/cv/score_", train, ".txt"), append = TRUE)
    print(c(
      fit7a$bru_info$lhoods[[1]]$formula,
      fit8a$bru_info$lhoods[[1]]$formula,
      fit9a$bru_info$lhoods[[1]]$formula,
      fit10a$bru_info$lhoods[[1]]$formula,
      fit11a$bru_info$lhoods[[1]]$formula,
      fit12a$bru_info$lhoods[[1]]$formula
    ))
    sink()
    sink(paste0("figures/model/cv/score_", train, ".txt"), append = TRUE)
    print(knitr::kable(scores_a))
    sink()
    sink(paste0("figures/model/cv/score_", train, ".txt"), append = TRUE)
    print(knitr::kable(scores_a, "latex"))
    sink()

    if (exists("ls_a_score")) {
      rm(ls_a_score)
    }
    ls_a_score <- c(ls(pattern = "f.a_score"), ls(pattern = "f..a_score"))

    df <- tibble::tibble(
      Model = rep(mod_names_a, each = nrow(cv_grid_test)),
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
      CRPS = c(crps7a, crps8a, crps9a, crps10a, crps11a, crps12a),
      geometry = rep(cv_grid_test$geometry, times = length(mod_names_a))
    )

    df_ <- df %>%
      left_join(
        df %>%
          filter(Model == "fit7a") %>% # choose a ref baseline model
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


    saveRDS(df_, file = here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))
  }

  AE_range <- range(df$AE - df_$AE_ref)
  RMSE_range <- range(df$RMSE - df_$RMSE_ref)
  DS_range <- range(df$DS - df_$DS_ref)
  LS_range <- range(df$LS - df_$LS_ref)
  CRPS_range <- range(df$CRPS - df_$CRPS_ref)

  AE_diff_range <- c(-1, 1) * max(abs(AE_range))
  RMSE_diff_range <- c(-1, 1) * max(abs(RMSE_range))
  DS_diff_range <- c(-1, 1) * max(abs(DS_range))
  LS_diff_range <- c(-1, 1) * max(abs(LS_range))
  CRPS_diff_range <- c(-1, 1) * max(abs(CRPS_range))

  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    sfd <- scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      limits = get(paste0(i, "_diff_range")),
      values = scales::rescale(abs(2 * sc - 1)^pwr * sign(sc - 0.5))
    )

    p1 <- ggplot() +
      gg(df_ %>% filter(Model == "fit7a"), lwd = 0, aes(fill = .data[[i]])) +
      scale_fill_distiller(
        type = "seq",
        palette = "Reds",
        direction = 1
      ) +
      ggtitle(paste0(i, " scores")) +
      # labs(col = "", fill = i)
      guides(col = "none", fill = guide_legend(i))

    p2 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit8a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit8a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit9a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit9a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit10a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit10a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit11a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit11a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p6 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit12a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit12a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, nrow = 2, guides = "collect")

    ggsave(paste0("figures/model/cv/", i, "_", train, cv_chess_resol[1], "_diff_", trainset, ".png"), width = tw / 1.2, height = tw / 3)
  }


  ### ECDF --------------------------------------------------------------------


  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    ggplot() +
      stat_ecdf(data = df_ %>% filter(Model == "fit7a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit7a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit8a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit8a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit9a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit9a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit10a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit10a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit11a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit11a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit12a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit12a", alpha = .5)) +
      # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      guides(alpha = "none") +
      xlab(i) +
      ylab(paste0(i, " ECDF"))
    ggsave(paste0("figures/model/cv/", i, "ecdf_", train, cv_chess_resol[1], ".pdf"), width = tw / 2, height = tw / 5)
  }

  ## fitb --------------------------------------------------------------------

  if (file.exists(here("RDS", trainset, paste0("dfb_score_", nm_chess, ".RDS")))) {
    dfb_ <- readRDS(here("RDS", trainset, paste0("dfb_score_", nm_chess, ".RDS")))
  } else {
    f7b_score <- {
      score(fit7b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f8b_score <- {
      score(fit8b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f9b_score <- {
      score(fit9b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f10b_score <- {
      score(fit10b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f11b_score <- {
      score(fit11b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f12b_score <- {
      score(fit12b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    ls_b_score <- ls(pattern = "b_score")
    # crps7b <-crps_(fit7b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps8b <-crps_(fit8b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps9b <-crps_(fit9b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps10b <-crps_(fit10b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps11b <-crps_(fit11b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps12b <-crps_(fit12b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)

    scores_b <- bind_rows(
      f7b_score$mean_score,
      f8b_score$mean_score,
      f9b_score$mean_score,
      f10b_score$mean_score,
      f11b_score$mean_score,
      f12b_score$mean_score
      # bind_cols(f7b_score$mean_score, CRPS_mean = mean(crps7b)),
      # bind_cols(f8b_score$mean_score, CRPS_mean = mean(crps8b)),
      # bind_cols(f9b_score$mean_score, CRPS_mean = mean(crps9b)),
      # bind_cols(f10b_score$mean_score, CRPS_mean = mean(crps10b)),
      # bind_cols(f11b_score$mean_score, CRPS_mean = mean(crps11b)),
      # bind_cols(f12b_score$mean_score, CRPS_mean = mean(crps12b))
    ) %>%
      bind_cols(data.frame(
        Model = mod_names_b
      ), .)
    sink(paste0("figures/model/cv/score_", train, "_b.txt"), append = TRUE)
    print(Sys.time())
    print(paste0("train dataset: ", train))
    print("grid resolution :")
    print(paste0(cv_chess_resol, "km"))
    print(c(
      fit7b$bru_info$lhoods[[1]]$formula,
      fit8b$bru_info$lhoods[[1]]$formula,
      fit9b$bru_info$lhoods[[1]]$formula,
      fit10b$bru_info$lhoods[[1]]$formula,
      fit11b$bru_info$lhoods[[1]]$formula,
      fit12b$bru_info$lhoods[[1]]$formula
    ))
    print(knitr::kable(scores_b))
    print(knitr::kable(scores_b, "latex"))
    sink()

    ## ECDF --------------------------------------------------------------------


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
      geometry = rep(1:nrow(landslides_c_test), times = length(mod_names_b))
    )

    dfb_ <- dfb %>%
      left_join(
        dfb %>%
          filter(Model == "fit7b") %>% # choose a ref baseline model
          select(
            geometry,
            AE_ref = AE,
            RMSE_ref = RMSE,
            DS_ref = DS,
            LS_ref = LS
          ),
        by = c("geometry")
      )

    saveRDS(dfb_, file = here("RDS", trainset, paste0("dfb_score_", nm_chess, ".RDS")))
  }


  for (i in (c("AE", "RMSE", "DS", "LS"))) {
    ggplot() +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit7b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit7b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit8b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit8b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit9b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit9b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit10b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit10b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit11b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit11b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit12b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit12b", alpha = .5)) +
      # stat_ecdf(data = dfb_ %>% filter(Model == "fit_test_b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "testb", alpha = .5)) +
      # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      guides(alpha = "none") +
      xlab(i) +
      ylab(paste0(i, " ECDF"))
    ggsave(paste0("figures/model/cv/", i, "ecdf_b", train, cv_chess_resol[1], ".pdf"), width = tw / 2, height = tw / 5)
  }

  plan(sequential)
}

# CV Thin ---------------------------------------------------------------

if (CV_thin) {
  # in case one wanna change the resolution again to check
  if (is.null(cv_thin_resol)) {
    cv_thin_resol <- c(3, 3)
  }

  cv_grid <- cv_partition(bnd,
    resolution = cv_thin_resol,
    chess = FALSE
  )

  cv_grid$count <- lengths(st_intersects(cv_grid, landslides_c))
  cv_grid$count_test <- lengths(st_intersects(cv_grid, landslides_c_test))
  cv_grid$.block <- 1:nrow(cv_grid)

    if (file.exists(here("data", paste0("cv_newdata", cv_thin_resol[1], ".RDS")))) {
    cv_newdata <- readRDS(here("data", paste0("cv_newdata", cv_thin_resol[1], ".RDS")))
  } else {
    # it takes 1257 ish seconds for 30 x 30 km
    cv_newdata <- fm_int(mesh_fm, cv_grid)
    reorder <- order(cv_newdata$.block)
    cv_newdata <- cv_newdata[reorder, , drop = FALSE]
    saveRDS(cv_newdata, here("data", paste0("cv_newdata", cv_thin_resol[1], ".RDS")))
  }

  ## fita --------------------------------------------------------------------

  if (file.exists(here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))) {
    df_ <- readRDS(here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))
  } else {
 
    f7a_score <- {
      score(fit7a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 10, seed = seed)
    }
    f7a_score <- {
      score(fit7a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f8a_score <- {
      score(fit8a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f9a_score <- {
      score(fit9a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f10a_score <- {
      score(fit10a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f11a_score <- {
      score(fit11a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f12a_score <- {
      score(fit12a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }

    plan(sequential)

    crps7a <- crps_(fit7a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps8a <- crps_(fit8a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps9a <- crps_(fit9a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps10a <- crps_(fit10a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps11a <- crps_(fit11a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps12a <- crps_(fit12a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)


    scores <- bind_rows(
      bind_cols(f7a_score$mean_score, CRPS_mean = mean(crps7a)),
      bind_cols(f8a_score$mean_score, CRPS_mean = mean(crps8a)),
      bind_cols(f9a_score$mean_score, CRPS_mean = mean(crps9a)),
      bind_cols(f10a_score$mean_score, CRPS_mean = mean(crps10a)),
      bind_cols(f11a_score$mean_score, CRPS_mean = mean(crps11a)),
      bind_cols(f12a_score$mean_score, CRPS_mean = mean(crps12a))
    ) %>%
      bind_cols(data.frame(
        Model = mod_names_a
      ), .)


    sink("figures/model/cv/score_thin.txt", append = TRUE)
    print(trainset)
    print(Sys.time())
    print("grid resolution :")
    print(paste0(cv_thin_resol, "km"))
    sink()
    sink("figures/model/cv/score_thin.txt", append = TRUE)
    print(c(
      fit7a$bru_info$lhoods[[1]]$formula,
      fit8a$bru_info$lhoods[[1]]$formula,
      fit9a$bru_info$lhoods[[1]]$formula,
      fit10a$bru_info$lhoods[[1]]$formula,
      fit11a$bru_info$lhoods[[1]]$formula,
      fit12a$bru_info$lhoods[[1]]$formula
    ))
    sink()
    sink("figures/model/cv/score_thin.txt", append = TRUE)
    print(knitr::kable(scores))
    sink()
    sink("figures/model/cv/score_thin.txt", append = TRUE)
    print(knitr::kable(scores, "latex"))
    sink()

    if (exists("ls_a_score")) {
      rm(ls_a_score)
    }
    ls_a_score <- c(ls(pattern = "f.a_score"), ls(pattern = "f..a_score"))
    ls_a_crps <-  c(ls(pattern = "crps.a"), ls(pattern = "crps..a"))

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
      geometry = rep(cv_grid$geometry, times = length(mod_names_a))
    )

    df_ <- df %>%
      left_join(
        df %>%
          filter(Model == "fit7a") %>% # choose a ref baseline model
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
    saveRDS(df_, file = here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))
  }


  AE_range <- range(df$AE - df_$AE_ref)
  RMSE_range <- range(df$RMSE - df_$RMSE_ref)
  DS_range <- range(df$DS - df_$DS_ref)
  LS_range <- range(df$LS - df_$LS_ref)
  CRPS_range <- range(df$CRPS - df_$CRPS_ref)

  AE_diff_range <- c(-1, 1) * max(abs(AE_range))
  RMSE_diff_range <- c(-1, 1) * max(abs(RMSE_range))
  DS_diff_range <- c(-1, 1) * max(abs(DS_range))
  LS_diff_range <- c(-1, 1) * max(abs(LS_range))
  CRPS_diff_range <- c(-1, 1) * max(abs(CRPS_range))

  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    sfd <- scale_fill_distiller(
      type = "div",
      palette = "RdBu",
      limits = get(paste0(i, "_diff_range")),
      values = scales::rescale(abs(2 * sc - 1)^pwr * sign(sc - 0.5))
    )

    p1 <- ggplot() +
      gg(df_ %>% filter(Model == "fit7a"), lwd = 0, aes(fill = .data[[i]], col = .data[[i]])) +
      scale_fill_distiller(
        type = "seq",
        palette = "Reds",
        direction = 1
      ) +
      ggtitle(paste0(i, " scores")) +
      # labs(col = "none", fill = i)
      guides(col = "none", fill = guide_legend(i))


    p2 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit8a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit8a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit9a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit9a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit10a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit10a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit11a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit11a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p6 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit12a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit12a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, nrow = 2, guides = "collect")
    ggsave(paste0("figures/model/cv/", i, cv_thin_resol[1], "_diff_", trainset, ".png"), width = tw / 1.2, height = tw / 3)
  }

  ### ECDF --------------------------------------------------------------------

  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    ggplot() +
      stat_ecdf(data = df_ %>% filter(Model == "fit7a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit7a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit8a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit8a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit9a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit9a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit10a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit10a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit11a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit11a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit12a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit12a", alpha = .5)) +
      # scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      guides(alpha = "none") +
      xlab(i) +
      ylab(paste0(i, " ECDF"))
    ggsave(paste0("figures/model/cv/", i, "ecdf_", cv_thin_resol[1], "_", trainset, ".pdf"), width = tw / 2, height = tw / 5)
  }

  ## fitb --------------------------------------------------------------------

  if (file.exists(here("RDS", trainset, paste0("dfb_score_", nm_chess, ".RDS")))) {
    dfb_ <- readRDS(here("RDS", trainset, paste0("dfb_score_", nm_chess, ".RDS")))
  } else {
    f7b_score <- {
      score(fit7b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f8b_score <- {
      score(fit8b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f9b_score <- {
      score(fit9b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f10b_score <- {
      score(fit10b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f11b_score <- {
      score(fit11b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f12b_score <- {
      score(fit12b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    ls_b_score <- ls(pattern = "b_score")

    scores_b <- bind_rows(
      f7b_score$mean_score,
      f8b_score$mean_score,
      f9b_score$mean_score,
      f10b_score$mean_score,
      f11b_score$mean_score,
      f12b_score$mean_score
    ) %>%
      bind_cols(data.frame(
        Model = mod_names_b
      ), .)

    sink("figures/model/cv/score_thin_b.txt", append = TRUE)
    print(trainset)
    print(Sys.time())
    print("grid resolution :")
    print(paste0(cv_thin_resol, "km"))
    print(c(
      fit7b$bru_info$lhoods[[1]]$formula,
      fit8b$bru_info$lhoods[[1]]$formula,
      fit9b$bru_info$lhoods[[1]]$formula,
      fit10b$bru_info$lhoods[[1]]$formula,
      fit11b$bru_info$lhoods[[1]]$formula,
      fit12b$bru_info$lhoods[[1]]$formula
    ))
    print(knitr::kable(scores_b))
    print(knitr::kable(scores_b, "latex"))
    sink()


    ## ECDF --------------------------------------------------------------------


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
      geometry = rep(1:nrow(landslides_c_test), times = length(mod_names_b))
    )

    dfb_ <- dfb %>%
      left_join(
        dfb %>%
          filter(Model == "fit7b") %>% # choose a ref baseline model
          select(
            geometry,
            AE_ref = AE,
            RMSE_ref = RMSE,
            DS_ref = DS,
            LS_ref = LS
          ),
        by = c("geometry")
      )

    saveRDS(dfb_, file = here("RDS", trainset, paste0("dfb_score_", nm_chess, ".RDS")))
  }

  for (i in (c("AE", "RMSE", "DS", "LS"))) {
    ggplot() +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit7b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit7b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit8b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit8b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit9b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit9b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit10b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit10b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit11b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit11b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit12b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit12b", alpha = .5)) +
      guides(alpha = "none") +
      xlab(i) +
      ylab(paste0(i, " ECDF"))
    # ggsave(paste0("figures/model/cv/", i, "ecdf_b", cv_thin_resol[1], "_",  trainset, ".pdf"), width = tw/4, height = tw/5) # also an option
    ggsave(paste0("figures/model/cv/", i, "ecdf_b", cv_thin_resol[1], "_", trainset, ".pdf"), width = tw / 2, height = tw / 5)
  }

  plan(sequential)
}
