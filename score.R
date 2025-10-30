# for CV ------------------------------------------------------------------

pwr <- 8
sc <- seq(0, 1, length.out = 20)

mod_names_a <- ls(pattern = "fit.a")
mod_names_b <- ls(pattern = "fit.b")

path <- here("figures", "model", "cv", "dummy")
if (!dir.exists(dirname(path))) {
      dir.create(dirname(path), recursive = TRUE)
}

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
    f1a_score <- {
      score(fit1a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f2a_score <- {
      score(fit2a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f3a_score <- {
      score(fit3a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f4a_score <- {
      score(fit4a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f5a_score <- {
      score(fit5a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f6a_score <- {
      score(fit6a, newdata = cv_newdata, cv_grid = cv_grid_test, obs = "count_test", n.samples = 1000, seed = seed)
    }

    plan(sequential)
    plan(multicore, workers = 6)

    crps1a <- crps_(fit1a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps2a <- crps_(fit2a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps3a <- crps_(fit3a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps4a <- crps_(fit4a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps5a <- crps_(fit5a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)
    crps6a <- crps_(fit6a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid_test, n.samples = 1000, seed = seed)

    scores_a <- bind_rows(
      bind_cols(f1a_score$mean_score, CRPS_mean = mean(crps1a)),
      bind_cols(f2a_score$mean_score, CRPS_mean = mean(crps2a)),
      bind_cols(f3a_score$mean_score, CRPS_mean = mean(crps3a)),
      bind_cols(f4a_score$mean_score, CRPS_mean = mean(crps4a)),
      bind_cols(f5a_score$mean_score, CRPS_mean = mean(crps5a)),
      bind_cols(f6a_score$mean_score, CRPS_mean = mean(crps6a))
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
      fit1a$bru_info$lhoods[[1]]$formula,
      fit2a$bru_info$lhoods[[1]]$formula,
      fit3a$bru_info$lhoods[[1]]$formula,
      fit4a$bru_info$lhoods[[1]]$formula,
      fit5a$bru_info$lhoods[[1]]$formula,
      fit6a$bru_info$lhoods[[1]]$formula
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
    ls_a_score <- ls(pattern = "a_score")

    df <- tibble::tibble(
      Model = rep(mod_names_a, each = nrow(cv_grid_test)),
      # expect = as.vector(sapply(ls_a_score, function(x) {
      #   get(x)$pred$expect$mean
      # })),
      # count_test = rep(cv_grid_test$count_test, times = length(mod_names_a)),
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
      CRPS = c(crps1a, crps2a, crps3a, crps4a, crps5a, crps6a),
      geometry = rep(cv_grid_test$geometry, times = length(mod_names_a))
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
      gg(df_ %>% filter(Model == "fit1a"), lwd = 0, aes(fill = .data[[i]])) +
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
        df_ %>% filter(Model == "fit2a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit2a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit3a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit3a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit4a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit4a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit5a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit5a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p6 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit6a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit6a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, nrow = 2, guides = "collect")

    ggsave(paste0("figures/model/cv/", i, "_", train, cv_chess_resol[1], "_diff_", trainset, ".png"), width = tw / 1.2, height = tw / 3)
  }


  ### ECDF --------------------------------------------------------------------


  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    ggplot() +
      stat_ecdf(data = df_ %>% filter(Model == "fit1a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit1a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit2a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit2a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit3a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit3a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit4a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit4a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit5a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit5a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit6a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit6a", alpha = .5)) +
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
    f1b_score <- {
      score(fit1b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f2b_score <- {
      score(fit2b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f3b_score <- {
      score(fit3b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f4b_score <- {
      score(fit4b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f5b_score <- {
      score(fit5b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f6b_score <- {
      score(fit6b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    ls_b_score <- ls(pattern = "b_score")
    # crps1b <-crps_(fit1b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps2b <-crps_(fit2b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps3b <-crps_(fit3b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps4b <-crps_(fit4b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps5b <-crps_(fit5b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    # crps6b <-crps_(fit6b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)

    scores_b <- bind_rows(
      f1b_score$mean_score,
      f2b_score$mean_score,
      f3b_score$mean_score,
      f4b_score$mean_score,
      f5b_score$mean_score,
      f6b_score$mean_score
      # bind_cols(f1b_score$mean_score, CRPS_mean = mean(crps1b)),
      # bind_cols(f2b_score$mean_score, CRPS_mean = mean(crps2b)),
      # bind_cols(f3b_score$mean_score, CRPS_mean = mean(crps3b)),
      # bind_cols(f4b_score$mean_score, CRPS_mean = mean(crps4b)),
      # bind_cols(f5b_score$mean_score, CRPS_mean = mean(crps5b)),
      # bind_cols(f6b_score$mean_score, CRPS_mean = mean(crps6b))
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
      fit1b$bru_info$lhoods[[1]]$formula,
      fit2b$bru_info$lhoods[[1]]$formula,
      fit3b$bru_info$lhoods[[1]]$formula,
      fit4b$bru_info$lhoods[[1]]$formula,
      fit5b$bru_info$lhoods[[1]]$formula,
      fit6b$bru_info$lhoods[[1]]$formula
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
          filter(Model == "fit1b") %>% # choose a ref baseline model
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
      stat_ecdf(data = dfb_ %>% filter(Model == "fit1b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit1b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit2b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit2b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit3b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit3b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit4b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit4b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit5b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit5b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit6b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit6b", alpha = .5)) +
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
  # }

  # plan(multicore, workers = 6)

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



  ## fita --------------------------------------------------------------------

  if (file.exists(here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))) {
    df_ <- readRDS(here("RDS", trainset, paste0("df_score_", nm_chess, ".RDS")))
  } else {
 
    # cv_newdata <- cv_newdata %>% left_join(data.frame(cv_grid %>% st_drop_geometry() %>% select(.block, count, count_test)),
    #   by = ".block")
    f1a_score <- {
      score(fit1a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 10, seed = seed)
    }
    f1a_score <- {
      score(fit1a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f2a_score <- {
      score(fit2a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f3a_score <- {
      score(fit3a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f4a_score <- {
      score(fit4a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f5a_score <- {
      score(fit5a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }
    f6a_score <- {
      score(fit6a, newdata = cv_newdata, cv_grid = cv_grid, obs = "count_test", n.samples = 1000, seed = seed)
    }

    plan(sequential)
    # plan(multicore, workers = 6)

    crps1a <- crps_(fit1a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps2a <- crps_(fit2a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps3a <- crps_(fit3a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps4a <- crps_(fit4a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps5a <- crps_(fit5a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)
    crps6a <- crps_(fit6a, newdata = cv_newdata, obs = "count_test", cv_grid = cv_grid, n.samples = 1000, seed = seed)


    scores <- bind_rows(
      bind_cols(f1a_score$mean_score, CRPS_mean = mean(crps1a)),
      bind_cols(f2a_score$mean_score, CRPS_mean = mean(crps2a)),
      bind_cols(f3a_score$mean_score, CRPS_mean = mean(crps3a)),
      bind_cols(f4a_score$mean_score, CRPS_mean = mean(crps4a)),
      bind_cols(f5a_score$mean_score, CRPS_mean = mean(crps5a)),
      bind_cols(f6a_score$mean_score, CRPS_mean = mean(crps6a))
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
      fit1a$bru_info$lhoods[[1]]$formula,
      fit2a$bru_info$lhoods[[1]]$formula,
      fit3a$bru_info$lhoods[[1]]$formula,
      fit4a$bru_info$lhoods[[1]]$formula,
      fit5a$bru_info$lhoods[[1]]$formula,
      fit6a$bru_info$lhoods[[1]]$formula
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
    ls_a_score <- ls(pattern = "a_score")
    ls_a_crps <- ls(pattern = "crps.a")

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
      gg(df_ %>% filter(Model == "fit1a"), lwd = 0, aes(fill = .data[[i]], col = .data[[i]])) +
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
        df_ %>% filter(Model == "fit2a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit2a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    p3 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit3a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit3a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p4 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit4a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit4a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    p5 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit5a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit5a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
    p6 <- ggplot() +
      gg(
        df_ %>% filter(Model == "fit6a"),
        lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
      ) +
      sfd +
      ggtitle(paste0("fit6a ", i, " score difference")) +
      guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

    patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, nrow = 2, guides = "collect")
    ggsave(paste0("figures/model/cv/", i, cv_thin_resol[1], "_diff_", trainset, ".png"), width = tw / 1.2, height = tw / 3)
  }

  ### ECDF --------------------------------------------------------------------

  for (i in (c("AE", "RMSE", "DS", "LS", "CRPS"))) {
    ggplot() +
      stat_ecdf(data = df_ %>% filter(Model == "fit1a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit1a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit2a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit2a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit3a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit3a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit4a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit4a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit5a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit5a", alpha = .5)) +
      stat_ecdf(data = df_ %>% filter(Model == "fit6a"), aes(y = after_stat(y), x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit6a", alpha = .5)) +
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
    f1b_score <- {
      score(fit1b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f2b_score <- {
      score(fit2b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f3b_score <- {
      score(fit3b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f4b_score <- {
      score(fit4b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f5b_score <- {
      score(fit5b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    f6b_score <- {
      score(fit6b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
    }

    ls_b_score <- ls(pattern = "b_score")

    scores_b <- bind_rows(
      f1b_score$mean_score,
      f2b_score$mean_score,
      f3b_score$mean_score,
      f4b_score$mean_score,
      f5b_score$mean_score,
      f6b_score$mean_score
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
      fit1b$bru_info$lhoods[[1]]$formula,
      fit2b$bru_info$lhoods[[1]]$formula,
      fit3b$bru_info$lhoods[[1]]$formula,
      fit4b$bru_info$lhoods[[1]]$formula,
      fit5b$bru_info$lhoods[[1]]$formula,
      fit6b$bru_info$lhoods[[1]]$formula
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
          filter(Model == "fit1b") %>% # choose a ref baseline model
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
      stat_ecdf(data = dfb_ %>% filter(Model == "fit1b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit1b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit2b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit2b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit3b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit3b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit4b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit4b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit5b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit5b", alpha = .5)) +
      stat_ecdf(data = dfb_ %>% filter(Model == "fit6b"), aes(x = .data[[i]] - .data[[paste0(i, "_ref")]], col = "fit6b", alpha = .5)) +
      guides(alpha = "none") +
      xlab(i) +
      ylab(paste0(i, " ECDF"))
    # ggsave(paste0("figures/model/cv/", i, "ecdf_b", cv_thin_resol[1], "_",  trainset, ".pdf"), width = tw/4, height = tw/5) # also an option
    ggsave(paste0("figures/model/cv/", i, "ecdf_b", cv_thin_resol[1], "_", trainset, ".pdf"), width = tw / 2, height = tw / 5)
  }

  plan(sequential)
}

# PIT ---------------------------------------------------------------------

if (FALSE) {
  # to  be removed ----------------------------------------------------------


  if (FALSE) {
    ls_b_score <- ls(pattern = "b_score")

    df <- tibble::tibble(
      Model = rep(mod_names_b, each = nrow(landslides_c_test)),
      RMSE = as.vector(sapply(ls_b_score, function(x) {
        sqrt(get(x)$pred$obs_prob$SE)
      })),
      DS = as.vector(sapply(ls_b_score, function(x) {
        get(x)$pred$obs_prob$DS
      })),
      # AE = as.vector(sapply(ls_b_score, function(x) {
      #   get(x)$pred$obs_prob$AE
      # })),
      LS = as.vector(sapply(ls_b_score, function(x) {
        get(x)$pred$obs_prob$LS
      })),
      geometry = rep(landslides_c_test$geometry, times = length(mod_names_b))
    )

    df_ <- df %>%
      left_join(
        df %>%
          filter(Model == "fit1a") %>% # choose a ref baseline model
          select(geometry,
            # AE_ref = AE,
            RMSE_ref = RMSE,
            DS_ref = DS,
            LS_ref = LS
          ),
        by = c("geometry")
      ) %>%
      sf::st_as_sf()


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

    for (i in c("AE", "RMSE", "DS", "LS", "CRPS")) {
      sfd <- scale_fill_distiller(
        type = "div",
        palette = "RdBu",
        limits = get(paste0(i, "_range"))
      ) +
        geom_sf(data = bnd, col = "red", fill = NA)


      # sfd <- scale_colour_gradient2(
      #   low = "grey",
      #   mid = "white",
      #   high = "brown",
      #   limits = get(paste0(i, "_range")))

      p1 <- ggplot() +
        # geom_sf(data = landcover %>% filter(CODE1 %in% c("8SN", "8ICE", "8SNs", "8ICEr")), fill = "red") +
        geom_sf(data = bnd, col = "red", fill = NA) +
        gg(df_ %>% filter(Model == "fit1b"), aes(
          col = .data[[i]], alpha = log(.data[[i]])
          # , size = log(.data[[i]])
        ), size = 0.01) +
        scale_fill_viridis_c() +
        # scale_fill_distiller(
        #   type = "seq",
        #   palette = "Reds",
        #   direction = 1
        # ) +
        ggtitle(paste0(i, " scores")) +
        guides(col = "none", fill = guide_legend(i))

      # p1

      # ggsave(paste0("figures/model/cv/", i, "_size",cv_thin_resol[1], ".pdf"), width = tw/1.2, height = tw / 3)

      p2 <- ggplot() +
        gg(
          df_ %>% filter(Model == "fit2b"),
          lwd = 0, aes(fill = .data[[i]])
        ) +
        sfd +
        ggtitle(paste0("fit2b ", i, " score difference")) +
        guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

      p3 <- ggplot() +
        gg(
          df_ %>% filter(Model == "fit3a"),
          lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
        ) +
        sfd +
        ggtitle(paste0("fit3a ", i, " score difference")) +
        guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))
      p4 <- ggplot() +
        gg(
          df_ %>% filter(Model == "fit4a"),
          lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
        ) +
        sfd +
        ggtitle(paste0("fit4a ", i, " score difference")) +
        guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

      p5 <- ggplot() +
        gg(
          df_ %>% filter(Model == "fit5a"),
          lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
        ) +
        sfd +
        ggtitle(paste0("fit5a ", i, " score difference")) +
        guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

      p6 <- ggplot() +
        gg(
          df_ %>% filter(Model == "fit6a"),
          lwd = 0, aes(fill = .data[[i]] - .data[[paste0(i, "_ref")]], col = .data[[i]] - .data[[paste0(i, "_ref")]])
        ) +
        sfd +
        ggtitle(paste0("fit6a ", i, " score difference")) +
        guides(col = "none", fill = guide_legend(paste0(i, "-", i, "_ref"))) # labs(col = "", fill = paste0(i, "-", i, "_ref"))

      patchwork::wrap_plots(p1, p2, p3, p4, p5, p6, nrow = 2, guides = "collect")
      ggsave(paste0("figures/model/cv/", i, cv_thin_resol[1], "_diff_", trainset, ".png"), width = tw / 1.2, height = tw / 3)
    }
  }

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
      # AE = abs(count - pred_median),
      SE = (count - pred_mean)^2,
      DS = (count - pred_mean)^2 / pred_var + log(pred_var),
      LG = log_score
    )
  # summary(df %>% filter(DS < quantile(DS,.995)))
  scores <- df %>%
    # filter(DS > quantile(DS,.995)) %>%
    group_by(Model) %>%
    summarise(
      # MAE = mean(AE),
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
          # AE_fit1a = AE,
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
    guides(col = "none", fill = guide_legend("DS"))
  p1
  ggsave("fit1a_ds.pdf")
}




# fm_int
# formula_ <- as.formula(paste0(" ~ sum(weight * exp(", ff, "))")) # the whole region

# pred %<-% {
#   predict(fit1a,  fm_int(mesh_fm, bnd_grid_sf),
#           formula = formula_, n.samples = 1)
# }

# > class(pred["mean"])
# [1] "bru_prediction" "data.frame"

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

if (FALSE) {
  # abs(2*sc-1)^1.5*sign(sc-0.5)

  # sfd <- scale_colour_gradient2(
  #   low = "grey",
  #   mid = "white",
  #   high = "brown",
  #   limits = get(paste0(i, "_range")))
  # sfd <- scale_fill_distiller(
  #   type = "div",
  #   palette = "RdBu",
  #   limits = range(d_grid$z),
  #   values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5))
  # )
  #
  # sfgn <- scale_fill_gradientn(
  #   colours = colorspace::diverge_hcl(7),
  #   limits = LS_diff_range,
  #   values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5))
  # )
  #
  # ggplot() +
  #   gg(data.frame(df_ %>% filter(Model == "fit1a")) %>% st_as_sf(), aes(fill = .data[[i]], col = .data[[i]])))) +
  #   sfd +
  #   ggtitle(paste0(i, " scores")) +
  #   guides(col = "none", fill = guide_legend(i))
  # ggsave("figures/model/test_sfd.png")
  #
  # ggplot() +
  #   gg(data.frame(df_ %>% filter(Model == "fit1a")) %>% st_as_sf(), aes(fill = .data[[i]], col = .data[[i]])))) +
  #   sfgn +
  #   ggtitle(paste0(i, " scores")) +
  #   guides(col = "none", fill = guide_legend(i))
  # ggsave("figures/model/test_sfgn.png")
  # sfd <- scale_colour_gradient2(
  #   low = "grey",
  #   mid = "white",
  #   high = "brown",
  #   limits = get(paste0(i, "_range")))
}
