library(patchwork)
# TODO set seed for predict
# TODO how to deal with bru_fill_missing predict
## fit pred ---------------------------------------------------------------------

### sf pts ------------------------------------------------------------------

# https://inlabru-org.github.io/inlabru/articles/2d_lgcp_covars.html
# predict_plot(fit2, pts,)

# pred <- predict(fit,
#   newdata = pts,
#   formula = ~ list(
#     lambda = Intercept + pgv + landuse,
#     pgv = pgv,
#     landuse = landuse
#   ),
#   n.samples = 100, seed = seed[1]
# )
#
# pts$lambda_post <- pred$lambda$mean
#
# p_pred <- ggplot() +
#   gg(data = pts, aes(fill = lambda_post), geom = "tile") +
#   geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.5) +
#   # geom_sf(data = landslides, aes(col = Area_m2)) +
#   coord_sf() +
#   scale_fill_viridis_c()
# p_pred
# ggsave(here("figures", "p_pred.pdf"), width = tw, height = tw/2)


plan(multicore, workers = 2)

# helper function to make formula for prediction 
fml_lambda <- function(fml){
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  tt <- labels(terms(fml))
  if (any(stringr::str_detect(tt, "exp"))) {
    tt1 <- tt
    tt1[stringr::str_detect(tt1, "exp")] <- "exp_diff"
  } else {
    tt1 <- tt
  }
  # if (any(stringr::str_detect(tt, "beta"))) {
  #   tt1 <- tt
  #   tt1[stringr::str_detect(tt1, "beta")] <- ""
  # } else {
  #   tt1 <- tt
  # }
  as.formula(paste0(" ~ list(", "lambda = exp(", ff, "),", 
                    "log_lambda = ", ff, 
                    paste(",", tt1, "=", tt, collapse = ""), ")"))
}

fml_mu <- function(fml){
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  tt <- labels(terms(fml))
  if (any(stringr::str_detect(tt, "exp"))) {
    tt1 <- tt
    tt1[stringr::str_detect(tt1, "exp")] <- "exp_diff"
  } else {
    tt1 <- tt
  }
  as.formula(paste0(" ~ list(", "mu = ", ff, paste(",", tt1, "=", tt, collapse = ""), ")"))
}

# textbox
p_textbox <- function(text){
  df <- data.frame(
    x = 0.1,
    y = 0.8,
    label = paste0("Intercept = ", as.character(text))
  )
  
  p <- ggplot() +
    ggtext::geom_textbox(
      data = df,
      aes(x, y, label = label),
      # width = grid::unit(0.73, "npc"), # 73% of plot panel width
      hjust = 0, vjust = 1
    ) +
    xlim(0, 1) + ylim(0, 1)
  p
}

### Predict ----------------------------------------------------------------
fp1a %<-% {
  predict(fit1a,
    newdata = pxl,
    formula = fml_lambda(fml1a),
    n.samples = 100, seed = seed[1]
  )
}

fp1b %<-% {
  predict(fit1b,
    newdata = pxl,
    formula = fml_mu(fml1b),
    n.samples = 100, seed = seed[1]
  )
}

fp2a %<-% {
  predict(fit2a,
          newdata = pxl,
          formula = fml_lambda(fml2a),
          n.samples = 100, seed = seed[1]
  )
}

fp2b %<-% {
  predict(fit2b,
          newdata = pxl,
          formula = fml_mu(fml2b),
          n.samples = 100, seed = seed[1]
  )
}

fp3a %<-% {
  predict(fit3a,
          newdata = pxl,
          formula = fml_lambda(fml3a),
          n.samples = 100, seed = seed[1]
  )
}

fp3b %<-% {
  predict(fit3b,
          newdata = pxl,
          formula = fml_mu(fml3b),
          n.samples = 100, seed = seed[1]
  )
}

fp4a %<-% {
  predict(fit4a,
          newdata = pxl,
          formula = fml_lambda(fml4a),
          n.samples = 100, seed = seed[1]
  )
}

fp4b %<-% {
  predict(fit4b,
          newdata = pxl,
          formula = fml_mu(fml4b),
          n.samples = 100, seed = seed[1]
  )
}

fp5a %<-% {
  predict(fit5a,
          newdata = pxl,
          formula = fml_lambda(fml5a),
          n.samples = 100, seed = seed[1]
  )
}

fp5b %<-% {
  predict(fit5b,
          newdata = pxl,
          formula = fml_mu(fml5b),
          n.samples = 100, seed = seed[1]
  )
}

if(FALSE){
fp6a %<-% {
  predict(fit6a,
          newdata = pxl,
          formula = fml_lambda(fml6a),
          n.samples = 100, seed = seed[1]
  )
}

fp6b %<-% {
  predict(fit6b,
          newdata = pxl,
          formula = fml_mu(fml6b),
          n.samples = 100, seed = seed[1]
  )
}

fp7a %<-% {
  predict(fit7a,
          newdata = pxl,
          formula = fml_lambda(fml7a),
          n.samples = 100, seed = seed[1]
  )
}

fp7b %<-% {
  predict(fit7b,
          newdata = pxl,
          formula = fml_mu(fml7b),
          n.samples = 100, seed = seed[1]
  )
}

fp8a %<-% {
  predict(fit8a,
          newdata = pxl,
          formula = fml_lambda(fml8a),
          n.samples = 100, seed = seed[1]
  )
}

fp8b %<-% {
  predict(fit8b,
          newdata = pxl,
          formula = fml_mu(fml8b),
          n.samples = 100, seed = seed[1]
  )
}
}
plan(sequential)

# > ls( pattern = "fit.ca")
# [1] "fit1a" "fit1b" "fit2a" "fit2b"

mod_names_a <- c(
  "fit1a", "fit2a",
  "fit3a",
  "fit4a",
  "fit5a",
  # "fit6a",
  # "fit7a",
  # "fit8a"
  # "fit9a"
  # "fit10a", "fit11a"
)
mod_names_b <- c(
  "fit1b", "fit2b",
  "fit3b",
  "fit4b",
  "fit5b",
  # "fit6b",
  # "fit7b",
  # "fit8b"
  # "fit9b"
  # "fit10b", "fit11b"
)
pred_lst_a <- c(
  "fp1a", "fp2a",
  "fp3a",
  "fp4a",
  "fp5a",
  # "fp6a",
  # "fp7a",
  # "fp8a"
  # "fp9a"
  # "fp10a", "fp11a"
)
pred_lst_b <- c(
  "fp1b", "fp2b",
  "fp3b",
  "fp4b",
  "fp5b",
  # "fp6b",
  # "fp7b",
  # "fp8b"
  # "fp9b"
  # "fp10b", "fp11b"
)

# ggplot() + gg(fp5a$landuse["mean"], geom = "tile") +   scale_fill_viridis_c() +
#   geom_sf(data = landuse_8wp, fill = "red") + 
#   geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") 
# ggsave(paste0("figures/model/check_landuse.pdf"), width = tw, height = tw/2)

# https://jayrobwilliams.com/posts/2021/05/geom-sf-facet
# TODO with future_lapply

if (to_plot) {
  for (i in 1:length(pred_lst_a)) {
    p_lst_a <- list()
    ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
    for (j in 1:length(get(pred_lst_a[i]))) {
      if (names(get(pred_lst_a[[i]])[j]) != "Intercept") {
        p_lst_a[[j]] <- ggplot() +
          gg(get(pred_lst_a[[i]])[[j]]["mean"], geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = landslides_c, aes(col = "red", alpha = log(Area_m2)), size = 0.1) +
          geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      } else {
        
        p_lst_a[[j]] <- p_textbox(get(pred_lst_a[[i]])[[j]]["mean"])
        # p_lst_a[[j]] <- plot_spacer() + ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      }
    }
    wrap_plots(p_lst_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_lds.pdf"), width = tw, height = 1.25 * tw)
  }
}

if (to_plot) {
  for (i in 1:length(pred_lst_a)) {
    p_lst_a <- list()
    ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
    for (j in 1:length(get(pred_lst_a[i]))) {
      if (names(get(pred_lst_a[[i]])[j]) != "Intercept") {
        p_lst_a[[j]] <- ggplot() +
          gg(get(pred_lst_a[[i]])[[j]]["mean"], geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      } else {
        p_lst_a[[j]] <- p_textbox(get(pred_lst_a[[i]])[[j]]["mean"])
        # p_lst_a[[j]] <- plot_spacer() + ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      }
    }
    wrap_plots(p_lst_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, ".pdf"), width = tw, height = 1.25 * tw)
  }
}

# if (to_plot) {
#   for (i in 1:length(pred_lst_a)) {
#     p_lst_a <- list()
#       ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
#     for (j in 1:length(get(pred_lst_a[i]))) {
#       p_lst_a[[j]] <- ggplot() +
#         gg(get(pred_lst_a[[i]])[[j]]["mean"], geom = "tile") +
#         scale_fill_viridis_c(option = "C") +
#         # geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
#         # geom_sf(data = bnd, col = "red", fill = NA) +
#         geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
#         ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
#     }
#     wrap_plots(p_lst_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
#       plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
#     print(paste0(deparse(ff, width.cutoff = 150L)))
#     ggsave(paste0("figures/", pred_lst_a[i], "_contour.pdf"), width = tw, height = 1.25*tw)
#   }
# }

# lambda_sc <- scale_fill_viridis_c(limits = c(-1, 15), name = "lambda")
# intensity in exponential scale
if (to_plot) {
  for (i in 1:length(pred_lst_a)) {
    p_lst_alambda <- list()
    for (j in 1:1) {
      p_lst_alambda[[j]] <- ggplot() +
        gg(get(pred_lst_a[[i]])[[j]]["mean"], geom = "tile") +
        scale_fill_viridis_c(
          option = "D",
          limits = c(
            median(get(pred_lst_a[[i]])[[j]]$mean),
            max(get(pred_lst_a[[i]])[[j]]$mean)
          ),
          name = "lambda"
        ) +
        # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
        geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
        ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
    }
    wrap_plots(p_lst_alambda, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation((paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3]))))
    print(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[c(2, 1, 3)])))
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_lambda.pdf"), width = tw, height = tw)
  }
}

# contour
# if (to_plot) {
#   for (i in 1:length(pred_lst_a)) {
#     p_lst_alambda <- list()
#     for (j in 1:1) {
#       p_lst_alambda[[j]] <- ggplot() +
#         gg(get(pred_lst_a[[i]])[[j]]["mean"], geom = "tile") +
#         scale_fill_viridis_c(option = "A",
#                              limits =  c(median(get(pred_lst_a[[i]])[[j]]$mean),
#                                          max(get(pred_lst_a[[i]])[[j]]$mean)),
#                              name = "lambda") +
#         # geom_sf(data=eks::st_get_contour(Sta_den, cont=c(20,40,60,80)), fill=NA) +
#         geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
#         ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
#     }
#     wrap_plots(p_lst_alambda, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
#       plot_annotation((paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3]))))
#     print(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[c(2,1,3)])))
#     ggsave(paste0("figures/", pred_lst_a[i], "_contour_lambda.pdf"), width = tw, height = tw)
#   }
# }

#
# p_lst_a <- list()
# library(future.apply)
# p_lst_a <- future_lapply(1:length(mod_names_a), function(i) {
#   for (i in 1:length(pred_lst_a)) {
#     p_lst_a <- list()
#     for (j in 1:length(get(pred_lst_a[i]))) {
#       p_lst_a[[j]] <- ggplot() +
#         gg(get(pred_lst_a[[i]])[[j]]["mean"], geom = "tile") +
#         scale_fill_viridis_c(option = "C") +
#         geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
#         geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
#         ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
#     }
#     wrap_plots(p_lst_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
#       plot_annotation((paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3]))))
#     print(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[c(2,1,3)])))
#     ggsave(paste0("figures/", pred_lst_a[i], ".pdf"), width = tw, height = 1.25*tw)
#   }
# }
# )

if (to_plot) {
  for (i in 1:length(pred_lst_b)) {
    p_lst_b <- list()
    for (j in 1:length(get(pred_lst_b[i]))) {
      ff <- get(mod_names_b[i])$bru_info$lhoods[[1]]$formula
      if (names(get(pred_lst_b[[i]])[j]) != "Intercept") {
        p_lst_b[[j]] <- ggplot() +
          gg(get(pred_lst_b[[i]])[[j]]["mean"], geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
          geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_b[[i]])[j])))
      } else {
        p_lst_b[[j]] <- p_textbox(get(pred_lst_b[[i]])[[j]]["mean"])
        # p_lst_b[[j]] <- plot_spacer() +
        #   ggtitle(paste0(names(get(pred_lst_b[[i]])[j])))
      }
    }
    wrap_plots(p_lst_b, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, "_nolds.pdf"), width = tw, height = 1.25 * tw)
  }
}


# if (to_plot) {
#   for (i in 1:length(pred_lst_a)) {
#     for (j in 1:length(get(pred_lst_a[i]))) {
#       ggplot() +
#         gg(get(pred_lst_a[i])["mean"], geom = "tile") +
#         scale_fill_viridis_c(option = "C") +
#         geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
#         ggtitle(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3])))
#       print(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[c(2,1,3)])))
#       ggsave(paste0("figures/", pred_lst_a[i], ".pdf"), width = tw, height = tw / 2)
#     }
#   }
# }
# paste0(paste0(get(mod_names_a[1])$bru_info$lhoods[[1]]$formula[c(2,1,3)]))

# if (to_plot) {
#   for (i in 1:length(pred_lst_b)) {
#     ggplot() +
#       gg(get(pred_lst_b[i])["mean"], geom = "tile") +
#       scale_fill_viridis_c(option = "C") +
#       geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
#       ggtitle(paste0(paste0(get(mod_names_b[i])$bru_info$lhoods[[1]]$formula[3])))
#     print(paste0(paste0(get(mod_names_b[i])$bru_info$lhoods[[1]]$formula[c(2,1,3)])))
#     ggsave(paste0("figures/", pred_lst_b[i], ".pdf"), width = tw, height = tw / 2)
#   }
# }


# RW2 ---------------------------------------------------------------------

for(i in c(mod_names_a, mod_names_b)){
  ksn_tag_pred <- predict(
    get(i),
    data.frame(log_ksn_tag = seq(0, max_ksn_tag, length.out = 1000)),
    formula = ~ ksn_tag_eval(log_ksn_tag)
  )
  
  # debug: mapper_val[[part]] <- input_eval(component[[part]]$input, data, 
  #                                         env = component$env, label = part, ...)
  # Browse[1]> str(component[[part]]$input)                                                                                                                        
  # List of 4
  # $ input   : language ksn_tag$log_cop30dem_channel_tagged_pixels
  # $ label   : chr "ksn_tag"
  # $ layer   : NULL
  # $ selector: NULL
  # - attr(*, "class")= chr "bru_input"
  # 
  # 
  # Warning in handle_problems(e_input) :
  #   The input evaluation 'ksn_tag$log_cop30dem_channel_tagged_pixels' for 'ksn_tag' failed. Perhaps the data object doesn't contain the needed variables? Falling back to '1'.
  
  
  ggplot(ksn_tag_pred) +
    geom_line(aes(log_ksn_tag, mean)) +
    geom_ribbon(
      aes(log_ksn_tag,
          ymin = q0.025,
          ymax = q0.975
      ),
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(log_ksn_tag,
          ymin = mean - 1 * sd,
          ymax = mean + 1 * sd
      ),
      alpha = 0.2
    )
  
  ggsave(here("figures", "model", paste0(i, "ksn_tag_rw2", nm_chess, ".pdf")), width = tw, height = tw / 2)
  
  pga_mean_pred <- predict(
    get(i),
    data.frame(pga_mean_ = seq(min_pga_mean, max_pga_mean, length.out = 1000)),
    formula = ~ pga_mean_raster_eval(pga_mean_)
  )
  
  # debug: mapper_val[[part]] <- input_eval(component[[part]]$input, data, 
  #                                         env = component$env, label = part, ...)
  # Browse[1]> str(component[[part]]$input)                                                                                                                        
  # List of 4
  # $ input   : language pga_mean$log_cop30dem_channel_tagged_pixels
  # $ label   : chr "pga_mean"
  # $ layer   : NULL
  # $ selector: NULL
  # - attr(*, "class")= chr "bru_input"
  # 
  # 
  # Warning in handle_problems(e_input) :
  #   The input evaluation 'pga_mean$log_cop30dem_channel_tagged_pixels' for 'pga_mean' failed. Perhaps the data object doesn't contain the needed variables? Falling back to '1'.
  
  
  ggplot(pga_mean_pred) +
    geom_line(aes(pga_mean_, mean)) +
    geom_ribbon(
      aes(pga_mean_,
          ymin = q0.025,
          ymax = q0.975
      ),
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(pga_mean_,
          ymin = mean - 1 * sd,
          ymax = mean + 1 * sd
      ),
      alpha = 0.2
    )
  
  ggsave(here("figures", "model", paste0(i, "pga_mean_rw2", nm_chess, ".pdf")), width = tw, height = tw / 2)

}

  # rf2ch
  
  mod_names_a <- "fit4a"
  mod_names_b <- "fit4b"
  for(i in c(mod_names_a, mod_names_b)){
    rf2ch_pred <- predict(
      get(i),
      data.frame(rf2ch_ = seq(0, 4.72, length.out = 1000)),
      formula = ~ rf2ch_rw2_eval(rf2ch_)
    )
    
    ggplot(rf2ch_pred) +
      geom_line(aes(rf2ch_, mean)) +
      geom_ribbon(
        aes(rf2ch_,
            ymin = q0.025,
            ymax = q0.975
        ),
        alpha = 0.2
      ) +
      geom_ribbon(
        aes(rf2ch_,
            ymin = mean - 1 * sd,
            ymax = mean + 1 * sd
        ),
        alpha = 0.2
      )
    
    ggsave(here("figures", "model", paste0(i, "rf2ch_rw2", nm_chess, ".pdf")), width = tw, height = tw / 2)
    
  }
  if(FALSE){
  mod_names_a <- "fit3a"
  mod_names_b <- "fit3b"
  for(i in c(mod_names_a, mod_names_b)){
    rf2ch_inv_pred <- predict(
      get(i),
      data.frame(rf2ch_inv_ = seq(0, 4.72, length.out = 1000)),
      formula = ~ rf2ch_inv_rw2_eval(rf2ch_inv_)
    )
    
    ggplot(rf2ch_inv_pred) +
      geom_line(aes(rf2ch_inv_, mean)) +
      geom_ribbon(
        aes(rf2ch_inv_,
            ymin = q0.025,
            ymax = q0.975
        ),
        alpha = 0.2
      ) +
      geom_ribbon(
        aes(rf2ch_inv_,
            ymin = mean - 1 * sd,
            ymax = mean + 1 * sd
        ),
        alpha = 0.2
      )
    
    ggsave(here("figures", "model", paste0(i, "rf2ch_inv_rw2", nm_chess, ".pdf")), width = tw, height = tw / 2)
    
  }

}

# txt summary output ------------------------------------------------------


sink("figures/model/mod_lst_a.txt")
for (i in 1:length(mod_names_a)) {
  print(mod_names_a[i])
  print(summary(get(mod_names_a[i])))
  print("landuse")
  print(get(mod_names_a[i])$summary.random$landuse[1:6])
  print("geology")
  print(get(mod_names_a[i])$summary.random$nepal_geo[1:6])
  # print("aspect")
  # print(get(mod_names_a[i])$summary.random$asp[1:6])
}
sink()

sink("figures/model/mod_lst_b.txt")
for (i in 1:length(mod_names_b)) {
  print(mod_names_b[i])
  print(summary(get(mod_names_b[i])))
  print("landuse")
  print(get(mod_names_b[i])$summary.random$landuse_[1:6])
  print("geology")
  print(get(mod_names_b[i])$summary.random$nepal_geo_[1:6])
  # print("aspect")
  # print(get(mod_names_b[i])$summary.random$asp[1:6])
}
sink()

# if(to_plot){
#   ggplot() +
#     gg(pred_pxl["mean"], geom = "tile") + scale_fill_viridis_c() +
#     geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3)
#   ggsave(here("figures", "pred_pxl.pdf"), width = tw, height = tw/2)
# }
#
# pred_pxl2 <- predict(fit2,
#                      newdata = pxl,
#                      formula = ~ Intercept + pga_mean_raster + nepal_geo,
#                      n.samples = 100, seed = seed[1]
# )
#
# if(to_plot){
#   ggplot() +
#     gg(pred_pxl2["mean"], geom = "tile") + scale_fill_viridis_c() +
#     geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3)
#   ggsave(here("figures", "pred_pxl2.pdf"), width = tw, height = tw/2)
# }
#

plan(sequential)



# train and test ----------------------------------------------------------

if(FALSE){
  newdata 
  fp1a_terra <- rasterize(vect(fp1a$lambda["mean"]), field="mean", pxl_terra, fun = "mean")
  
  
  pxl_terra$fp1a_se <- (pxl_terra$count - fp1a_terra$mean)^2
  
  post_E <- get("mean", pred)
  post_Var <- post_E + (get("sd", pred))^2
  pred$SE_score <- SE_score <- (pxl_terra$count - post_E)^2
  pred$DS_score <- DS_score <- SE_score / post_Var + log(post_Var)
  
  post_Var <- pred$mean + pred$sd^2
  SE_score <- (bnd_mesh$count - post_E)^2
  DS_score <- (bnd_mesh$count - post_E)^2 / post_Var + log(post_Var)
  
}

# future.apply ------------------------------------------------------------
# TODO automated prediction but too large memory as a list
# predict_lst_a <- future.apply::future_lapply(1:length(mod_names_a), function(i) {
#   ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
#   tt <- labels(terms(ff))
#   predict(get(mod_names_a[i]),
#     newdata = pxl,
#     formula = as.formula(paste0(" ~ list(", "lambda_ = exp(", deparse(ff[[3]], width.cutoff = 150L), ")", paste(",", tt, "=", tt, collapse = ""), ")")),
#     n.samples = 100, seed = seed[1]
#   )
# })

# fml <- as.formula(paste0("~ list(lambda = exp(",
#                          deparse(fml2a[[3]]),
#                          "), loglambda = ",
#                          deparse(fml2a[[3]]), ",",
#                          gsub(",$", "",
#                               paste0(attr(terms(fml2a), which = "term.labels"),
#                                      "=", attr(terms(fml2a), which = "term.labels") , collapse = ", ")),
#                          ")"))
