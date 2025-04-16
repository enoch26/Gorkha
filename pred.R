library(patchwork)
# TODO set seed for predict
# TODO how to deal with bru_fill_missing predict
## fit pred ---------------------------------------------------------------------

# > ls( pattern = "fit")
# [1] "fit1a" "fit1b" "fit2a" "fit2b"
mod_nm <- ls(pattern = "fit")
mod_names_a <- mod_nm[seq_along(mod_nm) %% 2 > 0]
mod_names_b <- mod_nm[seq_along(mod_nm) %% 2 == 0]


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
  if (FALSE) {
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


pred_nm <- ls(pattern = "fp")
pred_lst_a <- pred_nm[seq_along(pred_nm) %% 2 > 0]
pred_lst_b <- pred_nm[seq_along(pred_nm) %% 2 == 0]


# ggplot() + gg(fp5a$landuse["mean"], geom = "tile") +   scale_fill_viridis_c() +
#   geom_sf(data = landuse_8wp, fill = "red") +
#   geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red")
# ggsave(paste0("figures/model/check_landuse.pdf"), width = tw, height = tw/2)

# https://jayrobwilliams.com/posts/2021/05/geom-sf-facet
# TODO with future_lapply
if(FALSE){
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
            geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
            ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
        } else {
          p_lst_a[[j]] <- ggplot() + gg(get(pred_lst_a[[i]])[[j-1]]["mean"], geom = "tile") + 
            annotate(geom = 'label', 
                     label = paste0("Intercept = ", get(pred_lst_a[[i]])[[j]]["mean"]),
                     x = 400, y = 3150) +
            scale_fill_viridis_c(option = "D") +
            geom_sf(data = landslides_c, aes(col = "red", alpha = log(Area_m2)), size = 0.1) +
            geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
            ggtitle(paste0(names(get(pred_lst_a[[i]])[j-1])))
        }
      }
      wrap_plots(p_lst_a[-1], ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
        plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
      print(paste0(deparse(ff, width.cutoff = 150L)))
      ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_lds.png"), width = tw, height = 1.25 * tw)
    }
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
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      } else {
        p_lst_a[[j]] <- ggplot() + gg(get(pred_lst_a[[i]])[[j-1]]["mean"], geom = "tile") + 
          annotate(geom = 'label', 
                   label = paste0("Intercept = ", get(pred_lst_a[[i]])[[j]]["mean"]),
                   x = 400, y = 3150) +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j-1])))
      }
    }
    wrap_plots(p_lst_a[-1], ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, ".pdf"), width = tw, height = 1.25 * tw)
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, ".png"), width = tw, height = 1.25 * tw)
  }
}

if (to_plot) {
  for (i in 1:length(pred_lst_a)) {
    p_lst_a <- list()
    for (j in 1:length(get(pred_lst_a[i]))) {
      ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
      if (names(get(pred_lst_a[[i]])[j]) != "Intercept") {
        p_lst_a[[j]] <- ggplot() +
          gg(get(pred_lst_a[[i]])[[j]], aes(fill = logit_inv(sd/mean)), geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      } else {
        p_lst_a[[j]] <- ggplot() + gg(get(pred_lst_a[[i]])[[j-1]],
                                      aes(fill = logit_inv(sd/mean)), geom = "tile") + 
          annotate(geom = 'label',
                   label = paste0("Intercept CV = ", logit_inv(get(pred_lst_a[[i]])[[j]]["sd"]/get(pred_lst_a[[i]])[[j]]["mean"])),
                   x = 400, y = 3150) +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j-1])))
      }
    }
    wrap_plots(p_lst_a[-1], ncol = 2) + 
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_cv.pdf"), width = tw, height = 1.25 * tw)
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_cv.png"), width = tw, height = 1.25 * tw)
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
#         geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
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
if(FALSE){
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
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
      }
      wrap_plots(p_lst_alambda, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
        plot_annotation((paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3]))))
      print(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[c(2, 1, 3)])))
      ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_lambda.pdf"), width = tw, height = tw)
    }
  }
  
}



if (to_plot) {
  for (i in 1:length(pred_lst_b)) {
    p_lst_b <- list()
    for (j in 1:length(get(pred_lst_b[i]))) {
      ff <- get(mod_names_b[i])$bru_info$lhoods[[1]]$formula
      if (names(get(pred_lst_b[[i]])[j]) != "Intercept") {
        p_lst_b[[j]] <- ggplot() +
          gg(get(pred_lst_b[[i]])[[j]]["mean"], geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_b[[i]])[j])))
      } else {
        p_lst_b[[j]] <- ggplot() + gg(get(pred_lst_b[[i]])[[j-1]]["mean"], geom = "tile") + 
          annotate(geom = 'label', 
                   label = paste0("Intercept = ", get(pred_lst_b[[i]])[[j]]["mean"]),
                   x = 400, y = 3150) +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_b[[i]])[j-1])))
      }
    }
    wrap_plots(p_lst_b[-1], ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, ".pdf"), width = tw, height = 1.25 * tw)
    ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, ".png"), width = tw, height = 1.25 * tw)
  }
}

if (to_plot) {
  for (i in 1:length(pred_lst_b)) {
    p_lst_b <- list()
    for (j in 1:length(get(pred_lst_b[i]))) {
      ff <- get(mod_names_b[i])$bru_info$lhoods[[1]]$formula
      if (names(get(pred_lst_b[[i]])[j]) != "Intercept") {
        p_lst_b[[j]] <- ggplot() +
          gg(get(pred_lst_b[[i]])[[j]], aes(fill = logit_inv(sd/mean)), geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          # geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3) +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_b[[i]])[j])))
      } else {
        p_lst_b[[j]] <- ggplot() + gg(get(pred_lst_b[[i]])[[j-1]],
                                      aes(fill = logit_inv(sd/mean)), geom = "tile") + 
          annotate(geom = 'label',
                   label = paste0("Intercept CV = ", logit_inv(get(pred_lst_b[[i]])[[j]]["sd"]/get(pred_lst_b[[i]])[[j]]["mean"])),
                   x = 400, y = 3150) +
          scale_fill_viridis_c(option = "D") +
          # geom_sf(data = landslides_c, aes(col = "red", alpha = log(Area_m2)), size = 0.1) +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          ggtitle(paste0(names(get(pred_lst_b[[i]])[j-1])))
      }
    }
    wrap_plots(p_lst_b[-1], ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
      plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
    print(paste0(deparse(ff, width.cutoff = 150L)))
    ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, "_cv.pdf"), width = tw, height = 1.25 * tw)
    ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, "_cv.png"), width = tw, height = 1.25 * tw)
  }
}



# RW2 ---------------------------------------------------------------------

for (i in c(mod_names_a, mod_names_b)) {
  rw2_nm <- names(get(i)[["summary.random"]])[get(i)[["model.random"]] == "RW2 model"]
  for(j in rw2_nm){
    pred <- predict(
      get(i),
      data.frame(cov_range = seq(get(paste0("min_", j)), get(paste0("max_", j)), length.out = 1000)),
      formula = as.formula(paste0("~",  j, "_eval(cov_range)"))
    )
  
     ggplot(pred) +
      geom_line(aes(cov_range, mean)) +
      geom_ribbon(
        aes(cov_range,
          ymin = q0.025,
          ymax = q0.975
        ),
        alpha = 0.2
      ) +
      geom_ribbon(
        aes(cov_range,
          ymin = mean - 1 * sd,
          ymax = mean + 1 * sd
        ),
        alpha = 0.2
      ) + 
       ggtitle(j)
  
    ggsave(here("figures", "model", "rw2", paste0(i, "_", j, "_", nm_chess, ".pdf")), width = tw, height = tw / 2)
    
  }
  

  
}

# txt summary output ------------------------------------------------------


sink(paste0("figures/model/mod_lst_a", nm_chess," .txt"), append = TRUE)
for (i in 1:length(mod_names_a)) {
  print(mod_names_a[i])
  print(summary(get(mod_names_a[i])))
  print("landuse")
  print(get(mod_names_a[i])$summary.random$landuse[1:6])
  print("geology")
  print(get(mod_names_a[i])$summary.random$nepal_geo[1:6])
}
sink()

sink(paste0("figures/model/mod_lst_b", nm_chess,".txt"), append = TRUE)
for (i in 1:length(mod_names_b)) {
  print(mod_names_b[i])
  print(summary(get(mod_names_b[i])))
  print("landuse")
  print(get(mod_names_b[i])$summary.random$landuse_[1:6])
  print("geology")
  print(get(mod_names_b[i])$summary.random$nepal_geo_[1:6])
}
sink()

plan(sequential)

# future.apply ------------------------------------------------------------


## contour -----------------------------------------------------------------

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
#         geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
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
#         geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
#         ggtitle(paste0(names(get(pred_lst_a[[i]])[j])))
#     }
#     wrap_plots(p_lst_a, ncol = 2) + # TODO fail to share legend scale, guides = "collect") + needa align scale
#       plot_annotation((paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3]))))
#     print(paste0(paste0(get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[c(2,1,3)])))
#     ggsave(paste0("figures/", pred_lst_a[i], ".pdf"), width = tw, height = 1.25*tw)
#   }
# }
# )
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
