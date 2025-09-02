# TODO CV for exp(z(s) \beta), tailor made for the model, dun do a auto one 

library(patchwork)
# https://stackoverflow.com/questions/48424682/how-do-i-limit-the-range-of-the-viridis-colour-scale


  fp_a %<-% {
    predict(fit6a,
            newdata = pxl,
            formula = fml_lambda_(fml6a), # say fit6a
            n.samples = 100, seed = seed[1]
    )
  }
  fp_lst_a <- list()
  pwr <- 3
  sc <- scales::rescale(seq(0,1,length.out = 30)^pwr)
  
  for (j in 1:length(fp_a)) {
    ff <- fit6a$bru_info$lhoods[[1]]$formula # say fit6a
    if (names(fp_a[j]) != "Intercept") {
      fp_lst_a[[j]] <- ggplot() +
        # gg(fp_a[[j]], aes(fill = logit_inv(sd/mean)), geom = "tile") +
        gg(fp_a[[j]], aes(fill = sd/mean), geom = "tile") +
        labs(fill=expression(c['v']), x="",y="") +
        scale_fill_viridis_c(
          # limits = c(0.5, 1), 
                             values = sc, option = "D") +
        geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
        # ggtitle(paste0(names(fp_a[j])))
        ggtitle(gsub("^((log_[^_]+)|([^_]+)).*", "\\1", paste0(names(fp_a[j]))))
    } else {
      fp_lst_a[[j]] <- ggplot() + gg(fp_a[[j-1]],
                                     # aes(fill = logit_inv(sd/mean)), geom = "tile") + 
                                     aes(fill = sd/mean), geom = "tile") + 
        annotate(geom = 'label',
                 label = paste0("Intercept CV = ", logit_inv(fp_a[[j]]["sd"]/fp_a[[j]]["mean"])),
                 x = 400, y = 3150) +
        scale_fill_viridis_c(
          # limits = c(0.5, 1), 
                             values = sc, option = "D") +
        geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
        labs(fill=expression(c['v']), x="",y="") +
        # ggtitle(paste0(names(fp_a[j-1])))
      ggtitle(gsub("^((log_[^_]+)|([^_]+)).*", "\\1", paste0(names(fp_a[j-1]))))
    }
  }
  wrap_plots(fp_lst_a[-2], ncol = 2
             # , guides = "collect"
             ) + 
    plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
  print(paste0(deparse(ff, width.cutoff = 150L)))
  ggsave(paste0("figures/model/", "fp_a", nm_chess, "_cv.pdf"), width = .75*tw, height =  .75*tw)
  ggsave(paste0("figures/model/", "fp_a", nm_chess, "_cv.png"), width = .75*tw, height =  .75*tw, dpi = 150)
  
  fp_b %<-% {
    predict(fit6b,
            newdata = pxl,
            formula = fml_mu_(fml6b), # say fit6b
            n.samples = 100, seed = seed[1]
    )
  }
  fp_lst_b <- list()
  
   for (j in 1:length(fp_b)) {
    ff <- fit6b$bru_info$lhoods[[1]]$formula # say fit6b
    if (names(fp_b[j]) != "Intercept") {
      fp_lst_b[[j]] <- ggplot() +
        # gg(fp_b[[j]], aes(fill = logit_inv(sd/mean)), geom = "tile") +
        gg(fp_b[[j]], aes(fill = sd/mean), geom = "tile") +
        # scale_fill_viridis_c(limits = c(0.5, 1), values = sc, option = "D") +
        scale_fill_viridis_c(values = sc, option = "D") +
        geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
        labs(fill=expression(c['v']), x="",y="") +
        # ggtitle(paste0(names(fp_b[j])))
        ggtitle(gsub("^((log_[^_]+)|([^_]+)).*", "\\1", paste0(names(fp_b[j]))))
    } else {
      fp_lst_b[[j]] <- ggplot() + gg(fp_b[[j-1]],
                                     # aes(fill = logit_inv(sd/mean)), geom = "tile") + 
                                     aes(fill = sd/mean), geom = "tile") + 
        annotate(geom = 'label',
                 label = paste0("Intercept CV = ", logit_inv(fp_b[[j]]["sd"]/fp_b[[j]]["mean"])),
                 x = 400, y = 3150) +
        # scale_fill_viridis_c(limits = c(0.5, 1), values = sc, option = "D") +
        scale_fill_viridis_c(values = sc, option = "D") +
        geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
        labs(fill=expression(c['v']), x="",y="") +
        # ggtitle(paste0(names(fp_b[j-1])))
        ggtitle(gsub("^((log_[^_]+)|([^_]+)).*", "\\1", paste0(names(fp_b[j-1]))))
    }
  }
  wrap_plots(fp_lst_b[-2], ncol = 2
             # , guides = "collect"
             ) + 
    plot_annotation(paste0(deparse(ff, width.cutoff = 150L)))
  print(paste0(deparse(ff, width.cutoff = 150L)))
  ggsave(paste0("figures/model/", "fp_b", nm_chess, "_cv.pdf"), width = .75*tw, height =  .75*tw)
  ggsave(paste0("figures/model/", "fp_b", nm_chess, "_cv.png"), width = .75*tw, height =  .75*tw, dpi = 150)
  
  
  # ggplot() +
  #   gg(fp6a_, aes(fill = logit_inv(sd/mean)), geom = "tile") +
  #   scale_fill_viridis_c(option = "D") +
  #   geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
  #   # ggtitle(paste0("fp6a DEM CV"))
  #   ggtitle(paste0("fp6a landcover CV"))
  # ggsave("figures/model/test.pdf")
  
if(FALSE){
  

  for (i in 1:length(pred_lst_a)) {
    p_lst_a <- list()
    for (j in 1:length(get(pred_lst_a[i]))) {
      ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
      if (names(get(pred_lst_a[[i]])[j]) != "Intercept") {
        p_lst_a[[j]] <- ggplot() +
          gg(get(pred_lst_a[[i]])[[j]], aes(fill = logit_inv(sd/mean)), geom = "tile") +
          scale_fill_viridis_c(option = "D") +
          geom_sf(data = st_as_sfc(bnd), fill = NA, color = "red") +
          # theme_minimal() +
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
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_cv.pdf"), width = .75*tw, height =  .75*tw)
    ggsave(paste0("figures/model/", pred_lst_a[i], nm_chess, "_cv.png"), width = .75*tw, height =  .75*tw, dpi = 150)
  }

  
}
  if (FALSE) {
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
            theme_minimal() +
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
      ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, "_cv.pdf"), width = .75*tw, height =  .75*tw)
      ggsave(paste0("figures/model/", pred_lst_b[i], nm_chess, "_cv.png"), width = .75*tw, height =  .75*tw)
    }
  }