# for CV ------------------------------------------------------------------
# https://stackoverflow.com/questions/75584181/how-to-layer-two-geom-sf-layers-in-ggplot-with-two-different-scale-fill-gradient
# https://inlabru-org.github.io/inlabru/articles/zip_zap_models.html


# compute for grid_sf -----------------------------------------------------
if(CV_chess){
plan(multicore, workers = 2)
cv_grid$black$count_test <- lengths(st_intersects(cv_grid$black, landslides_c_test))
fp1a_chess %<-% {cv_chess(fit1a, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp1b_chess %<-% {cv_chess(fit1b, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp2a_chess %<-% {cv_chess(fit2a, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp2b_chess %<-% {cv_chess(fit2b, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp3a_chess %<-% {cv_chess(fit3a, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp3b_chess %<-% {cv_chess(fit3b, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp4a_chess %<-% {cv_chess(fit4a, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
fp4b_chess %<-% {cv_chess(fit4b, mesh = mesh_fm, cv_grid = cv_grid$black, n.samples = 100)}
plan(sequential)

# TODO compute scoes from here

}


# TODO Compute grid score
if(FALSE){
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
            formula = formula_, n.samples = 10)
  }
}


# AE SE DS Log scores ---------------------------------------------------------------


if(FALSE){
    # pxl <- st_as_sf(as.points(pxl_terra))
  fit1a_logs <- log_score(fit1a, newdata = pxl, test_obs = "count_test")
  fit2a_logs <- log_score(fit2a, newdata = pxl, test_obs = "count_test")
  fit3a_logs <- log_score(fit3a, newdata = pxl, test_obs = "count_test")
  fit4a_logs <- log_score(fit4a, newdata = pxl, test_obs = "count_test")
  f1a_dse <- dse_score_terra(fit1a, newdata = pxl, testdata = pxl_terra, test_obs = "count_test")
  f2a_dse <- dse_score_terra(fit2a, newdata = pxl, testdata = pxl_terra, test_obs = "count_test")
  f3a_dse <- dse_score_terra(fit3a, newdata = pxl, testdata = pxl_terra, test_obs = "count_test")
  f4a_dse <- dse_score_terra(fit4a, newdata = pxl, testdata = pxl_terra, test_obs = "count_test")
  
  f1a_dse$mean_score
  f2a_dse$mean_score
  f3a_dse$mean_score
  f4a_dse$mean_score
  # ksn_tag rw1
  # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.670         -1.47
  # # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.646         -1.43
  # # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.675         -1.46
  # # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.669         -1.43
  
  # ksn_tag rw2 
  # fml1a <- geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + rf2ch
  # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.676         -1.46
  # geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag 
  # # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.648         -1.43
  # # A tibble: 1 × 2
  # geometry ~ Intercept + pga_mean_raster + landuse + nepal_geo + ksn_tag + rf2ch_inv_rw2 
  # rf2ch_inv_rw2 is just linear 
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.675         -1.46
  # # A tibble: 1 × 2
  # SE_score_mean DS_score_mean
  # <dbl>         <dbl>
  #   1         0.678         -1.46
  
}

# TODO plot the difference across DSE maps
plan(multicore, workers = 2)

f1a_score %<-% {score(fit1a, newdata = pxl, obs = "count_test", seed = seed)}
f2a_score %<-% {score(fit2a, newdata = pxl, obs = "count_test", seed = seed)}
f3a_score %<-% {score(fit3a, newdata = pxl, obs = "count_test", seed = seed)}
f4a_score %<-% {score(fit4a, newdata = pxl, obs = "count_test", seed = seed)}
f5a_score %<-% {score(fit5a, newdata = pxl, obs = "count_test", seed = seed)}


f1a_score$mean
f2a_score$mean
f3a_score$mean
f4a_score$mean
f5a_score$mean

ggplot() + gg(f1a_score$pred$obs_prob["LS"], geom = "tile")
plan(sequential)

if(FALSE){
  se_sc <- scale_fill_viridis_c(
    limits = c(-200, 225),
    name = "SE",
    na.value = "transparent"
  )
  
  p12 <- ggplot() + geom_spatraster(data =  f1a_dse$score$SE_score_terra - f2a_dse$score$SE_score_terra) + 
    se_sc + ggtitle("SE Score Difference")
  p13 <- ggplot() + geom_spatraster(data =  f1a_dse$score$SE_score_terra - f3a_dse$score$SE_score_terra) + 
    se_sc + ggtitle("SE Score Difference")
  p14 <- ggplot() + geom_spatraster(data =  f1a_dse$score$SE_score_terra - f4a_dse$score$SE_score_terra) + 
    se_sc + ggtitle("SE Score Difference")
  # p12
  # ggsave("figures/model/f1a_f2a_se_diff.pdf", width = tw, height = tw/2)
  # p13
  # ggsave("figures/model/f1a_f3a_se_diff.pdf", width = tw, height = tw/2)
  # p14
  # ggsave("figures/model/f1a_f4a_se_diff.pdf", width = tw, height = tw/2)
  
  ls_p_se <- list(p12, p13, p14)
  
  wrap_plots(ls_p_se, guides = "collect", byrow = TRUE, ncol = 3
  ) & theme(plot.title = element_text(size = 15),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            legend.position = "right")
  ggsave("figures/SE_diff.pdf", width = tw, height = tw / 2.5)
  
  
  # ggplot() + geom_spatraster(data =  f1a_dse$score$DS_score_terra - f2a_dse$score$DS_score_terra ) + 
  #   scale_fill_viridis_c(na.value = "transparent") + ggtitle("DS Score Difference")
  # ggsave("figures/model/f1a_f2a_ds_diff.pdf", width = tw, height = tw/2)
  # ggplot() + geom_spatraster(data =  f1a_dse$score$DS_score_terra - f3a_dse$score$DS_score_terra ) + 
  #   scale_fill_viridis_c(na.value = "transparent") + ggtitle("DS Score Difference")
  # ggsave("figures/model/f1a_f3a_ds_diff.pdf", width = tw, height = tw/2)
  # ggplot() + geom_spatraster(data =  f1a_dse$score$DS_score_terra - f4a_dse$score$DS_score_terra ) + 
  #   scale_fill_viridis_c(na.value = "transparent") + ggtitle("DS Score Difference")
  # ggsave("figures/model/f1a_f4a_ds_diff.pdf", width = tw, height = tw/2)
  
}



if(FALSE){
  fit1a_dse <- {dse_score(fit1a, pxl, n.samples = 100)}
  fit2a_dse <- {dse_score(fit2a, pxl, n.samples = 100)}
  fit3a_dse <- {dse_score(fit3a, pxl, n.samples = 100)}
  fit4a_dse <- {dse_score(fit4a, pxl, n.samples = 100)}
  dse_ls <- c("fit1a_dse", "fit2a_dse", "fit3a_dse", "fit4a_dse") 
  
  
  for(i in dse_ls){
    ggplot() +
      gg(data = get("pred", get(i))["SE_score"], geom = "tile") +
      scale_fill_viridis_c(trans="log") +
      ggtitle("Squared Error Score")
    ggsave(paste0(i, "_se.pdf"), width = tw, height = tw/2)
  }
  for(i in dse_ls){
    ggplot() +
      gg(data = get("pred", get(i))["DS_score"], geom = "tile") +
      scale_fill_viridis_c(trans="log") +
      ggtitle("Dawid-Sebastiani Score")
    ggsave(paste0(i, "_ds.pdf"), width = tw, height = tw/2)
  }
  
  # TODO match each newdata log Area sizes 
  if(FALSE){
    fit1b_dse %<-% {dse_score(fit1b, pxl, n.samples = 100)}
    fit2b_dse %<-% {dse_score(fit2b, pxl, n.samples = 100)}
    fit3b_dse %<-% {dse_score(fit3b, pxl, n.samples = 100)}
    fit4b_dse %<-% {dse_score(fit4b, pxl, n.samples = 100)}
    
  }
}



if(FALSE){
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

