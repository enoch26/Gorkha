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

# crps1b %<-% crps_(fit1b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed) 
# crps2b %<-% crps_(fit2b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
# crps3b %<-% crps_(fit3b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
# crps4b %<-% crps_(fit4b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
# crps5b %<-% crps_(fit5b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)
# crps6b %<-% crps_(fit6b, newdata = landslides_c_test, obs = "logarea_m2", n.samples = 1000, seed = seed)

scores_b <- bind_rows(
  f1b_score$mean_score,
  f2b_score$mean_score,
  f3b_score$mean_score,
  f4b_score$mean_score,
  f5b_score$mean_score,
  f6b_score$mean_score
  # bind_cols(f1a_score$mean_score, CRPS_mean = mean(crps1b)),
  # bind_cols(f2a_score$mean_score, CRPS_mean = mean(crps2b)),
  # bind_cols(f3a_score$mean_score, CRPS_mean = mean(crps3b)),
  # bind_cols(f4a_score$mean_score, CRPS_mean = mean(crps4b)),
  # bind_cols(f5a_score$mean_score, CRPS_mean = mean(crps5b)),
  # bind_cols(f6a_score$mean_score, CRPS_mean = mean(crps6b))
) %>%
  bind_cols(data.frame(
    Model = mod_names_b
  ), .)



sink("figures/model/cv/score_thin_b.txt", append = TRUE)
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

for (i in (c("AE", "SE", "DS", "LS"))) {
  # if(i == "CRPS"){
  #   ggplot() +
  #     stat_ecdf(aes(x = crps1a, col = "fit1a", alpha = .5)) +
  #     stat_ecdf(aes(x = crps2a, col = "fit2a", alpha = .5)) +
  #     stat_ecdf(aes(x = crps3a, col = "fit3a", alpha = .5)) +
  #     stat_ecdf(aes(x = crps4a, col = "fit4a", alpha = .5)) +
  #     stat_ecdf(aes(x = crps5a, col = "fit5a", alpha = .5)) +
  #     stat_ecdf(aes(x = crps6a, col = "fit6a", alpha = .5)) +
  #     scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  #     scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  #     xlab(i) + ylab(paste0("Empirical ", i, " CDF"))
  #   ggsave(paste0("figures/model/cv/", i, "ecdf_", cv_thin_resol[1], ".pdf"), width = tw, height = tw/2)
  #   
  # }else{
  
  ggplot() +
    stat_ecdf(data = f1b_score$pred$obs_prob, aes(x = .data[[i]], col = "fit1b", alpha = .5)) +
    stat_ecdf(data = f2b_score$pred$obs_prob, aes(x = .data[[i]], col = "fit2b", alpha = .5)) +
    stat_ecdf(data = f3b_score$pred$obs_prob, aes(x = .data[[i]], col = "fit3b", alpha = .5)) +
    stat_ecdf(data = f4b_score$pred$obs_prob, aes(x = .data[[i]], col = "fit4b", alpha = .5)) +
    stat_ecdf(data = f5b_score$pred$obs_prob, aes(x = .data[[i]], col = "fit5b", alpha = .5)) +
    stat_ecdf(data = f6b_score$pred$obs_prob, aes(x = .data[[i]], col = "fit6b", alpha = .5)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    xlab(i) + ylab(paste0("Empirical ", i, " CDF"))
  ggsave(paste0("figures/model/cv/", i, "ecdf_b", cv_thin_resol[1], ".pdf"), width = tw, height = tw/2)
  # }
}