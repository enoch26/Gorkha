# empirical score diff
if(FALSE){
  # this is correct from the supervision meeting
  LS_diff <- f1a_score$pred$obs_prob$LS - f6a_score$pred$obs_prob$LS
  LS_var_diff <- var(LS_diff)
  LS_ci <- mean(LS_diff) +
    c(-1, 1) * qnorm(0.975) * sqrt(LS_var_diff/length(LS_diff))
  
  crps_diff <- crps1a - crps6a
  crps_var_diff <- var(crps_diff)
  crps_ci <- mean(crps_diff) +
    c(-1, 1) * qnorm(0.975) * sqrt(mean(var_diff)/length(crps_diff))
}

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


df_ %>% mutate(AE_score_diff = AE - AE_ref,
               RMSE_score_diff = RMSE - RMSE_ref,
               DS_score_diff = DS - DS_ref,
               LS_score_diff = LS - LS_ref,
               CRPS_score_diff = CRPS - CRPS_ref)


for (i in (c("AE_score_diff","RMSE_score_diff", "DS_score_diff", "LS_score_diff", "CRPS_score_diff"))) {
  df_ %>% group_by(Model) %>%  
    mutate(var_diff = var(i)) %>% 
    mutate(score_ci = mean(i) +
             c(-1, 1) * qnorm(0.975) * sqrt(var_diff/length(i))) 

}

# TODO empirical score diff with 95% CI and fitb



