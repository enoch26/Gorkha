# residuals analysis  -----------------------------------------------------
# https://inlabru-org.github.io/inlabru/articles/2d_lgcp_residuals.html
plan(multicore, workers = 4)
source("2d_lgcp_residuals_functions_sf.R")

# Store the required matrices and data frames for residual computation

# As <- prepare_residual_calculations(
#   samplers = bnd, domain = mesh_fm,
#   observations = landslides_c
# )
# tr_lst_a %<-% {
#   sink("figures/r_lst_a.txt")
#   r_lst_a <- list()
#   for (i in 1:length(mod_names_a)) {
#     r_lst_a[[i]] <- residual_df(
#       get(mod_names_a[i]), As$df,
#       str2expression(paste0("exp(", get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3], ")")),
#       As$A_sum, As$A_integrate
#     )
#     print(mod_names_a[i])
#     print(r_lst_a[[i]])
#   }
#   # print(r_lst_a)
#   sink()
# }
# 
# tr_lst_b %<-% {
#   sink("figures/r_lst_b.txt")
#   r_lst_b <- list()
#   for (i in 1:length(mod_names_b)) {
#     r_lst_b[[i]] <- residual_df(
#       get(mod_names_b[i]), As$df,
#       str2expression(paste0("exp(", get(mod_names_b[i])$bru_info$lhoods[[1]]$formula[3], ")")),
#       As$A_sum, As$A_integrate
#     )
#     print(mod_names_b[i])
#     print(r_lst_b[[i]])
#   }
#   # print(r_lst_b)
#   sink()
# }

# plan(sequential)

# grid residuals -----------------------------------------------------------
# plan(multicore, workers = 4)
grid_mesh <- partition(samplers = bnd, resolution = c(20, 10))

As_grid <- prepare_residual_calculations(
  samplers = grid_mesh, domain = mesh_fm,
  observations = landslides_c
)

r_grid_lst_a <- list()
r_grid_lst_b <- list()

tr_grid_a %<-% {
  sink("figures/r_grid_lst_a.txt")
  for (i in 1:length(mod_names_a)) {
    r_grid_lst_a[[i]] <- residual_df(
      get(mod_names_a[i]), As_grid$df,
      str2expression(paste0("exp(", get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3], ")")),
      As_grid$A_sum, As_grid$A_integrate
    )
    print(mod_names_a[i])
    print(r_grid_lst_a[[i]])
  }
  # print(r_grid_lst_a)
  sink()
}


# future.apply ------------------------------------------------------------
library(future.apply)
plan(multicore)
sink("figures/r_grid_lst_a.txt")
tr_grid_a <- future_lapply(1:length(mod_names_a), function(i) {
  r_grid_lst_a[[i]] <- residual_df(
    get(mod_names_a[i]), As_grid$df,
    str2expression(paste0("exp(", get(mod_names_a[i])$bru_info$lhoods[[1]]$formula[3], ")")),
    As_grid$A_sum, As_grid$A_integrate
  )
  print(mod_names_a[i])
  print(r_grid_lst_a[[i]])
})
sink()

########################
tr_grid_b %<-% {
  sink("figures/r_grid_lst_b.txt")
  for (i in 1:length(mod_names_b)) {
    r_grid_lst_b[[i]] <- residual_df(
      get(mod_names_b[i]), As_grid$df,
      str2expression(paste0(get(mod_names_b[i])$bru_info$lhoods[[1]]$formula[3])),
      As_grid$A_sum, As_grid$A_integrate
    )
    print(mod_names_b[i])
    print(r_grid_lst_b[[i]])
  }
  # print(r_grid_lst_b)
  sink()
}

plan(sequential)

plan(multicore, workers = 4)
# Set the colour scale
csc_part <- 10
  grid_range_a <- rbind(
    unlist(r_grid_lst_a)
  )
  grid_csc <- set_csc(grid_range_a, rep("RdBu", csc_part))
  pr_grid_lst_a <- list()

tpr_grid_lst_a %<-% {

  for (i in 1:length(mod_names_a)) {
    pr_grid_lst_a[[i]] <- residual_plot(grid_mesh, r_grid_lst_a[[i]], grid_csc, mod_names_a[i])
  }
  wrap_plots(pr_grid_lst_a, ncol = 2, guides = "collect") + plot_annotation("Grid Residuals")
  ggsave("figures/pr_grid_lst_a.pdf", width = tw, height = tw / 2)
}


  grid_range_b <- rbind(
    unlist(r_grid_lst_b)
  )
  grid_csc <- set_csc(grid_range_b, rep("RdBu", csc_part))

  pr_grid_lst_b <- list()
tpr_grid_lst_b %<-% {
  for (i in 1:length(mod_names_b)) {
    pr_grid_lst_b[[i]] <- residual_plot(grid_mesh, r_grid_lst_b[[i]], grid_csc, mod_names_b[i])
  }
  wrap_plots(pr_grid_lst_b, ncol = 2, guides = "collect") + plot_bnnotation("Grid Residuals")
  ggsave("figures/pr_grid_lst_b.pdf", width = tw, height = tw / 2)
}
