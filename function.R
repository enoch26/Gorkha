# hexagon lattice  --------------------------------------------------------


# TODO lattice # function in x
# bnd: boundary
# x_bin: # of bin in x axis
# edge_len_n: # of edge length of mesh from the boundary to create hexagon mesh using x_bin
# S2 not supported
hexagon_lattice <- function(bnd = bnd,
                            x_bin = 250, # 300 then running forever
                            edge_len_n = 1) {
  stopifnot(x_bin / 2 > edge_len_n)
  crs <- fm_crs(bnd)
  fm_crs(bnd) <- NA
  # TODO rep the values separately, one grid n values and one grid n-1,
  # two separate grid and combine
  edge_len <- as.numeric((fm_bbox(bnd)[[1]][2] - fm_bbox(bnd)[[1]][1])) / (x_bin + 2 * edge_len_n) # two ends
  # sf_buffer to work on negative buffer to stay a distance from the boundary
  # Turn off S2 to avoid zig zag
  # suppressMessages(sf::sf_use_s2(FALSE))
  bnd_inner <- st_buffer(bnd, dist = -edge_len_n * edge_len) # st_buffer for edge_len x1
  y_diff <- fm_bbox(bnd_inner)[[2]][2] - fm_bbox(bnd_inner)[[2]][1]
  x_diff <- fm_bbox(bnd_inner)[[1]][2] - fm_bbox(bnd_inner)[[1]][1]
  y_bin <- as.integer(y_diff / (sqrt(3) / 2 * edge_len))
  # TODO rep n, n-1, length
  h <- (sqrt(3) / 2 * edge_len) # height
  x_adj <- .5 * (x_diff - x_bin * edge_len)
  y_adj <- .5 * (y_diff - y_bin * h)
  # x
  x_1_ <- seq(
    fm_bbox(bnd_inner)[[1]][1] + x_adj,
    fm_bbox(bnd_inner)[[1]][2] - x_adj, edge_len
  )
  x_2_ <- seq(
    (fm_bbox(bnd_inner)[[1]][1] + x_adj + .5 * edge_len),
    (fm_bbox(bnd_inner)[[1]][2] - x_adj - .5 * edge_len),
    edge_len
  )
  y_1_ <- seq(
    fm_bbox(bnd_inner)[[2]][1] + y_adj,
    fm_bbox(bnd_inner)[[2]][2] - y_adj,
    by = 2 * h
  )
  y_2_ <- seq(
    fm_bbox(bnd_inner)[[2]][1] + y_adj + h,
    fm_bbox(bnd_inner)[[2]][2] - y_adj + h, 2 * h
  )
  
  x_1 <- rep(x_1_, times = length(y_1_))
  x_2 <- rep(x_2_, times = length(y_2_))
  y_1 <- rep(y_1_, each = length(x_1_))
  y_2 <- rep(y_2_, each = length(x_2_))
  
  mesh_df <- data.frame(x = c(x_1, x_2), y = c(y_1, y_2))
  # turn the mesh nodes into lattice sf
  lattice_sf <- st_as_sf(mesh_df, coords = c("x", "y"), crs = st_crs(bnd))
  # ggplot() + geom_sf(data=bnd) + geom_sf(data=bnd_inner) + gg(lattice_sf)
  lattice_sfc <- lattice_sf %>% st_as_sfc()
  # https://stackoverflow.com/questions/62442150/why-use-st-intersection-rather-than-st-intersects
  # https://gis.stackexchange.com/questions/394954/r-using-st-intersects-to-classify-points-inside-outside-and-within-a-buffer
  pts_inside <- lengths(st_intersects(lattice_sfc, bnd_inner)) != 0
  # plot(bnd)
  # plot(lattice_sfc[pts_inside], pch=19, col="blue", add=TRUE)
  pts_lattice_sfc <- lattice_sfc[pts_inside]
  fm_crs(pts_lattice_sfc) <- fm_crs(bnd_inner) <- crs
  return(list(
    lattice = pts_lattice_sfc,
    edge_len = edge_len,
    bnd_inner = bnd_inner
  ))
  # nepal_lattice_sf <- st_intersection(lattice_sf, bnd_inner) # this is too slow
  # nepal_mesh <- fm_mesh_2d(
  #   loc = nepal_lattice,
  #   boundary = fm_as_segm(bnd),
  #   max.edge = c(edge_len, 3 * edge_len),
  #   crs = fm_CRS(bnd)
  # )
  #
  # nepal_mesh <- inla.mesh.2d(
  #   boundary = fm_as_segm(bnd),
  #   max.edge = c(edge_len, 3 * edge_len),
  #   crs = fm_CRS(bnd)
  # )
}

# split.edges <- function(segm, n) {
#   if (is.null(segm) || (nrow(segm$idx) == 0)) {
#     return(segm)
#   }
#   n.loc <- nrow(segm$loc)
#   n.idx <- nrow(segm$idx)
#   loc <- do.call(
#     rbind,
#     c(
#       list(segm$loc),
#       lapply(
#         seq_len(n),
#         function(k) {
#           (segm$loc[segm$idx[, 1], ] * k / (n + 1) +
#              segm$loc[segm$idx[, 2], ] * (n - k + 1) / (n +
#                                                           1))
#         }
#       )
#     )
#   )
#   idx <- do.call(
#     rbind,
#     c(
#       list(cbind(
#         segm$idx[, 1], n.loc + seq_len(n.idx)
#       )),
#       lapply(
#         seq_len(n - 1),
#         function(k) {
#           cbind(
#             n.loc * k + seq_len(n.idx),
#             n.loc * (k + 1) + seq_len(n.idx)
#           )
#         }
#       ),
#       list(cbind(
#         n.loc * n + seq_len(n.idx), segm$idx[, 2]
#       ))
#     )
#   )
#
#   segm2 <-
#     fm_segm(
#       loc = loc,
#       idx = idx,
#       grp = rep(segm$grp, n + 1),
#       is.bnd = segm$is.bnd
#     )
#
#   segm2
# }



# log_score ---------------------------------------------------------------

log_score <- function(object,
                      newdata,
                      # formula = NULL,
                      n.samples = 100,
                      # data_obs = "lambda",
                      test_obs = "count",
                      # inla.link = NULL,
                      seed = NULL,
                      plot = FALSE,
                      ...){
  fml <- object$bru_info$lhoods[[1]]$formula
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  formula_ <- as.formula(paste0(" ~ dpois(", test_obs, ", lambda = exp(", ff, "))"))
  # pxl[obs] <- as.points(pxl_terra[obs])
  pred <- predict(object,
                  newdata = newdata,
                  formula = formula_,
                  n.samples = n.samples
  )
  pred$LS <- -log(pred$mean)
  return(pred)
}


# sf2sr -------------------------------------------------------------------
# Convert sf points object from predict.bru() to Spatraster object
# x: sf points object, usually from predict(..., fm_pixel(..., format = "sf"))
# y: Spatraster object, usually from fm_pixel(..., format = "terra")
sf2sr <- function(x, y, field = "mean", fun = "mean", ...){
  rasterize(vect(x), field = field, y, fun = fun, ...)
}

# score -------------------------------------------------------------------

## sf -------------------------------------------------------------------


# TODO extend to list of formulae, too much for a function, probably give up
# TODO for a list of bru objects with the same formula to compare the SE and DS score
# A function to compute the SE and DS score for bru object(s)
# newdata: sf points object from fm_pixel(mesh,...)
# testdata: test data in Spatraster and ideally same resolution as the newdata, otherwise mean across cells is assumed
# obs: the response name in the newdata/testdata to compare with the prediction
# formula(e): the formula to predict the response;
#             if NULL, the formula in the model is used;
#             TODO for a list of formulae
# seed: seed for reproducibility in the the global seed and prediction;
#       seed restored after the computation
# plot: logical, whether to plot the SE and DS score
# inla.link: the INLA link function suffix (can be found in INLA::link), eg "log"
# to inverse the prediction
# set seed issue: https://github.com/inlabru-org/inlabru/issues/88
# now only work for Gaussian (and Poisson, because Poisson we can turn in Gaussian)
score <- function(object,
                      newdata = NULL,
                      # mesh,
                      cv_grid = NULL,
                      formula = NULL,
                      n.samples = 1000,
                      obs = "count",
                      inla.link = NULL,
                      seed = NULL,
                      plot = FALSE,
                      ...) {
  # old_seed <- .Random.seed
  # on.exit(expr = {
  #   .Random.seed <- old_seed
  # }, add = TRUE) # restore the seed
  # if the second one is not provided, generate the second seed for reproducibility.
  # check the vector of seed len 2
  


  if (length(seed) > 2) {
    stop("At most two seeds should be provided")
  }

  if (length(seed) >= 1) {
    set.seed(seed[1])
  }

  if (length(seed) >= 2) {
    seed2 <- seed[2]
  } else {
    seed2 <- as.integer(runif(1) * .Machine$integer.max) # this is how INLA set seed
  }

  # TODO check multiple likelihood here
  # ask user which likelihood to pick
  # https://stackoverflow.com/questions/11007178/creating-a-prompt-answer-system-to-input-data-into-r
  # expected to be single character class
  # TODO should stop and ask question which likelihood to take
  if (is.null(inla.link)) {
    inla.link <- object$misc$linkfunctions[["names"]] # TODO what if multiple likelihood
    if (length(inla.link) > 1) {
      # stop(paste0("Multiple link functions (",
      #             paste0(inla.link, collapse = ", "),
      #             ") detected. Please provide an inla.link argument"))
      warning(paste0(
        "Multiple link functions (",
        paste0(inla.link, collapse = ", "),
        ") detected. Please provide an inla.link argument"
      ))
    }
  }

  # Use the above seed to reproduce the same result and into the predict call
  if(FALSE){
    if (is.null(formula)) {
      # TODO auto detect matching (maybe) object$bru_info$lhoods[[1]]$inla.family
      # TODO user to provide .block in newdata
      if(FALSE){
        formula <- object$bru_info$lhoods[[1]]$formula
        formula <- as.formula(paste0(
          "~ inla.link.", inla.link,
          "(", labels(terms(formula)), ", inverse = TRUE)"
        ))
      }
  }
    

  }

  #################################################################
  fml <- object$bru_info$lhoods[[1]]$formula
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  print("formula: ")
  print(ff)
  # formula <- as.formula(paste0(" ~  exp(", ff, ")"))
  
  # formula <- as.formula(paste(
  #   paste0(" ~ {expect <- exp(", ff, ")"),
  #   "\n",
  #   paste0(
  #     "list(expect = expect, obs_prob = dpois(", obs,", lambda = expect))}"
  #   )
  # ))
  
  if(is.null(cv_grid)){
    formula <- as.formula(paste(
      paste0(" ~ { expect <-  ", ff),
      "\n",
    "variance <- 1/exp(Precision_for_the_Gaussian_observations) ",
    "\n",
      paste0(
        "list(expect = expect, obs_prob = dnorm(", get(obs, newdata),
        ", mean = expect, sd = sqrt(variance)))}"
      )
    )
    )
    # formula <- as.formula(paste(
    #   paste0(" ~ { expect <-  ", ff), 
    #   "\n",
    #   paste0(
    #     "list(expect = expect)}"
    #   )
    # )
    # )
    
    pred <- predict(object,
                    newdata = newdata, formula = formula,
                    n.samples = n.samples,
                    seed = seed2,
                    ...
    )
    
    post_E <- get("mean", pred$expect)
    # post_median <- get("median", pred$expect)
    post_Var <- post_E + (get("sd", pred$expect))^2 # because of conditional 
    
    
    # pred$obs_prob$AE <- AE <- abs(get(obs, newdata) - post_median)
    pred$obs_prob$SE <- AE <- abs(get(obs, newdata) - post_E)
    pred$obs_prob$SE <- SE <- (get(obs, newdata) - post_E)^2
    pred$obs_prob$DS <- DS <- SE / post_Var + log(post_Var)
    # TODO log score
    pred$obs_prob$LS <- LS <- -log(pred$obs_prob$mean)
    # pred$obs_prob$LS_upper <- LS_upper <- -log(pred$obs_prob$mean + 2*pred$obs_prob$mean.mc_std_err)
    # pred$obs_prob$LS_lower <- LS_lower <- -log(pred$obs_prob$mean - 2*pred$obs_prob$mean.mc_std_err)
  }else{
    # Poisson point pattern
  agg_nc <- bru_mapper_logsumexp(rescale = FALSE)
  formula <- as.formula(paste(
    paste0(" ~ { expect <- ibm_eval(agg_nc, input = list(
                                        block = .block,
                                        weights = weight
                                      ),
                                      state = ", ff, ", log = FALSE)"),
    "\n",
    # "y <- ", obs,
    # "\n",
    paste0(
      # "list(expect = expect)}"
      "list(expect = expect, obs_prob = dpois(", get(obs, cv_grid), ", lambda = expect))}"
      # "list(expect = expect, obs_prob = dpois(", obs , "lambda = expect))}"
    )
  )
  )
  
  # pred <- predict(fit1a,
  #   newdata = cv_newdata, formula = formula_,
  #   n.samples = 5
  # )
  pred <- predict(object,
    newdata = newdata, formula = formula,
    n.samples = n.samples,
    seed = seed2,
    ...
  )
  
  post_E <- get("mean", pred$expect)
  # post_median <- get("median", pred$expect)
  post_Var <- post_E + (get("sd", pred$expect))^2
  
  # pred$obs_prob$AE <- AE <- abs(get(obs, cv_grid) - post_median)
  pred$obs_prob$AE <- AE <- abs(get(obs, cv_grid) - post_E)
  pred$obs_prob$SE <- SE <- (get(obs, cv_grid) - post_E)^2

  
  pred$obs_prob$DS <- DS <- SE / post_Var + log(post_Var)
  # log score
  # pred$obs_prob$LS <- LS <- -log(dpois(get(obs, cv_grid), lambda = post_E))
  pred$obs_prob$LS <- LS <- -log(pred$obs_prob$mean)
  # pred$obs_prob$LS_upper <- LS_upper <- -log(pred$obs_prob$mean + 2 * pred$obs_prob$mean.mc_std_err)
  # pred$obs_prob$LS_lower <- LS_lower <- -log(pred$obs_prob$mean - 2 * pred$obs_prob$mean.mc_std_err)
  
  }
  AE_mean <- mean(AE, na.rm = TRUE)
  SE_mean <- mean(SE, na.rm = TRUE)
  DS_mean <- mean(DS, na.rm = TRUE)
  LS_mean <- mean(LS, na.rm = TRUE)
  # LS_mean_upper <- mean(LS_upper, na.rm = TRUE)
  # LS_mean_lower <- mean(LS_lower, na.rm = TRUE)
  cat(
    "Mean AE score: ", AE_mean, "\n",
    "Mean SE score: ", SE_mean, "\n",
    "Mean DS score: ", DS_mean, "\n",
    "Mean log score: ", LS_mean, "\n"
    # "Mean log upper score: ", LS_mean_upper, "\n",
    # "Mean log lower score: ", LS_mean_lower, "\n"
  )
  
  # An example of how to plot the SE and DS score
  if (plot) {
    p1 <- ggplot() +
      gg(data = pred["SE"], geom = "tile") +
      ggtitle("Squared Error Score")
    p2 <- ggplot() +
      gg(data = pred["DS"], geom = "tile") +
      ggtitle("Dawid-Sebastiani Score")
    print(p1)
    print(p2)
    # Collect the plots
    # print(p1 / p2 + patchwork::plot_layout(guides = "collect"))
  }
  # https://stackoverflow.com/questions/28300713/how-and-when-should-i-use-on-exit

  return(list(
    pred = pred,
    mean_score = tibble::tibble(
      AE_mean = AE_mean,
      SE_mean = SE_mean,
      DS_mean = DS_mean,
      LS_mean = LS_mean
      # LS_mean_upper = LS_mean_upper,
      # LS_mean_lower = LS_mean_lower
    )
    # score = tibble::tibble(SE = SE, DS = DS)
    # cbind for user
    # plot = list(p1,p2)
  ))

}


## terra ----------------------------------------------------------------------


# TODO extend to list of formulae, too much for a function, probably give up
# TODO for a list of bru objects with the same formula to compare the SE and DS score
# A function to compute the SE and DS score for bru object(s)
# newdata: sf points object from fm_pixel(mesh,...)
# testdata: test data in Spatraster and ideally same resolution as the newdata, otherwise mean across cells is assumed
# obs: the response name in the newdata/testdata to compare with the prediction
# formula(e): the formula to predict the response;
#             if NULL, the formula in the model is used;
#             TODO for a list of formulae
# seed: seed for reproducibility in the the global seed and prediction;
#       seed restored after the computation
# plot: logical, whether to plot the SE and DS score
# inla.link: the INLA link function suffix (can be found in INLA::link), eg "log"
# to inverse the prediction
# set seed issue: https://github.com/inlabru-org/inlabru/issues/88
# now only work for Gaussian (and Poisson, because Poisson we can turn in Gaussian)
dse_score_terra <- function(object,
                      newdata = NULL,
                      testdata = NULL, 
                      formula = NULL,
                      n.samples = 100,
                      # data_obs = "lambda",
                      test_obs = "count",
                      inla.link = NULL,
                      seed = NULL,
                      plot = FALSE,
                      ...) {
  # old_seed <- .Random.seed
  # on.exit(expr = {
  #   .Random.seed <- old_seed
  # }, add = TRUE) # restore the seed
  # if the second one is not provided, generate the second seed for reproducibility.
  # check the vector of seed len 2

  if (length(seed) > 2) {
    stop("At most two seeds should be provided")
  }

  if (length(seed) >= 1) {
    set.seed(seed[1])
  }

  if (length(seed) >= 2) {
    seed2 <- seed[2]
  } else {
    seed2 <- as.integer(runif(1) * .Machine$integer.max) # this is how INLA set seed
  }

  # TODO check multiple likelihood here
  # ask user which likelihood to pick
  # https://stackoverflow.com/questions/11007178/creating-a-prompt-answer-system-to-input-data-into-r
  # expected to be single character class
  # TODO should stop and ask question which likelihood to take
  if (is.null(inla.link)) {
    inla.link <- object$misc$linkfunctions[["names"]] # TODO what if multiple likelihood
    if (length(inla.link) > 1) {
      # stop(paste0("Multiple link functions (",
      #             paste0(inla.link, collapse = ", "),
      #             ") detected. Please provide an inla.link argument"))
      warning(paste0(
        "Multiple link functions (",
        paste0(inla.link, collapse = ", "),
        ") detected. Please provide an inla.link argument"
      ))
    }
  }

  # Use the above seed to reproduce the same result and into the predict call
  if(FALSE){
    if (is.null(formula)) {
      # TODO auto detect matching (maybe) object$bru_info$lhoods[[1]]$inla.family
      # TODO user to provide .block in newdata
      if(FALSE){
        formula <- object$bru_info$lhoods[[1]]$formula
        formula <- as.formula(paste0(
          "~ inla.link.", inla.link,
          "(", labels(terms(formula)), ", inverse = TRUE)"
        ))
      }
  }
    

  }

  # an old way to get the predictor
  # len_fml <- length(formula)
  # predictor <- as.character(
  #   as.expression(formula[[len_fml]])
  # )

  # Notes from the supervision meeting
  #     a <-  parse(text = "cos(x)")
  #     expression(a)
  #     form <- lambda ~ exp(cos(x))
  #     as.formula(paste0("~ exp(", as.character(a), ")"))
  # https://rdrr.io/github/andrewzm/INLA/man/link-functions.html
  # control.predictor = list(compute = TRUE, link = 1))
  # formula <- as.formula(paste0(
  #   "~ inla.link.", inla.link,
  #   "(", predictor, ", inverse = TRUE)"
  # ))


  # TODO check inla.link earlier to see if its valid
  # else if (!is.null(inla.link)) {
  # TODO check if that inla.link exist and trycatch that error
  # }

  # if(obs=="lambda") {
  #   obs <- "lambda"
  # } else {
  #   stop("Only lambda is supported for now")
  # }
  
  #################################################################
  fml <- object$bru_info$lhoods[[1]]$formula
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  formula <- as.formula(paste0(" ~  exp(", ff, ")"))
  
  pred <- predict(object,
    newdata = newdata, formula = formula,
    n.samples = n.samples,
    seed = seed2,
    ...
  )
  
  post_E_terra <- sf2sr(x = pred, field = "mean", y = pxl_terra)
  post_Var_terra <- post_E_terra +  sf2sr(x = pred, field = "sd", y = pxl_terra)^2
  
  SE_score_terra <- (testdata[test_obs] - post_E_terra)^2
  DS_score_terra <- SE_score_terra / post_Var_terra + log(post_Var_terra)
  SE_score_mean <- mean(values(SE_score_terra), na.rm = TRUE)
  DS_score_mean <- mean(values(DS_score_terra), na.rm = TRUE)
  cat(
    "Mean SE score: ", SE_score_mean, "\n",
    "Mean DS score: ", DS_score_mean, "\n"
  )
  # An example of how to plot the SE and DS score
  if (plot) {
    p1 <- ggplot() +
      geom_spatraster(data = SE_score_terra) +
      scale_fill_viridis_c(na.value = "transparent", trans = "log") +
      ggtitle("Squared Error Score")
    # ggsave("se_terra.pdf")
    p2 <- ggplot() +
      # gg(data = log(DS_score_terra+10)) +
      gg(data = DS_score_terra) +
      scale_fill_viridis_c(na.value = "transparent") +
      ggtitle("Dawid-Sebastiani Score")
    print(p1)
    print(p2)
    # Collect the plots
    # print(p1 / p2 + patchwork::plot_layout(guides = "collect"))
  }
  # https://stackoverflow.com/questions/28300713/how-and-when-should-i-use-on-exit

  return(list(
    # pred = pred,
    mean_score = tibble::tibble(
      SE_score_mean = SE_score_mean,
      DS_score_mean = DS_score_mean
    ),
    score = list(SE_score_terra = SE_score_terra, DS_score_terra = DS_score_terra)
    # cbind for user
    # plot = list(p1,p2)
  ))
}


# make Q symmetric --------------------------------------------------------
# thanks to Stephen Villejo for sharing

# A function to make a symmetric matrix for the Q matrix from the INLA model
make_sym <- function(Q) {
  d <- diag(Q)
  Q <- (Q + t(Q)) / 2
  diag(Q) <- d
  return(Q)
}



# prediction plot ---------------------------------------------------------

if(FALSE){
  
predict_plot <- function(fit, mesh, n.samples = 100) {
  pxl <- fm_pixels(mesh_fm, dims = c(400, 200), mask = bnd)

  pred_pxl <- predict(fit,
    newdata = pxl,
    formula = fit$bru_info$lhoods[[1]]$formula,
    n.samples = 100
  )

  ggplot() +
    gg(pred_pxl["mean"], geom = "tile") +
    scale_fill_viridis_c() +
    geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3)
  ggsave(here("figures", paste0(fit, "pred_pxl.pdf")), width = tw, height = tw / 2)
}
}


# predict_plot <- function(fit, newdata, formula, n.samples = 100){
#   pred <- predict(fit,
#                   newdata = newdata,
#                   formula = ~ list(lambda = formula),
#                   n.samples = n.samples
#   )
#   # newdata$lambda_post <- pred$lambda$mean
#   p_pred <- ggplot() +
#     gg(data = pred, aes(fill = lambda$mean), geom = "tile") +
#     geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.5) +
#     # geom_sf(data = landslides, aes(col = Area_m2)) +
#     coord_sf() +
#     scale_fill_viridis_c()
#   p_pred
#   ggsave(here("figures", paste0(fit), "_pred.pdf"), width = tw, height = tw/2)
# }

# pxl <- fm_pixels(mesh_fm, dims = c(200, 200), mask = bnd)
#
# pred_pxl <- predict(fit,
#                     newdata = pxl,
#                     formula = ~ Intercept + pgv + landuse,
#                     n.samples = 100
# )


# if(to_plot){
#   ggplot() +
#     gg(pred_pxl, geom = "tile") +
#     geom_sf(data = landslides_c, col = "red")
#   ggsave(here("figures", "pred_pxl.pdf"), width = tw, height = tw/2)
# }

# pred_pxl <- predict(fit4,
#                     newdata = pxl,
#                     formula = fit4$bru_info$lhoods[[1]]$formula,
#                     n.samples = 100
# )
#
# ggplot() +
#   gg(pred_pxl["mean"], geom = "tile") + scale_fill_viridis_c() +
#   geom_sf(data = landslides_c, aes(col = log(Area_m2)), size = 0.1, alpha = 0.3)
# ggsave(here("figures","pred4_pxl.pdf"), width = tw, height = tw/2)


# Gaussian kernel ---------------------------------------------------------
# Modify from the R code in the link below
# https://stackoverflow.com/questions/22747916/how-to-generate-a-discrete-2d-gaussian-smoothing-kernel-using-r
# A function to pad zeros around a matrix of its original dimensions
padzeros2d <- function(mat, n0 = NULL) {
  padzeros <- function(mat, nzeros, side = "both") {
    if (is.vector(mat) == TRUE) mat <- t(as.matrix(mat))

    M.zeros <- matrix(0, nrow(mat), nzeros)

    newmat <- switch(side,
      both = cbind(M.zeros, mat, M.zeros),
      left = cbind(M.zeros, mat),
      right = cbind(mat, M.zeros)
    )
  }
  # how many rows/cols of zeroes are used to pad.
  width <- dim(mat)[1]
  if (is.null(n0)) {
  pad <- ceiling(width / 2)
  } else {
   pad <- n0
  }
  # TODO do.call

  x_pad <- t(padzeros(mat = mat, nzeros = pad, side = "both"))
  x_pad <- t(padzeros(mat = x_pad, nzeros = pad, side = "both"))
  return(x_pad)
}

# A function to convolve a matrix to give a Gaussian kernel
gkernel <- function(mat, norm = TRUE) {
  outkernel <- gsignal::conv2(padzeros2d(mat), mat, shape = "valid")
  if (norm == TRUE) {
    outkernel <- outkernel / sum(outkernel)
  }
  return(outkernel)
}

# kernel <- gsignal::conv2(padzeros2d(matrix(1, 2, 2)), matrix(1, 2, 2), shape = "valid")
# kernel2 <- gsignal::conv2(padzeros2d(kernel), kernel, shape = "valid")
# norm_kernel <- kernel / sum(abs(kernel))
# norm_kernel2 <- kernel2 / sum(abs(kernel2))


# https://stackoverflow.com/questions/65907022/given-a-spatial-hexagonal-grid-how-can-i-obtain-a-sample-of-higher-order-neigh
# https://cran.r-project.org/web/packages/geostan/vignettes/spatial-weights-matrix.html
#' ------------------
#' cv_partition
#' ------------------
#'
#' Partitions the region based on the given criteria for calculating residuals
#' in each partition. Parts of this function are taken from concepts in
#' https://rpubs.com/huanfaChen/grid_from_polygon
#'
#' Input:
#' @param samplers A sf object containing region for which
#' partitions need to be created
#' @param resolution resolution of the grids that are required
#' @param nrows number of rows of grids that are required
#' @param ncols number of columns of grids that are required
#' @param chess chessboard partitioning
#' Output:
#' @return a partitioned sf object as required or a list of partitioned sf objects if chess is TRUE
#'
#'
cv_partition <- function(samplers, resolution = NULL, nrows = NULL, ncols = NULL, 
                         chess = TRUE) {
  # Create a grid for the given boundary
  if (is.null(resolution)) {
    grid <- terra::rast(terra::ext(st_as_sf(fm_nonconvex_hull(samplers))),
                        crs = fm_crs(samplers)$input,
                        nrows = nrows, ncols = ncols
    )
  }
  
  if (is.null(c(nrows, ncols))) {
    grid <- terra::rast(terra::ext(st_as_sf(fm_nonconvex_hull(samplers))),
                        crs = fm_crs(samplers)$input,
                        resolution = resolution
    )
  }
  
  gridPolygon <- terra::as.polygons(grid)
  if(chess == TRUE){
    # no idea how it works
    # spatSample(x = gridPolygon, size = 0.5*nrow(gridPolygon), method="random", strata=NULL, chess="black")
    grid_chess <- init(grid, "chess")
    grid_chess_sf <- st_intersection(st_cast(st_as_sf(terra::as.polygons(grid_chess)), "POLYGON"), samplers)
    grid_chess_white_sf <- grid_chess_sf[grid_chess_sf$lyr.1==1,]
    grid_chess_black_sf <- grid_chess_sf[grid_chess_sf$lyr.1==0,]
    return(list(white=grid_chess_white_sf, black=grid_chess_black_sf))
    # ggplot() + gg(data = grid_chess_white_sf, aes(fill = lyr.1)) + geom_sf(data = nepal_bnd, col = "red", fill = "NA")
    # ggplot() + gg(data = grid_chess_black_sf, aes(fill = lyr.1)) + geom_sf(data = nepal_bnd, col = "red", fill = "NA")
  } else {
    # Extract the boundary with subpolygons only
    sf::st_as_sf(
      gridPolygon <- terra::intersect(gridPolygon, terra::vect(samplers))
    )
  }
}

 
# grid_chess_sf_ <- st_intersection(grid_chess_sf, nepal_bnd)
# 
# ggplot() + gg(data = grid_chess_sf_)

# values(grid) <- rep(c(rep(c(1, 0), length.out = ncol(grid)),
#               rep(c(0, 1), length.out = ncol(grid))),
#               times = nrow(grid)/2)
# 
# 
# 
# # Convert to polygons
# gridPolygon <- terra::as.polygons(grid)
# 
# # Select only "black" (or "white") cells
# gridPolygon_samp <- st_as_sf(gridPolygon[gridPolygon$lyr.1 == 1, ])  # Select every other cell
# gridPolygon <- st_as_sf(gridPolygon)
# 
# ggplot() + gg(data = gridPolygon) + gg(data = gridPolygon_samp)


# grid_vect <- as.points(grid)
# values(grid_vect) <- 1:nrow(grid_vect)
# grid$id <- cells(grid)
# grid <- terra::aggregate(grid, fact=2)



# gridPolygon <- terra::as.polygons(grid)
# 
# gridPolygon_samp <- spatSample(x = gridPolygon,
#                                size = floor(nrow(gridPolygon)),
#                                # strata = grid,
#                                method = "regular",
#                                chess="black"
#                                )


# gridPolygon <- terra::intersect(gridPolygon, terra::vect(nepal_bnd))
# https://damariszurell.github.io/EEC-MGC/b5_pseudoabsence.html

# gridPolygon_samp <- terra::as.polygons(gridPolygon_samp)

# gridPolygon_ <- st_as_sf(gridPolygon)
# gridPolygon_samp_ <- st_as_sf(gridPolygon_samp)
# 
# ggplot() + gg(data = gridPolygon_) + gg(data = gridPolygon_samp_)



# cv_chess -----------------------------------------------------------
# use tgt with cv_partition

cv_chess <- function(object,
                     mesh = NULL,
                     cv_grid = NULL,
                     # newdata = NULL,
                     formula = NULL,
                     n.samples = 100,
                     obs = "lambda",
                     inla.link = NULL,
                     seed = NULL,
                     plot = FALSE,
                     ...) {
  
  fml <- object$bru_info$lhoods[[1]]$formula
  ff <- deparse(fml[[3]], width.cutoff = 150L)
  # this one computes the sum across the entire domain
  # formula_ <- as.formula(paste0(" ~ sum(weight * exp(", ff, "))"))
  # TODO

  agg_nc <- bru_mapper_logsumexp(rescale = FALSE)
  formula_ <- as.formula(paste0(" ~ ibm_eval(agg_nc,
                                    input = list(
                                      block = .block,
                                      weights = weight
                                    ),
                                    state = ", ff, ", log = FALSE)"))
  cv_newdata <- fm_int(mesh, cv_grid$black)
  
  reorder <- order(cv_newdata$.block)
  cv_newdata <- cv_newdata[reorder, , drop  = FALSE]
  
  pred <- predict(object, newdata = cv_newdata,
                  formula = formula_, n.samples = n.samples)
  return(pred)
}

# st_make_grid ------------------------------------------------------------


if(FALSE){
  bnd_grid <- st_intersection(st_make_grid(nepal_bnd, cellsize = 50, square = TRUE), nepal_bnd)
  bnd_grid_sf <- st_as_sf(bnd_grid)
  bnd_grid_sf$id = 1:nrow(bnd_grid_sf)
  ggplot() + 
    geom_sf(data = bnd_grid_sf) + 
    geom_sf_text(aes(label = id)) +
    geom_sf(data = nepal_bnd, col = "red", fill = NA)
  
  
  bnd_grid_odd <- bnd_grid[c(TRUE,FALSE),]
  bnd_grid_even <- bnd_grid[!c(TRUE,FALSE),]
  
  bnd_grid_train <- bnd_grid[sample(1:length(bnd_grid), 0.5*length(bnd_grid)),]
  
  ggplot() + geom_sf(data = bnd_grid_odd, fill = "blue") + geom_sf(data = nepal_bnd, col = "red", fill = NA)
  ggplot() + geom_sf(data = bnd_grid_train, fill = "blue") + geom_sf(data = nepal_bnd, col = "red", fill = NA)
}




# for pred.R --------------------------------------------------------------


# helper function to make formula for prediction
fml_lambda <- function(fml) {
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
  as.formula(paste0(
    " ~ list(", "lambda = exp(", ff, ")",
    # ",", "log_lambda = ", ff,
    paste(",", tt1, "=", tt, collapse = ""), ")"
  ))
}

fml_mu <- function(fml) {
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
p_textbox <- function(text) {
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
    xlim(0, 1) +
    ylim(0, 1)
  p
}

# nice little plotting uncertainty trick
# if(FALSE){
  logit_inv <- function(x) {
    1 / (1 + exp(-abs(x)))
  }
  # gg(get(pred_lst_b[[i]])[[j]], geom = "tile",
  #    aes(fill = mean, alpha = logit_inv(sd/mean))) +
  
# }
  
  

# CRPS -------------------------------------------------------------------------
  # some large value, so that 1-F(K) is small
  crps <- function(object, newdata, obs, cv_grid, n.samples = 1000, seed = NULL,...) {
    
    if (length(seed) > 2) {
      stop("At most two seeds should be provided")
    }
    
    if (length(seed) >= 1) {
      set.seed(seed[1])
    }
    
    if (length(seed) >= 2) {
      seed2 <- seed[2]
    } else {
      seed2 <- as.integer(runif(1) * .Machine$integer.max) # this is how INLA set seed
    }
    
    if(is.null(cv_grid)){
      obs_data <- get(obs, newdata)
    } else{
      obs_data <- get(obs, cv_grid)
    }

    max_K <- ceiling(max(obs_data) + 4 * sqrt(max(obs_data)))
    k <- seq(0, max_K)
    kk <- rep(k, times = length(obs_data))
    i <- seq_along(obs_data)
    fml <- object$bru_info$lhoods[[1]]$formula
    ff <- deparse(fml[[3]], width.cutoff = 150L)
    
    if(is.null(cv_grid)){
        break()
      # formula <- as.formula(paste(
      #   paste0(" ~ { expect <-  ", ff),
      #   "\n",
      #   "variance <- 1/exp(Precision_for_the_Gaussian_observations) ",
      #   "\n",
      #   paste0(
      #     "pnorm(kk, mean = rep(expect, each = length(k)), sd = rep(sqrt(variance), each = length(k)))}"
      #   )
      # )
      # )
      # 
      # pred <- predict(object,
      #                 newdata = newdata, formula = formula,
      #                 n.samples = n.samples,
      #                 seed = seed2,
      #                 ...
      } else{

        # cv_newdata <- fm_int(newdata, cv_grid)
        # reorder <- order(cv_newdata$.block)
        # cv_newdata <- cv_newdata[reorder, , drop  = FALSE]
        
        agg_nc <- bru_mapper_logsumexp(rescale = FALSE)
        formula <- as.formula(paste(
          paste0(" ~ { expect <- ibm_eval(agg_nc, input = list(
                                        block = .block,
                                        weights = weight
                                      ),
                                      state = ", ff, ", log = FALSE)"),
          "\n",
          paste0(
            "ppois(kk, lambda = rep(expect, each = length(k)))}"
          )
        )
        )
        pred <- generate(object, newdata = newdata,
                         formula = formula,
                         n.samples = 1000,
                         seed = seed2,...
        )
      }
    
    results <- data.frame(
      i = rep(i, each = length(k)),
      k = kk,
      Fpred = rowMeans(pred, na.rm = TRUE),
      residuals = rowMeans(pred, na.rm = TRUE)- (rep(obs_data, each = length(k)) <= kk)
    )
    # Check that the cutoff point K has nearly probability mass 1 below it,
    # for all i:
    min(results %>% dplyr::filter(k == max_K) %>% pull(Fpred))
    
    crps_scores <-
      (results %>%
         group_by(i) %>%
         summarise(crps = sum(residuals^2), .groups = "drop") %>%
         pull(crps))
    return(crps_scores)
  }
  