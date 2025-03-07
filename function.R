# Covariate deterministic function ----------------------------------------


cov_fun <- function(x, y) {
  (x^2 - y^2) * exp(-.5 * x^2 - .5 * y^2)
  # ((2 * x)^2 + 2 * y) * exp(-.5 * x^2 - .5 * y^2)
}


# Nonlinear transformation function ---------------------------------------


exp_nl <- function(x) (exp(a * x) - c_) / b # -1 to sthelse 20240627



# invert scale ------------------------------------------------------------

inv_scale <- function(x, xmin, xmax, xmin_new, xmax_new) {
  x_new <- (x - xmin) / (xmax - xmin) * (xmax_new - xmin_new) + xmin_new
  return(x_new)
}


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

# score -------------------------------------------------------------------
# TODO extend to list of formulae, too much for a function, probably give up
# TODO for a list of bru objects with the same formula to compare the SE and DS score
# A function to compute the SE and DS score for bru object(s)
# obs: the response name in the newdata to compare with the prediction
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
dse_score <- function(object,
                      newdata = NULL,
                      formula = NULL,
                      n.samples = 100,
                      obs = "lambda",
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
  if (is.null(formula)) {
    # TODO auto detect matching (maybe) object$bru_info$lhoods[[1]]$inla.family
    # TODO user to provide .block in newdata
    formula <- object$bru_info$lhoods[[1]]$formula
    formula <- as.formula(paste0(
      "~ inla.link.", inla.link,
      "(", labels(terms(formula)), ", inverse = TRUE)"
    ))
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
  pred <- predict(object,
    newdata = newdata, formula = formula,
    n.samples = n.samples,
    seed = seed2,
    ...
  )
  post_E <- get("mean", pred)
  post_Var <- (get("sd", pred))^2
  # TODO this only applies to Poisson lambda/ Gaussian mean, count needa add post_E to the variance, other cases need to do case by case, forget it.
  pred$SE_score <- SE_score <- (get(obs, newdata) - post_E)^2
  pred$DS_score <- DS_score <- SE_score / post_Var + log(post_Var)
  SE_score_mean <- mean(SE_score, na.rm = TRUE)
  DS_score_mean <- mean(DS_score, na.rm = TRUE)
  cat(
    "Mean SE score: ", SE_score_mean, "\n",
    "Mean DS score: ", DS_score_mean, "\n"
  )
  # An example of how to plot the SE and DS score
  if (plot) {
    p1 <- ggplot() +
      gg(data = pred["SE_score"], geom = "tile") +
      ggtitle("Squared Error Score")
    p2 <- ggplot() +
      gg(data = pred["DS_score"], geom = "tile") +
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
      SE_score_mean = SE_score_mean,
      DS_score_mean = DS_score_mean
    ),
    score = tibble::tibble(SE_score = SE_score, DS_score = DS_score)
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
