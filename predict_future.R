library(inlabru)
library(INLA)
library(mgcv)
library(ggplot2)
library(fmesher)
data(Poisson2_1D, package = "inlabru")
x <- seq(0, 55, length.out = 50)
mesh1D <- fm_mesh_1d(x, boundary = "free")
matern <- inla.spde2.pcmatern(mesh1D,
                              prior.range = c(150, 0.75),
                              prior.sigma = c(0.1, 0.75),
                              constr = TRUE
)
comp <- ~ spde1D(x, model = matern) + Intercept(1)
fit.spde <- bru(
  comp,
  bru_obs(x ~ spde1D + Intercept, family = "cp", data = pts2, domain = list(x = mesh1D))
)
## Equivalent call for this particular example:
# fit.spde <- lgcp(
#   comp,
#   formula = x ~ ., data = pts2, domain = list(x = mesh1D)
# )

# Set up a data frame of explanatory values at which to predict
predf <- data.frame(x = seq(0, 55, by = 1))
pred_spde <- predict(fit.spde,
                     predf,
                     ~ exp(spde1D + Intercept),
                     n.samples = 1000
)


  ff <- fit.spde$bru_info$lhoods[[1]]$formula
  tt <- labels(terms(ff))
  pred_spde_ <- predict(fit.spde,
          newdata = predf,
          formula = ~ list(lambda_ = exp(spde1D + Intercept), spde1D = spde1D),
          n.samples = 10)
  mod_names_a <- c("fit.spde")
  
  predict_lst_a <- future.apply::future_lapply(1:length(mod_names_a), function(i) {
    ff <- get(mod_names_a[i])$bru_info$lhoods[[1]]$formula
    tt <- labels(terms(ff))
    predict(get(mod_names_a[i]),
            newdata = predf,
            formula = as.formula(paste0(" ~ list(","lambda_ = exp(",deparse(ff[[3]], width.cutoff = 150L),")",paste(",", tt, "=", tt, collapse = ""),")")),
            n.samples = 10)
  })
  
  