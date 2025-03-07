library(INLA)
library(inlabru)
library(ggplot2)
library(fmesher)

data(Poisson2_1D)
cd <- countdata2
x <- seq(-10, 65, by = 1) # this sets mesh points - try others if you like
mesh1D <- fm_mesh_1d(x, boundary = "free")
the_spde <- inla.spde2.pcmatern(mesh1D,
                                prior.range = c(1, 0.01),
                                prior.sigma = c(1, 0.01)
)

comp <- ~ field(x, model = the_spde) + Intercept(1, prec.linear = 1 / 2^2)

fit2.bru <- bru(
  comp,
  like(count ~ Intercept + field,
          data = cd,
          family = "poisson",
          E = exposure
  )
)

summary(fit2.bru)
xs <- seq(0, 55, length = 100)
exposures <- rep(cd$exposure[1], 100)
x4pred <- data.frame(x = xs)
ff <- fit2.bru$bru_info$lhoods[[1]]$formula
tt <- labels(terms(ff))
as.formula(paste0(" ~ list(","lambda_ = exp(",deparse(ff, , width.cutoff = 150L),")",paste(",", tt, "=", tt, collapse = ""),")"))

for (i in 1:length(tt)) {
  print(paste(tt[i], "=", tt[i], sep = ","))
}

pred2.bru <- predict(fit2.bru,
                     x4pred,
                     as.formula(paste0(" ~ list(","lambda_ = exp(",deparse(ff, , width.cutoff = 150L),")",paste(",", tt, "=", tt, collapse = ""),")")),
                     n.samples = 100
)

ff <- as.formula("~list(lambda=exp(field+Intercept))")

ff <- as.formula(paste0("~list(lambda_=exp(",
                        deparse(fit2.bru$bru_info$lhoods[[1]]$formula[[3]]),
                        "))"))
