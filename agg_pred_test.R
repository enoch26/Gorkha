library(INLA)
library(inlabru)
library(terra)
library(sf)
source("function.R")

cv_grid <- cv_partition(gorillas_sf$boundary,
                        resolution = c(.5,.5)
)

# Load the example data
nests <- gorillas_sf$nests
mesh <- gorillas_sf$mesh
boundary <- gorillas_sf$boundary
gcov <- gorillas_sf_gcov()

cv_grid$black$nests <- lengths(st_intersects(cv_grid$black, gorillas_sf$nests))

comp1 <- geometry ~ vegetation(gcov$vegetation, model = "factor_full") - 1

fit1 <- lgcp(comp1, nests, samplers = boundary, domain = list(geometry = mesh))

agg_nc <- bm_logsumexp(rescale = FALSE)
agg_nc <- bru_mapper_logsumexp(rescale = FALSE)
ff <- "vegetation"
formula <- as.formula(paste(
  paste0(" ~ { expect <- ibm_eval(agg_nc, input = list(
                                        block = .block,
                                        weights = weight
                                      ),
                                      state = ", ff, ", log = FALSE)}")
  # "\n",
  # paste0(
  #   "list(expect = expect, obs_prob = dpois(", get("nests", cv_grid$black), ", lambda = expect))}"
  # )
)
)

cv_newdata <- fm_int(mesh, samplers = st_geometry(cv_grid$black))
int1 <- predict(fit1, cv_newdata, formula = formula, n.samples = 5)
