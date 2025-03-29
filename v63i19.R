################################################### 
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("INLA")
library("lattice")
set.seed(12345L)
inla.qsample(n = 1, Matrix(1, 1, 1), seed = 12345L)


################################################### 
my.levelplot <- function(proj, values, col.regions = grey.colors(64, 1, 0), aspect = "fill", 
  contour = TRUE, labels = FALSE, xlim = range(proj$x), ylim = range(proj$y), ...) {
  z <- inla.mesh.project(proj, values)
  print(levelplot(row.values = proj$x, column.values = proj$y, x = z, xlim = xlim, 
    ylim = ylim, col.regions = col.regions, aspect = aspect, contour = contour, 
    labels = labels, ...))
}
lattice.to.plot <- function(latt, projection, skip = 10) {
  loc <- inla.mesh.map(latt$loc, projection = projection, inverse = FALSE)
  mat1 <- matrix(loc[, 1], nrow = latt$dims[1], ncol = latt$dims[2])
  mat2 <- matrix(loc[, 2], nrow = latt$dims[1], ncol = latt$dims[2])
  hskip <- seq(1, latt$dims[1], skip)
  vskip <- seq(1, latt$dims[2], skip)
  return(rbind(cbind(as.vector(rbind(cbind(mat1[, vskip], NA), NA)), as.vector(rbind(cbind(mat2[, 
    vskip], NA), NA))), cbind(as.vector(rbind(cbind(t(mat1[hskip, ]), NA), NA)), 
    as.vector(rbind(cbind(t(mat2[hskip, ]), NA), NA)))))
}


################################################### 
mesh1d.basis.plot <- function(mesh, n, idx = 1:mesh$m, w = NULL, add = FALSE, ...) {
  x <- seq(mesh$interval[1], mesh$interval[2], length = n)
  A <- inla.mesh.1d.A(mesh, x)
  if (is.null(w)) {
    if (!add) 
      plot(x = 0, type = "n", xlim = mesh$interval, ylim = range(A), ...)
    for (k in idx) lines(x, A[, k])
  } else {
    f <- A %*% w
    if (!add) 
      plot(x = 0, type = "n", xlim = mesh$interval, ylim = range(f), ...)
    lines(x, f)
  }
}


################################################### 
m <- 50
points <- matrix(runif(m * 2), m, 2)
mesh <- inla.mesh.2d(loc = points, cutoff = 0.05, offset = c(0.1, 0.4), max.edge = c(0.05, 
  0.5))


################################################### 
plot(mesh, main = "")
points(points)


################################################### 
bnd <- inla.nonconvex.hull(points, convex = 0.12)


################################################### 
lines(bnd, add = FALSE)
points(points)


################################################### 
mesh <- inla.mesh.2d(boundary = bnd, cutoff = 0.05, max.edge = c(0.1))


################################################### 
plot(mesh, main = "")


################################################### 
mesh <- inla.mesh.2d(boundary = bnd, cutoff = 0.05, offset = c(1, 0.5), max.edge = c(0.1, 
  0.5))


################################################### 
plot(mesh, main = "")


################################################### 
mesh2 <- inla.mesh.create(globe = 10)


################################################### 
A <- inla.spde.make.A(mesh, loc = points)


################################################### 
spde <- inla.spde2.matern(mesh, alpha = 2)


################################################### 
sigma0 <- 1
size <- min(c(diff(range(mesh$loc[, 1])), diff(range(mesh$loc[, 2]))))
range0 <- size/5
kappa0 <- sqrt(8)/range0
tau0 <- 1/(sqrt(4 * pi) * kappa0 * sigma0)
spde <- inla.spde2.matern(mesh, B.tau = cbind(log(tau0), -1, +1), B.kappa = cbind(log(kappa0), 
  0, -1), theta.prior.mean = c(0, 0), theta.prior.prec = c(0.1, 1))


################################################### 
truevar <- (3 * sigma0)^2
truerange <- range0


################################################### 
Q <- inla.spde.precision(spde, theta = c(log(3), 0))


################################################### 
x <- inla.qsample(n = 2, Q, seed = 123L)


################################################### 
x <- inla.qsample(n = 2, Q, constr = spde$f$extraconstr)


################################################### 
plot(mesh)
plot(mesh, rgl = TRUE)
lines(mesh$segm$bnd, mesh$loc, add = FALSE)


################################################### 
plot(mesh, rgl = TRUE, col = x[, 1], color.palette = function(n) grey.colors(n, 1, 
  0), draw.edges = FALSE, draw.segments = TRUE, draw.vertices = FALSE)


################################################### 
proj <- inla.mesh.projector(mesh, dims = c(100, 100))
image(proj$x, proj$y, inla.mesh.project(proj, field = x[, 1]))


################################################### 
mesh2 <- inla.mesh.create(globe = 10)
proj2a <- inla.mesh.projector(mesh2, projection = "longlat", dims = c(361, 181))
proj2b <- inla.mesh.projector(mesh2, projection = "mollweide", dims = c(361, 181))


################################################### 
spde2 <- inla.spde2.matern(mesh2)
Q2 <- inla.spde2.precision(spde2, theta = c(0, -1))
x2 <- inla.qsample(n = 1, Q2, seed = 1234L)[, 1]


################################################### 
my.levelplot(proj, x[, 1], at = pretty(x, 16), aspect = "iso", xlim = c(0, 1), ylim = c(0, 
  1), xlab = "", ylab = "", main = "")


################################################### 
my.levelplot(proj, x[, 2], at = pretty(x, 16), aspect = "iso", xlim = c(0, 1), ylim = c(0, 
  1), xlab = "", ylab = "", main = "")


################################################### 
latt <- inla.mesh.lattice(x = seq(-180, 180, 1), y = seq(-90, 90, 1) * (1 - 1e-08), 
  units = "longlat")


################################################### 
print(contourplot(x = matrix(c(0, 0, 0, 0), 2, 2), row.values = c(0, 1), column.values = c(0, 
  1), xlim = c(-180, 180), ylim = c(-90, 90), aspect = 1/2, xlab = "Longitude", 
  ylab = "Latitude"))
trellis.focus("panel", 1, 1, highlight = FALSE)
print(llines(lattice.to.plot(latt, "longlat"), col = 1))
trellis.unfocus()


################################################### 
print(contourplot(x = matrix(c(0, 0, 0, 0), 2, 2), row.values = c(0, 1), column.values = c(0, 
  1), xlim = c(-180, 180), ylim = c(-1, 1), aspect = 1/2, xlab = "Longitude", ylab = ""))
trellis.focus("panel", 1, 1, highlight = FALSE)
print(llines(lattice.to.plot(latt, "longsinlat"), col = 1))
trellis.unfocus()


################################################### 
print(contourplot(x = matrix(c(0, 0, 0, 0), 2, 2), row.values = c(0, 1), column.values = c(0, 
  1), xlim = c(-2, 2), ylim = c(-1, 1), aspect = 1/2, xlab = "", ylab = ""))
trellis.focus("panel", 1, 1, highlight = FALSE)
print(llines(lattice.to.plot(latt, "mollweide"), col = 1))
trellis.unfocus()


################################################### 
my.levelplot(proj2a, x2, at = pretty(x2, 16), aspect = 1/2, xlab = "Longitude", ylab = "Latitude", 
  main = "Lon-Lat projection")
trellis.focus("panel", 1, 1, highlight = FALSE)
print(llines(lattice.to.plot(latt, "longlat", 30), col = 1))
trellis.unfocus()


################################################### 
my.levelplot(proj2b, x2, at = pretty(x2, 16), aspect = 1/2, xlab = "", ylab = "", 
  main = "Mollweide projection")
trellis.focus("panel", 1, 1, highlight = FALSE)
print(llines(lattice.to.plot(latt, "mollweide", 30), col = 1))
trellis.unfocus()


################################################### 
A <- inla.spde.make.A(mesh, loc = points, index = rep(1:m, times = 2), repl = rep(1:2, 
  each = m))


################################################### 
x <- as.vector(x)
covariate <- rnorm(m * 2)
y <- 5 + covariate * 2 + as.vector(A %*% x) + rnorm(m * 2) * 0.1


################################################### 
mesh.index <- inla.spde.make.index(name = "field", n.spde = spde$n.spde, n.repl = 2)


################################################### 
st.est <- inla.stack(data = list(y = y), A = list(A, 1), effects = list(c(mesh.index, 
  list(intercept = 1)), list(cov = covariate)), tag = "est")


################################################### 
st.pred <- inla.stack(data = list(y = NA), A = list(1), effects = list(c(mesh.index, 
  list(intercept = 1))), tag = "pred")


################################################### 
stack <- inla.stack(st.est, st.pred)


################################################### 
formula <- y ~ -1 + intercept + cov + f(field, model = spde, replicate = field.repl)
inla.result <- inla(formula, data = inla.stack.data(stack, spde = spde), family = "normal", 
  control.predictor = list(A = inla.stack.A(stack), compute = TRUE))


################################################### 
result <- inla.spde2.result(inla.result, "field", spde)
plot(result[["marginals.range.nominal"]][[1]], type = "l", main = "Nominal range, posterior density")


################################################### 
plot(result[["marginals.variance.nominal"]][[1]], type = "l", main = "Nominal variance, posterior density")


################################################### 
index <- inla.stack.index(stack, "pred")$data
linpred.mean <- inla.result[["summary.linear.predictor"]]$mean
linpred.sd <- inla.result[["summary.linear.predictor"]]$sd
image(proj$x, proj$y, inla.mesh.project(proj, linpred.mean[index[mesh.index$field.repl == 
  1]]))
image(proj$x, proj$y, inla.mesh.project(proj, linpred.sd[index[mesh.index$field.repl == 
  1]]))


###################################################
## Data -vs- linear model estimate
index.est <- inla.stack.index(stack, "est")$data
plot(inla.result$summary.fitted.values$mean[index.est], y)


################################################### 
linpred.mean <- inla.result[["summary.linear.predictor"]]$mean
xplot <- linpred.mean[index[mesh.index$field.repl == 1]]
my.levelplot(proj, xplot, at = pretty(xplot, 16), aspect = "iso", xlab = "", ylab = "", 
  main = "", xlim = c(0, 1), ylim = c(0, 1))
trellis.focus("panel", 1, 1, highlight = FALSE)
print(lpoints(points, col = 1))
trellis.unfocus()


################################################### 
linpred.sd <- inla.result[["summary.linear.predictor"]]$sd
xplot <- linpred.sd[index[mesh.index$field.repl == 1]]
my.levelplot(proj, xplot, at = pretty(xplot, 10), aspect = "iso", xlab = "", ylab = "", 
  main = "", xlim = c(0, 1), ylim = c(0, 1))
trellis.focus("panel", 1, 1, highlight = FALSE)
print(lpoints(points, col = 1))
trellis.unfocus()


################################################### 
data("Tokyo")
knots <- seq(1, 367, length = 25)
mesh <- inla.mesh.1d(knots, interval = c(1, 367), degree = 2, boundary = "cyclic")


################################################### 
sigma0 <- 1
kappa0 <- 0.001
tau0 <- 1/(4 * kappa0^3 * sigma0^2)^0.5


################################################### 
spde <- inla.spde2.matern(mesh, constr = FALSE, B.tau = cbind(log(tau0), 1), B.kappa = cbind(log(kappa0), 
  0), theta.prior.prec = 1e-04)


################################################### 
A <- inla.spde.make.A(mesh, loc = Tokyo$time)
time.index <- inla.spde.make.index("time", n.spde = spde$n.spde)
stack <- inla.stack(data = list(y = Tokyo$y, link = 1, Ntrials = Tokyo$n), A = list(A), 
  effects = list(time.index), tag = "est")
formula <- y ~ -1 + f(time, model = spde)
data <- inla.stack.data(stack)
result <- inla(formula, family = "binomial", data = data, Ntrials = data$Ntrials, 
  control.predictor = list(A = inla.stack.A(stack), link = data$link, compute = TRUE))


################################################### 
time <- 1:366
index <- inla.stack.index(stack, "est")$data
plot(Tokyo$time, Tokyo$y/Tokyo$n, xlab = "Day", ylab = "Probability")
lines(time, result$summary.fitted.values$mean[index])
lines(time, result$summary.fitted.values$"0.025quant"[index], lty = 2)
lines(time, result$summary.fitted.values$"0.975quant"[index], lty = 2)


###################################################
## Residuals for cumulative counts:
plot(time, cumsum(Tokyo$y/Tokyo$n) - cumsum(result$summary.fitted.values$mean[index])) 
