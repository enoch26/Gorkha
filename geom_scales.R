# https://stackoverflow.com/questions/66893928/when-using-a-color-transformation-in-ggplot2-change-the-legend-gradient-instead
# Aim: was try to figure why the colours scale got binned

library(ggplot2)
library(scales)
library(sf)
library(terra)
#> Warning: package 'scales' was built under R version 4.0.3

set.seed(20)
l <- 5
d = expand.grid(x = seq(0, 5, len = 5), y = seq(0, 5, len = 5))
d = transform(d, z = 1e-2*((x - 2)^2 + (2*y - 4)^2 + 10*rnorm(nrow(d)))^2)

d_sf <- st_as_sf(d, coords = c("x", "y")) 
d_terra <- rast(d)
d_grid <- st_as_sf(as.polygons(d_terra))
d_grid$z <- d_sf$z[1:12]

pal <- gradient_n_pal(brewer_pal(palette = "Spectral", direction = -1)(7))
sc <- seq(0,1,length.out = 20)
# the higher the power, the more the mass is concentrated in the middle
pwr <- 3
sfg <- scale_fill_gradientn(
  colours = pal(scales::rescale(seq_along(d_grid$z))), # <- extra 0, 1 for out-of-bounds
  # limits = c(0, 12), breaks = 0:12,
  values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5)))

sfg2 <- scale_fill_gradient2(
  low = "blue",
  mid = "white",
  high = "red",
  limits = range(d_grid$z),
  midpoint = diff(range(d_grid$z))/2
  # values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5))
)

sfgn <- scale_fill_gradientn(
  # colours = c("blue", "white", "red"),
  colours = colorspace::diverge_hcl(7),
  limits = range(d_grid$z),
  # midpoint = diff(range(d_grid$z))/2
  values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5))
)

sfd <- scale_fill_distiller(
  type = "div",
  palette = "RdBu",
  limits = range(d_grid$z),
  values = scales::rescale(abs(2*sc-1)^pwr*sign(sc-0.5))
)
  
ggplot() +
  geom_sf(data = d_grid, aes(fill = z))  + sfd
ggplot() +
  geom_sf(data = d_grid, aes(fill = z))  + sfgn

+
  # geom_raster(aes(x, y, fill = z)) +
sfd
