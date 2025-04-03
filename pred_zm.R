# run after source("pred.R")

basin <- rast(here("data", "lsdtt", fdr, "cop30dem_AllBasins.bil")) %>%
  project(crs_nepal$input)

basin_zm <- basin %>% filter(cop30dem_AllBasins == 14982)
basin_zm_sf <- as_sf(as.polygons(basin_zm))
landslides_poly_zm <- st_intersection(landslides, st_as_sfc(basin_zm_sf))

# TODO contour with ksn?
fp1a_zm <- fp1a$lambda["mean"] %>% st_intersection(basin_zm_sf)

ggplot() + 
  gg(data = fp1a_zm, aes(fill = mean), geom = "tile") +
  scale_fill_viridis_c() +
  gg(data = landslides_poly_zm, col="red", fill = "red", aes(alpha = log(Area_m2))) +
  geom_sf(data = basin_zm_sf, fill = NA, col = "red") 
ggsave(here("figures", "model","fp1a_zm.pdf"))
