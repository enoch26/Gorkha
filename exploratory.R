# https://tmieno2.github.io/R-as-GIS-for-Economists/int-RV.html
# histogram/density of the slope and landslides size and counts 
dem_landslides <- terra::extract(dem$dem_km, vect(landslides_c), ID = FALSE)
terrain_landslides <- terra::extract(dem_terrain_mask, vect(landslides_c), ID = FALSE)
pga_landslides <- terra::extract(pga_mean_raster, vect(landslides_c), ID = FALSE)
focal_landslides <- terra::extract(dem_terrain_focal2$relief, vect(landslides_c), ID = FALSE)
geo_landslides <- eval_spatial(data = nepal_geo, where = landslides_c$geometry , 
                               layer = "ROCK_TYPES")

geo_landslides <- st_intersection(landslides_c, nepal_geo)
landcover_landslides <- st_intersection(landslides_c, landcover)

p_geo <- ggplot(geo_landslides, aes(x = ROCK_TYPES)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p_landcover <- ggplot(landcover_landslides, aes(x = CODE1)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p_geo + p_landcover
ggsave("figures/landslides_geo_landcover.pdf", width = tw, height = tw)

landslides_df <- landslides_c %>% st_drop_geometry() %>% 
  bind_cols(list(dem_landslides, focal_landslides, terrain_landslides, 
                 pga_landslides, geo_landslides$ROCK_TYPES, landcover_landslides$CODE1))

landslides_samp <- landslides_df[sample(nrow(landslides_df), 1000), ]
ggpairs(landslides_samp[c(2:6,10)])
ggsave("figures/landslides_pairs.pdf", width = tw, height = tw)

ggpairs(landslides_samp[c(2:6,10)],
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet"))
ggsave("figures/landslides_pairs2.pdf", width = tw, height = tw)

ggplot(landslides_df, aes(x=slope)) + 
  geom_histogram(binwidth=0.05)
ggsave("figures/landslides_slope_hist.pdf", width = tw, height = tw)
ggplot(landslides_df, aes(x=relief)) + 
  geom_histogram(binwidth=0.05)
ggsave("figures/landslides_relief_hist.pdf", width = tw, height = tw)


