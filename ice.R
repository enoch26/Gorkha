
  
  landslides_c_ <- st_intersection(landslides_c, landcover)
  landslides_c_ <- landslides_c_[!is.na(landslides_c_$CODE1),]
  df <- data.frame(
    Area_m2 = landslides_c_$Area_m2,
    logarea_m2 = landslides_c_$logarea_m2, landcover = landslides_c_$CODE1
  )
  ggplot(df, aes(x = logarea_m2, fill = landcover)) +
    geom_histogram() +
    labs(y="landslides count", x = expression(paste("log area (", m^{2},")"))) +
    guides(fill=guide_legend(ncol=2))
  # ggsave("figures/landcover_log_hist.png", width = tw, height = tw / 2)
  ggsave("figures/landcover_log_hist.png", width = tw/2, height = tw / 5)
  
  df_ <- df %>% mutate(land_cover = substr(landcover, 0, 1))
  df_$land_cover <- factor(df_$land_cover, 
                           levels = c("1", "2", "6", "8"),
                           labels = c("1: Cultivated Crops", 
                                      "2: Natural Vegetation",
                                      "6: Rock/Gravel/Stones/Boulders",
                                      "8: Ice/Snow/Water bodies"))
  
  # levels(factor(df_$land_cover)) <- c("Cultivated Crops", 
  #                                     "Natural Vegetation",
  #                                     "Rock/Gravel/Stones/Boulders",
  #                                     "Ice/Snow/Water bodies")
  if (FALSE) {
  ggplot(df_, aes(x = logarea_m2, fill = land_cover)) +
    geom_histogram() + 
    labs(y="landslides count", x = expression(paste("log area (", m^{2},")"))) +
    guides(fill = "none") + 
    facet_wrap(vars(land_cover), scales = "free_y", ncol = 1)
  ggsave("figures/landcover_log_hist_fw.png", width = tw/4, height = tw / 3)
  
  ggplot(df_, aes(x = logarea_m2, fill = land_cover)) +
    stat_ecdf() + 
    labs(y="ecdf", x = expression(paste("log area (", m^{2},")"))) +
    guides(fill = "none") + 
    facet_wrap(vars(land_cover))
  ggsave("figures/landcover_log_ecdf_fw.png", width = tw/2, height = tw / 3)
  
  ggplot(df %>% filter(landcover %in% c("8SN", "8ICE", "8SNs", "8ICEr")), aes(x = logarea_m2, fill = landcover)) +
    geom_histogram() + labs(y="landslides count", x = expression(paste("log area (", m^{2},")"))) +
  ggsave("figures/landcover_ice_hist.png", width = tw, height = tw / 2)
  ggsave("figures/landcover_ice_hist.png", width = tw/2, height = tw / 4)
}

ggplot() + 
  geom_sf(data = landcover %>% filter(CODE1 %in% c("8SN", "8ICE", "8SNs", "8ICEr")), fill = "white") +
  geom_sf(data = landslides_c_test, aes(col = logarea_m2), size = 0.01) +
  geom_sf(data = landslides_c, aes(col = logarea_m2), size = 0.01) +
  geom_sf(data = bnd, col = "red", fill = NA) +
  scale_fill_viridis_c() 


ggsave(paste0("figures/ice.pdf"), width = tw, height = tw / 3)