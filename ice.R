if (FALSE) {
  
  landslides_c_ <- st_intersection(landslides_c, landcover)
  landslides_c_ <- landslides_c_[!is.na(landslides_c_$CODE1),]
  df <- data.frame(
    Area_m2 = landslides_c_$Area_m2,
    logarea_m2 = landslides_c_$logarea_m2, landcover = landslides_c_$CODE1
  )
  ggplot(df, aes(x = logarea_m2, fill = landcover)) +
    geom_histogram() +
    labs(y="landslides count", x = "log area (m2)") +
    guides(fill=guide_legend(ncol=2))
  # ggsave("figures/landcover_log_hist.png", width = tw, height = tw / 2)
  ggsave("figures/landcover_log_hist.png", width = tw/2, height = tw / 5)
  ggplot(df %>% filter(landcover %in% c("8SN", "8ICE", "8SNs", "8ICEr")), aes(x = logarea_m2, fill = landcover)) +
    geom_histogram()
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