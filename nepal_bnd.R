if(is.null(CV_thin) & is.null(CV_chess)){
  CV_thin <- FALSE
  CV_chess <- FALSE
}

nepal_nc %<-% {
  st_read(here(
    "data", "Local Unit",
    "local_unit.shp"
  )) %>%
    mutate(Province = stringr::str_replace(Province, "1", "Koshi")) %>%
    mutate(Province = stringr::str_replace(Province, "2", "Madhesh")) %>%
    mutate(Province = stringr::str_replace(Province, "5", "Lumbini")) %>%
    st_transform(crs = 32645) %>% # to UTM
    filter(Province %in% c("Koshi", "Bagmati", "Gandaki"))
}
crs_nepal <- fm_crs_set_lengthunit(fm_crs(nepal_nc), "km")
# set crs unit to km
nepal_nc <- nepal_nc %>% st_transform(crs = crs_nepal)

# data boundary box
# Valagussa and others (2021)
bbox_coords <- c(
  84.24265675600003, 27.383653885000058,
  86.69613706100006, 28.736201582000035
)
names(bbox_coords) <- c("xmin", "ymin", "xmax", "ymax")
landslides_bbox <- st_bbox(bbox_coords, crs = 4326) %>%
  st_transform(crs = crs_nepal)
# st_bbox(bbox_coords, crs = 4326) %>% st_as_sfc() %>% st_area()
# 36208604820 [m^2]

# Zhang and others (2016)
# https://www.sciencebase.gov/catalog/item/imap/5874a764e4b0a829a320bb3a
# bbox_coords_zhang <- c(
#   83.56403067945462, 27.451422357973854, 86.3577832466, 28.4712991961
# )
# names(bbox_coords_zhang) <- c("xmin", "ymin", "xmax", "ymax")
# landslides_bbox_zhang <- st_bbox(bbox_coords_zhang, crs = 4326) %>%
#   st_transform(crs = crs_nepal)
# st_bbox(bbox_coords_zhang, crs = 4326) %>% st_as_sfc() %>% st_area()
# 31118672300 [m^2]

# bnd should be the bbox if ignoring the administrative region
# TODO
nepal_bnd %<-% {
  st_cast(st_sf(data.frame(geometry = st_union(nepal_nc))), "POLYGON")
}

# remove the unrelated geological region
# if(FALSE){
geology %<-% {
  st_read(here("data", "nepal_geo_fill_.shp")) %>%
    st_transform(crs = crs_nepal)
  # st_intersection(bnd_out)
}

geology_ <- geology[(geology$ROCK_TYPES %in% c("Siwalik Group", "Gangetic Plain", "Sub Himalaya")), ]
nepal_bnd <- st_cast(
  st_difference(nepal_bnd, st_union(geology_$geometry)),
  "POLYGON"
)

# plot(nepal_bnd)
# plot(st_geometry(st_union(geology_)), add = TRUE)
# }


bnd <- st_as_sf(st_difference(
  st_as_sfc(landslides_bbox),
  st_difference(st_as_sfc(landslides_bbox), nepal_bnd)
))



# clean up the boudnary mismatch bit
bnd <- bnd[1, ]

bnd_out <- st_cast(
  st_sf(geometry = fm_nonconvex_hull(bnd, 2.5)),
  "POLYGON"
)

# within_idx  <- st_intersects(nepal_nc, st_as_sfc(landslides_bbox))
# nepal_nc <- nepal_nc[which(lengths(within_idx) != 0), ]
# bnd %<-% {
#   st_cast(st_sf(data.frame(geometry = st_union(nepal_nc))), "POLYGON")
# }

# another nepal shp
# nepal_muni %<-% {
#   st_read(here("data", "Municipality", "Municipality.shp")) %>%
#     st_transform(crs = crs_nepal) %>%
#     st_intersects(st_as_sfc(landslides_bbox))
# }
# bnd_muni %<-% {
#   st_cast(st_sf(data.frame(geometry = st_union(nepal_muni))), "POLYGON")
# }

# remove crs for covariate
# st_crs(nepal_nc) <- NA

# landslides data ---------------------------------------------------------


landslides %<-% {
  st_read(
    here(
      "data", "Valagussaandoth", "Valagussa_2021", "Valagussa_2021.shp"
    )
  ) %>%
    st_transform(crs = crs_nepal) %>% # to UTM
    st_intersection(bnd_out)
}

landslides_c <- st_centroid(landslides) %>% st_intersection(bnd)

# ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels_near.tif")) %>%
ksn_tag <- rast(here("data", "ksn_tag_near.tif")) %>%
  # ksn_tag <- rast(here("data", "lsdtt", fdr, "cop30dem_channel_tagged_pixels.bil")) %>%
  project(crs_nepal$input, threads = TRUE) %>%
  crop(bnd_out, mask = TRUE) %>%
  clamp(1, values = TRUE)
log_ksn_tag <- log(ksn_tag$cop30dem_channel_tagged_pixels)
sqrt_ksn_tag <- sqrt(ksn_tag$cop30dem_channel_tagged_pixels)
# rm(ksn_tag)

landslides_c$log_ksn <- extract(log_ksn_tag, vect(st_geometry(landslides_c)), ID = FALSE)

if(FALSE){
  landslides_c$log_ksn <- unlist(extract(log_ksn_tag, vect(st_geometry(landslides_c)), ID = FALSE))
  landslides_c$fd2ch <- unlist(extract(fd2ch$fd2ch_km, vect(st_geometry(landslides_c)), ID = FALSE))
  landslides_c$rf2ch <- unlist(extract(rf2ch$rf2ch_km, vect(st_geometry(landslides_c)), ID = FALSE))
  landslides_c <- st_intersection(landslides_c, landcover["CODE1"])
  landslides_c <- st_intersection(landslides_c, geology["ROCK_TYPES"])
  landslides_c$CODE1 <- factor(landslides_c$CODE1)
  
  library(viridis)
  # ggplot(landslides_c, aes(x=rf2ch, y=Area_m2)) + geom_point(aes(col=log_ksn), size =0.0001, alpha = 0.7) +
  #    scale_color_viridis(values = sc, option = "D") + facet_wrap(~ CODE1, scales="free_y")
  # ggsave("landslides_ksn_area.pdf", width = tw, height = tw)
  
  pwr <- .5
  sc <- scales::rescale(seq(0,1,length.out = 50)^pwr)
  col_opt <- "H"
  landslides_c_ <- landslides_c[!is.na(landslides_c$CODE1),]
  

  
  ggplot(landslides_c_, aes(x=log_ksn, y=logarea_m2)) + geom_point(aes(col=rf2ch), size =0.0001, alpha = 0.7) +
     scale_color_viridis(values = sc, option = col_opt) + facet_wrap(~ CODE1, scales="free_y")
  ggsave("landslides_ksn_logarea_fdcol_landcover.pdf", width = tw, height = tw)
  
  ggplot(landslides_c_, aes(x=fd2ch, y=logarea_m2)) + geom_point(aes(col=log_ksn), size =0.0001, alpha = 0.7) +
    scale_color_viridis(values = sc, option = col_opt) + facet_wrap(~ CODE1, scales="free_y")
  ggsave("landslides_ksn_logarea_fd_landcover.pdf", width = tw, height = tw)
  
  ggplot(landslides_c_, aes(x=rf2ch, y=logarea_m2)) + geom_point(aes(col=log_ksn), size =0.0001, alpha = 0.7) +
    scale_color_viridis(values = sc, option = col_opt) + facet_wrap(~ CODE1, scales="free_y")
  ggsave("landslides_ksn_logarea_rf_landcover.pdf", width = tw, height = tw)
  
  ggplot(landslides_c_, aes(x=fd2ch, y=logarea_m2)) + geom_point(aes(col=log_ksn), size =0.0001, alpha = 0.7) +
     scale_color_viridis(option = "D") + facet_wrap(~ ROCK_TYPES, scales="free_y")
  ggsave("landslides_ksn_logarea_fd_geology.pdf", width = tw, height = tw)
  

  
  ggplot(landslides_c_, aes(x=rf2ch, y=logarea_m2)) + geom_point(aes(col=log_ksn), size =0.0001, alpha = 0.7) +
     scale_color_viridis(option = "D") + facet_wrap(~ ROCK_TYPES, scales="free_y")
  ggsave("landslides_ksn_logarea_rf_geology.pdf", width = tw, height = tw)

  ggplot(landslides_c_, aes(x=fd2ch, y=logarea_m2)) + 
    geom_point(aes(col=CODE1, shape=CODE1), size =0.0001, alpha = 0.5) +
    scale_shape_manual(values=1:nlevels(landslides_c_$CODE1))
    # theme_bw()
  ggsave("landslides_ksn_LC.pdf")
  
}

# TODO find crown pt with flow direction
if (glacier_remove) {
  landslides_c <- (landslides_c[landslides_c$log_ksn >= 1.99, ])
  landslides_c_glacier <- (landslides_c[landslides_c$log_ksn < 1.99, ])
  # nrow(landslides_c_glacier)
  # 85
}


# train and test data split -----------------------------------------------

# TODO CV_thin: half the landslides pts, model the same, predict the same, compute the score with newdata$count
# TODO CV_chess: cv_partition the domain, model with test landslides pts data over the entire domain, predict the

# Swap the train and test set into thinA and thinB subfolders
if (CV_thin) {
  if (file.exists(here("data", trainset, "landslides_thin_train.shp"))) {
    landslides_c <- st_read(here("data", trainset, "landslides_thin_train.shp"))
    landslides_c_test <- st_read(here("data", trainset, "landslides_thin_test.shp"))
  } else {
    test_idx <- sample(1:nrow(landslides_c), 0.5 * nrow(landslides_c))
    landslides_c_test <- landslides_c[test_idx, ]
    landslides_c <- landslides_c[-test_idx, ]
    rownames(landslides_c) <- rownames(landslides_c_test) <- NULL
  st_write(landslides_c, here("data", trainset, "landslides_thin_train.shp"))
  st_write(landslides_c_test, here("data", trainset, "landslides_thin_test.shp"))
  }
    

} else if (CV_chess) {
  # https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html
  # get.chessboard
  # https://rdrr.io/cran/terra/src/R/sample.R
  cv_grid <- cv_partition(bnd,
    resolution = cv_chess_resol
  )
if(to_plot){
    ggplot() + gg(data = cv_grid[[train]], fill = train) + gg(data = cv_grid[[test]], fill = test) +
    geom_sf(data = bnd, col = "red", fill = "NA")
    ggsave(here("figures", paste0("cv_chess_",train, ".pdf")))
  
}
  # ggplot() +  + geom_sf(data = nepal_bnd, col = "red", fill = "NA")

  if (file.exists(here("data", paste0("landslides_chess", nm_chess ,"_train.shp")))) {
    landslides_c <- st_read(here("data",paste0("landslides_chess", nm_chess ,"_train.shp")))
    landslides_c_test <- st_read(here("data", paste0("landslides_chess", nm_chess, "_test.shp")))
    
    cv_grid[[train]]$count <- lengths(st_intersects(cv_grid[[train]], landslides_c))
    cv_grid[[test]]$count_test <- lengths(st_intersects(cv_grid[[test]], landslides_c_test))
  } else {
    landslides_c_test <- st_intersection(landslides_c, cv_grid[[test]])
    # pxl_black <- st_intersection(pxl, cv_grid[[test]])

    # ggplot() + geom_sf(data = pxl_black, size = .01)
    # ggsave("figures/pxl_black.pdf")
    landslides_c <- st_intersection(landslides_c, cv_grid[[train]])
  
    cv_grid[[train]]$count <- lengths(st_intersects(cv_grid[[train]], landslides_c))
    cv_grid[[test]]$count_test <- lengths(st_intersects(cv_grid[[test]], landslides_c_test))
    
  st_write(landslides_c, here("data", paste0("landslides_chess", nm_chess ,"_train.shp")))
  st_write(landslides_c_test, here("data", paste0("landslides_chess", nm_chess, "_test.shp")))
  }
} else {print("no thin no chess CV")}

landslides_c$logarea_m2 <- log(landslides_c$Area_m2)

if (CV_thin|CV_chess){
  landslides_c_test$logarea_m2 <- log(landslides_c_test$Area_m2)
}
