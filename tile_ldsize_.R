library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(ggspatial)
library(maptiles)
library(units)

CV_thin <- FALSE
CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
source("read_data.R")

#-------------------------------
# Preparation
#-------------------------------

target_crs <- st_crs(landslides_c)

bnd <- st_transform(bnd, target_crs)
bnd_out <- st_transform(bnd_out, target_crs)
landslides <- st_transform(landslides, target_crs)
landslides_c <- st_transform(landslides_c, target_crs)
landcover <- st_transform(landcover, target_crs)
epic_sf <- st_transform(st_as_sf(epic)[1, ], target_crs)

#-------------------------------
# Histogram for land cover
#-------------------------------

landslides_c_ <- st_intersection(landslides_c, landcover)
landslides_c_ <- landslides_c_[!is.na(landslides_c_$CODE1), ]

df <- data.frame(
  Area_m2 = landslides_c_$Area_m2,
  logarea_m2 = log10(landslides_c_$Area_m2),
  landcover = landslides_c_$CODE1
)

ggplot(df, aes(x = logarea_m2, fill = landcover)) +
  geom_histogram() +
  labs(
    y = "landslides count",
    x = expression(paste(log[10], " area (", m^{2}, ")"))
  ) +
  guides(fill = guide_legend(ncol = 2))

ggsave("figures/landcover_log_hist.png", width = tw/2, height = tw/5)

#-------------------------------
# Slight zoom-out extent
#-------------------------------

bb <- st_bbox(bnd)

# pad_x <- 15   # km
# pad_y <- 10   # km
pad_x <- 10   # km
pad_y <- 5   # km

bb_exp <- bb
bb_exp["xmin"] <- bb["xmin"] - pad_x
bb_exp["xmax"] <- bb["xmax"] + pad_x
bb_exp["ymin"] <- bb["ymin"] - pad_y
bb_exp["ymax"] <- bb["ymax"] + pad_y

bb_exp_poly <- st_as_sfc(bb_exp)
bb_exp_poly <- st_set_crs(bb_exp_poly, target_crs)


tile_margin_x <- 3
tile_margin_y <- 2

bb_tile <- bb_exp
bb_tile["xmin"] <- bb_exp["xmin"] + tile_margin_x
bb_tile["xmax"] <- bb_exp["xmax"] - tile_margin_x
bb_tile["ymin"] <- bb_exp["ymin"] + tile_margin_y
bb_tile["ymax"] <- bb_exp["ymax"] - tile_margin_y

bb_tile_poly <- st_as_sfc(bb_tile)
bb_tile_poly <- st_set_crs(bb_tile_poly, target_crs)

#-------------------------------
# Basemap
#-------------------------------

tile <- maptiles::get_tiles(
  bb_tile_poly,
  provider = "Esri.WorldImagery",
  crop = TRUE,
  zoom = 12
)

# landslides inside main study boundary
landslides_ <- st_intersection(landslides, bnd)


#-------------------------------
# ICIMOD / HKH fault lines
#-------------------------------

faults <- st_read("data/Fault Lines of Hindu Kush Himalayan (HKH) Region/data/fault.shp")
faults <- st_transform(faults_, target_crs)
# faults <- st_crop(faults, bb_exp)
faults_bb <- st_crop(faults, bb_tile_poly)
faults_tf <- faults_bb %>%
  dplyr::filter(TYPE == "thrust-fault")

#-------------------------------
# HimaTibetMap structures
#-------------------------------

if(FALSE){
  faults <- st_read(
    "data/HimaTibetMap-master/HimaTibetMap-master/arc/HimaTibetMap.shp",
    quiet = TRUE
  )
  faults <- st_transform(faults, target_crs)
  
  faults_bb <- st_crop(faults, bb_exp)
  
  # keep only faults and thrusts
  faults_ft <- faults_bb %>%
    filter(Type %in% c("normal", "thrust")) %>%
    mutate(struct_type = ifelse(Type == "thrust", "Thrust", "Fault"))
}


#-------------------------------
# PGA contours
#-------------------------------

pga_crop <- crop(pga_mean_raster$pga_mean_exp, vect(bb_exp_poly), mask = TRUE)


#-------------------------------
# teeth
#-------------------------------
make_thrust_teeth_by_feature <- function(lines, spacing = 6, offset = 0.4, size = 0.5,
                                         flip_ids = integer()) {
  teeth <- list()
  
  for (i in seq_len(nrow(lines))) {
    geom_i <- st_geometry(lines[i, ])[[1]]
    if (!inherits(geom_i, "LINESTRING")) next
    
    line_sfc <- st_sfc(geom_i, crs = st_crs(lines))
    line_len <- as.numeric(st_length(line_sfc))
    if (line_len < spacing * 2) next
    
    dists <- seq(spacing, line_len - spacing, by = spacing)
    
    # choose side per feature
    use_flip <- i %in% flip_ids
    
    for (d in dists) {
      p  <- st_line_sample(line_sfc, sample = d / line_len) %>% st_cast("POINT")
      p0 <- st_line_sample(line_sfc, sample = max(d - 0.5, 0) / line_len) %>% st_cast("POINT")
      p1 <- st_line_sample(line_sfc, sample = min(d + 0.5, line_len) / line_len) %>% st_cast("POINT")
      
      xy  <- st_coordinates(p)[1, ]
      xy0 <- st_coordinates(p0)[1, ]
      xy1 <- st_coordinates(p1)[1, ]
      
      dx <- xy1["X"] - xy0["X"]
      dy <- xy1["Y"] - xy0["Y"]
      L  <- sqrt(dx^2 + dy^2)
      if (L == 0) next
      
      tx <- dx / L
      ty <- dy / L
      
      # default side
      nx <- -ty
      ny <- tx
      
      # flip selected feature(s)
      if (use_flip) {
        nx <- -nx
        ny <- -ny
      }
      
      # shift triangle off the line
      cx <- xy["X"] + offset * nx
      cy <- xy["Y"] + offset * ny
      
      # tip points outward normal to line
      tip   <- c(cx + size * nx, cy + size * ny)
      base1 <- c(cx - 0.4 * size * nx + 0.6 * size * tx,
                 cy - 0.4 * size * ny + 0.6 * size * ty)
      base2 <- c(cx - 0.4 * size * nx - 0.6 * size * tx,
                 cy - 0.4 * size * ny - 0.6 * size * ty)
      
      poly_coords <- rbind(tip, base1, base2, tip)
      teeth[[length(teeth) + 1]] <- st_polygon(list(poly_coords))
    }
  }
  
  st_sf(geometry = st_sfc(teeth, crs = st_crs(lines)))
}

teeth_sf <- make_thrust_teeth_by_feature(
  faults_tf,
  spacing = 6,
  offset = 0.4,
  size = 0.5,
  flip_ids = c(3:5)
)

ggplot() +
  geom_sf(data = faults_tf, colour = "red3", linewidth = 0.5) +
  geom_sf(data = teeth_sf, fill = "red3", colour = "red3", linewidth = 0.2) +
  theme_bw()
ggsave("sth.pdf")


#-------------------------------
# white box
#-------------------------------

x1 <- bb_exp["xmin"] + 0.63 * (bb_exp["xmax"] - bb_exp["xmin"])
x2 <- bb_exp["xmin"] + 0.998 * (bb_exp["xmax"] - bb_exp["xmin"])
y1 <- bb_exp["ymin"] + 0.62 * (bb_exp["ymax"] - bb_exp["ymin"])
y2 <- bb_exp["ymin"] + 0.998 * (bb_exp["ymax"] - bb_exp["ymin"])

# legend item positions
lx1 <- x1 + 0.020 * (bb_exp["xmax"] - bb_exp["xmin"])
lx2 <- x1 + 0.090 * (bb_exp["xmax"] - bb_exp["xmin"])
tx  <- x1 + 0.095 * (bb_exp["xmax"] - bb_exp["xmin"])

ly_thrust <- y2 - 0.16 * (bb_exp["ymax"] - bb_exp["ymin"])
ly_ls     <- y2 - 0.24 * (bb_exp["ymax"] - bb_exp["ymin"])
ly_epic   <- y2 - 0.32 * (bb_exp["ymax"] - bb_exp["ymin"])

epx <- lx1 + 0.030 * (bb_exp["xmax"] - bb_exp["xmin"])

# landslide legend box size
ls_dx <- 0.010 * (bb_exp["xmax"] - bb_exp["xmin"])
ls_dy <- 0.010 * (bb_exp["ymax"] - bb_exp["ymin"])

#-------------------------------
# Plot
#-------------------------------

p1 <- ggplot() +
  geom_spatraster_rgb(data = tile) +
  guides(alpha = "none") +
  # thrust-fault line
  geom_sf(
    data = faults_tf,
    colour = "black",
    linewidth = 0.4,
    show.legend = FALSE
  ) +
  # thrust teeth
  geom_sf(
    data = teeth_sf,
    fill = "black",
    colour = "black",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  gg(data = bnd, col = "red", fill = "transparent", lwd = 0.5) +
  geom_sf(
    data = landslides_,
    colour = "red",
    fill = "grey70",
    linewidth = 0.2,
    alpha = 0.8
  ) +
  geom_spatraster_contour_text(
    data = pga_crop,
    breaks = seq(0.1, 0.9, 0.15),
    color = "white"
  ) +
  geom_sf(
    data = epic_sf,
    col = "yellow",
    fill = NA,
    shape = 2,
    size = 3
  ) +
  
  annotate(
    "rect",
    xmin = x1, xmax = x2,
    ymin = y1, ymax = y2,
    fill = scales::alpha("white", 0.96),
    colour = NA
  ) +
  # thrust-fault legend
  annotate(
    "segment",
    x = lx1, xend = lx2,
    y = ly_thrust, yend = ly_thrust,
    colour = "black",
    linewidth = 1.1
  ) +
  annotate(
    "point",
    x = (lx1 + lx2) / 2, y = ly_thrust,
    shape = 24,
    size = 2.8,
    fill = "black",
    colour = "black",
    stroke = 0.8
  ) +
  annotate(
    "text",
    x = tx, y = ly_thrust,
    label = "Thrust fault",
    hjust = 0,
    size = 3.3,
    colour = "black"
  ) +
  # landslide polygon legend
  annotate(
    "rect",
    xmin = epx - ls_dx,
    xmax = epx + ls_dx,
    ymin = ly_ls - ls_dy,
    ymax = ly_ls + ls_dy,
    fill = "grey70",
    colour = "red",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = tx, y = ly_ls,
    label = "Landslide polygon",
    hjust = 0,
    size = 3.3,
    colour = "black"
  ) +
  # epicentre legend
  annotate(
    "point",
    x = epx, y = ly_epic,
    shape = 2,
    size = 4,
    colour = "yellow",
    stroke = 1.2
  ) +
  annotate(
    "text",
    x = tx, y = ly_epic,
    label = "Epicentre",
    hjust = 0,
    size = 3.3,
    colour = "black"
  ) +
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.18, "in"),
    text_col = "black",
    line_col = "black"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0, "in"),
    pad_y = unit(0.5, "in"),
    height = unit(0.5, "in"),
    width  = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_col = "black",
      line_col = "black",
      fill = c("white", "black")
    )
  ) +
  coord_sf(
    xlim = c(bb_exp["xmin"], bb_exp["xmax"]),
    ylim = c(bb_exp["ymin"], bb_exp["ymax"]),
    expand = FALSE
  )

ggsave(
  "figures/tile_ldsize_pga_exp_mw78.jpg",
  plot = p1, width = tw/2.3, height = tw/4,
  type = "cairo", dpi = 300
)



# p2 ----------------------------------------------------------------------
p2 <- ggplot() +
  geom_spatraster_rgb(data = tile) +
  guides(alpha = "none") +
  # thrust-fault line
  geom_sf(
    data = faults_tf,
    colour = "black",
    linewidth = 0.4,
    show.legend = FALSE
  ) +
  # thrust teeth
  geom_sf(
    data = teeth_sf,
    fill = "black",
    colour = "black",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  gg(data = bnd, col = "red", fill = "transparent", lwd = 0.5) +
  geom_sf(
    data = landslides_,
    colour = "red",
    fill = "grey70",
    linewidth = 0.2,
    alpha = 0.8
  ) +
  geom_spatraster_contour_text(
    data = pga_mean_raster_mw73$pga_mean_exp %>% crop(vect(bb_exp_poly), mask = TRUE),
    breaks = seq(0.15, 0.55, 0.1),
    color = "white"
  ) +
  geom_sf(
    data = st_as_sf(epic)[2, ] |> st_transform(target_crs),
    col = "yellow",
    fill = NA,
    shape = 2,
    size = 3
  ) +
  annotate(
    "rect",
    xmin = x1, xmax = x2,
    ymin = y1, ymax = y2,
    fill = scales::alpha("white", 0.96),
    colour = NA
  ) +
  # thrust-fault legend
  annotate(
    "segment",
    x = lx1, xend = lx2,
    y = ly_thrust, yend = ly_thrust,
    colour = "black",
    linewidth = 1.1
  ) +
  annotate(
    "point",
    x = (lx1 + lx2) / 2, y = ly_thrust,
    shape = 24,
    size = 2.8,
    fill = "black",
    colour = "black",
    stroke = 0.8
  ) +
  annotate(
    "text",
    x = tx, y = ly_thrust,
    label = "Thrust fault",
    hjust = 0,
    size = 3.3,
    colour = "black"
  ) +
  # landslide polygon legend
  annotate(
    "rect",
    xmin = epx - ls_dx,
    xmax = epx + ls_dx,
    ymin = ly_ls - ls_dy,
    ymax = ly_ls + ls_dy,
    fill = "grey70",
    colour = "red",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = tx, y = ly_ls,
    label = "Landslide polygon",
    hjust = 0,
    size = 3.3,
    colour = "black"
  ) +
  # epicentre legend
  annotate(
    "point",
    x = epx, y = ly_epic,
    shape = 2,
    size = 4,
    colour = "yellow",
    stroke = 1.2
  ) +
  annotate(
    "text",
    x = tx, y = ly_epic,
    label = "Epicentre",
    hjust = 0,
    size = 3.3,
    colour = "black"
  ) +
  ggspatial::annotation_scale(
    location = "tr",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.18, "in"),
    text_col = "black",
    line_col = "black"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0, "in"),
    pad_y = unit(0.5, "in"),
    height = unit(0.5, "in"),
    width  = unit(0.5, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_col = "black",
      line_col = "black",
      fill = c("white", "black")
    )
  ) +
  coord_sf(
    xlim = c(bb_exp["xmin"], bb_exp["xmax"]),
    ylim = c(bb_exp["ymin"], bb_exp["ymax"]),
    expand = FALSE
  )


ggsave(
  "figures/tile_ldsize_pga_exp_mw73.jpg",
  plot = p2, width = tw/2.3, height = tw/4,
  type = "cairo", dpi = 300
)
