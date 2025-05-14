# Data --------------------------------------------------------------------
# Nepal shp
# https://download.hermes.com.np/nepal-administrative-boundary-wgs/

# Nepal - National Boundary
# Province Boundary
# District Boundary
# Local Level Boundary
# The administrative boundaries gets updated sometimes so you may as well check Survey Department Geo-portal Site for the updated maps
# River Network-Line
# Major River Network-Polygon
# Settlement Names
# Settlement Names as published by department of Survey
# Ward Map
# https://sites.google.com/view/maze215/Maps/Nepal-Shp
# https://nationalgeoportal.gov.np/#/dataset
# landslides
## Gnyawali and others (2016)
# https://www.sciencebase.gov/catalog/item/5874a7cee4b0a829a320bb3e
# https://data.usgs.gov/datacatalog/data/USGS:5874a7cee4b0a829a320bb3e
# https://www.sciencebase.gov/catalog/item/614512b3d34e0df5fb95b5f9
# https://www.sciencebase.gov/catalog/item/59cd0585e4b00fa06fefe80a
## Valagussa and others (2021)
# https://www.sciencebase.gov/catalog/item/61f040e1d34e8b818adc3251
# boundary box
# https://www.sciencebase.gov/catalog/item/imap/61f040e1d34e8b818adc3251

# landcover
# error in downloading
# [DirectoryNotFoundException: Could not find a part of the path 'E:\RDS_Main\BulkDownloads\Nepal_NLCMS\nlcms_2015.zip'.]
# https://rds.icimod.org/Home/DataDetail?metadataId=1972729
# https://observablehq.com/@categorise/icimod-land-cover-of-nepal

# Land Cover of Himalaya Region
# FAO (2021) The Himalaya Regional Land Cover Database: Lancover in Nepal, Bhutan, Afghanistan, Pakistan, India, China and Myanmar. FAO.
# hima_lc_npl
# https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/46d3c2ef-72c3-4f96-8e32-40723cd1847b
# https://storage.googleapis.com/fao-maps-catalog-data/geonetwork/landcover/hima_lc_npl.zip

# geology
# https://www.sciencebase.gov/catalog/item/60c3b89fd34e86b93897ef19
# considering buy a proper geology map
# https://dmgnepal.gov.np/en/resources/province-and-regional-geological-maps-6665
# even ESCAP map back in 1993 is better than the current one
# https://repository.unescap.org/handle/20.500.12870/4866
# Geological data of Nepal's Gorkha Earthquake 2015 affected area
# https://rds.icimod.org/Home/DataDetail?metadataId=24676&searchlist=True


# DEM
# https://opentopography.org/news/updated-copernicus-30m-DEM-available
# https://github.com/mlampros/CopernicusDEM
# https://doi.org/10.5069/G9028PQB
# Elevation Zones of Nepal
# https://rds.icimod.org/Home/DataDetail?metadataId=834&searchlist=True

# waterbody
# Water Bodies 2014-2020 (raster 300 m), global, 10-daily – version 1 - Geotiff (NUTS: NP)
# raster on 24 April 2015, Nepal earthquake that took place on 25 April 2015
# https://land.copernicus.eu/en/products/water-bodies/water-bodies-global-v1-0-300m
# Nepal Watercourses - Rivers
# Time Period of the Dataset [?]
# January 01, 2001-January 01, 2001
# https://data.humdata.org/dataset/nepal-watercourses-rivers
# MERIT Hydro: Global Hydrography Datasets
# https://hydro.iis.u-tokyo.ac.jp/~yamadai/MERIT_Hydro/
# https://water.usgs.gov/catalog/datasets/05199160-2947-404d-bac7-ba6aed53a96e/

# Internal Relief Map of Nepal's Gorkha Earthquake 2015 affected area


# Soil water index
# Soil Water Index 2007-present (raster 12.5 km), global, 10-daily – version 3 - Netcdf (NUTS: NP)
# Soil Water Index 2007-present (raster 12.5 km), global, daily – version 3 - Netcdf (NUTS: NP)


# try out this dataset with bigger coverage
# TODO https://www.sciencebase.gov/catalog/item/582c74fbe4b04d580bd377e8

# cubic vs lanczos
# https://r.geocompx.org/geometry-operations


# set global var ----------------------------------------------------------

if(CV_chess){
  if (train == "white") {
    test <- "black"
  }
  if(train == "black") {
    test <- "white"
  }
  
}

to_plot <- TRUE
ncore <- 10
x_bin <- 500 # TODO increase to 1000
edge_len_n <- 2
seed <- c(1234, 1234)
set.seed(1234)
tw <- 15.55528
fdr <- "lanczos_"
JU <- UP <- FALSE
x_pxl <- 500
glacier_remove <- FALSE

# CV ---------------------------------------------------------------------


if (CV_chess) {
  # cv_chess_resol <- c(5,5)
  # > nrow(landslides_c)
  # [1] 10390
  # > nrow(landslides_c_test)
  # [1] 10081
  nm_chess <- paste0("_", train, "_", cv_chess_resol[1])
} else {
  nm_chess <- ""
}

# Library -----------------------------------------------------------------


library(dplyr)
library(stars)
library(sf)
library(fmesher)
library(INLA)
library(inlabru)
library(here)
library(ggplot2)
library(terra)
library(tidyterra)
library(future)

# fasterRaster::fillNA vs terra::interpNear
# https://r-packages.io/packages/fasterRaster/fillNAs

# Set the number of threads for parallel processing
gdalraster::set_config_option("GDAL_NUM_THREADS", "10")
# setGDALconfig("GDAL_NUM_THREADS=10")
plan(multicore, workers = ncore)
options(future.globals.maxSize = 7864320000)
options(future.rng.onMisuse = "ignore")
inla.setOption(num.threads = "10:1")
source(here("function.R"))

# nepal boundary and landslides bbox  -------------------------------------

source(here("nepal_bnd.R"))

landcover %<-% {
  st_read(
    here(
      "data",
      "hima_lc_npl",
      "lc_nepal.shp"
    )
  ) %>%
    st_transform(crs = crs_nepal) %>% # to UTM
    st_intersection(bnd_out)
}

# landcover without landslides
landcover$landslides_count <- lengths(st_intersects(landcover %>% select(CODE1), landslides_c))

landcover_df <- as.data.frame(landcover) %>%
  group_by(CODE1) %>%
  summarise(
    total_count = sum(landslides_count),
    .groups = "drop"
  )
# CODE1 with landslides
landcover_ref <- landcover_df %>%
  filter(total_count != 0) %>%
  pull(CODE1)

landcover$CODE1_ref <- ifelse(landcover$CODE1 %in% landcover_ref,
  0, 1
)

landcover$CODE1 <- as.factor(landcover$CODE1)

# 8WP is problematic, so we replace it with NA, and NA will be replace with bru_fill_missing with nearest
landcover <- landcover_ <- landcover %>%
  mutate(CODE1 = stringr::str_replace(CODE1, "8WP", NA_character_))

# after bru fill missing. see landcover_fill.R
# landcover %<-% {
#   st_read(
#     here(
#       "data",
#       "landcover_.shp"
#     )
#   ) %>%
#     st_transform(crs = crs_nepal) %>% # to UTM
#     st_intersection(bnd_out)
# }

# hydrology ---------------------------------------------------------------
# TODO barrier model?
# https://land.copernicus.eu/en/technical-library/product-user-manual-10-daily-land-surface-temperature-v1.0/@@download/file
if (FALSE) {
  wb %<-% {
    rast(
      here(
        "data", "146856", "Results",
        "Water Bodies 2014-2020 (raster 300 m), global, 10-daily – version 1",
        "c_gls_WB300_201504210000_GLOBE_PROBAV_V1.0.1_NP_WB.nc"
      )
    ) %>%
      project(crs_nepal$input) %>%
      crop(bnd_out, mask = TRUE)
  }

  # ggplot() + geom_spatraster(data = wb) + geom_sf(data = bnd_out, fill = NA, col = "red")
  # ggsave("figures/wb.png", width = tw, height = tw/2)

  if (file.exists(here("data", "water.shp"))) {
    water <- st_read(here("data", "water.shp"))
  } else {
    water %<-% {
      st_read(
        here(
          "data",
          "npl-watcrsa-hydro-25k-50k-sdn-wgs84-shp",
          "npl_watcrsa_hydro_25K_50K_sdn_wgs84.shp"
        )
      ) %>%
        st_make_valid() %>%
        st_transform(crs = crs_nepal) %>% # to UTM
        st_intersection(bnd_out)
    }
    st_write(water, here("data", "water.shp"), overwrite = TRUE)
    ggplot() +
      geom_sf(data = water) +
      geom_sf(data = bnd_out, fill = NA, col = "red")
    ggsave("figures/water.png", width = tw, height = tw / 2)
  }
}




# water_crop <- st_crop(water, bbox_coords)

# ggplot() + geom_sf(data = water, aes(fill = FCODE)) +
#   geom_sf(data = bnd, fill = NA, col = "red") +
#   geom_sf(data = bnd_out, fill = NA, col = "red")
# ggsave("figures/water.png", width = tw, height = tw/2)

# landcover$CODE1 <- relevel(as.factor(landcover$CODE1),
#                          ref = "1HSs") # TODO oringially 2TCOne//2TCObe

# data format and source
# https://cbworden.github.io/shakemap/manual4_0/ug_products.html
# The units (except for MMI, which is expressed in linear units) are the natural logarithm of the physical units: for acceleration that is ln(g) and for velocity the units are ln(cm/s).
pga_mean_raster <-
  rast(here(
    "data", "raster", "pga_mean.flt"
  )) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)


# After looking into the variance, it is not sure if it is systematic or local error
# otherwise Cmatrix generic0 to incorporate uncertainty would work
# pga_std_raster <-
#   rast(here(
#     "data", "raster", "pga_std.flt"
#   )) %>%
#   project(crs_nepal$input) %>%
#   crop(bnd_out, mask = TRUE)
ggplot() + geom_spatraster(data = pga_std_raster) +
  geom_sf(data = bnd, fill = NA, col = "red") +
  scale_fill_viridis_c(option = "D") +
  ggtitle("PGA std")
ggsave("figures/pga_std.png", width = tw, height = tw / 2)

# revert back to original scale
pga_mean_raster$pga_mean_exp <- exp(pga_mean_raster$pga_mean)
# pga_std_raster$pga_std_exp <- exp(pga_std_raster$pga_std)

# https://www.nature.com/articles/s43247-024-01822-9
# The 2015 Nepal earthquake generated peak ground accelerations (PGA) ranging from 0.1 g to 0.5 g

# DEM related ---------------------------------------------------------------------
# https://stackoverflow.com/questions/76209910/replace-nas-by-numeric-values-in-rasterstack-raster-or-multi-layer-spatraster


# CHANNEL
rf2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_RELIEFTOCHAN.bil")) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)
rf2ch$rf2ch_km <- values(rf2ch) / 1000

fd2ch <- rast(here("data", "lsdtt", fdr, "cop30dem_FDTOCHAN.bil")) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)
fd2ch$fd2ch_km <- values(fd2ch) / 1000

# FAR RIDGE
rf2fr <- rast(here("data", "lsdtt", fdr, "cop30dem_RELIEFTOFARRIDGE.bil")) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)
rf2fr$rf2fr_km <- values(rf2fr) / 1000

fd2fr <- rast(here("data", "lsdtt", fdr, "cop30dem_FDTOFARRIDGE.bil")) %>%
  project(crs_nepal$input) %>%
  crop(bnd_out, mask = TRUE)
fd2fr$fd2fr_km <- values(fd2fr) / 1000

if (FALSE) {
  twi <- rast(here("data", "lsdtt", fdr, "cop30dem_TWI.bil")) %>%
    project(crs_nepal$input) %>%
    crop(bnd_out, mask = TRUE) %>%
    clamp(1, values = TRUE)


  names(twi) <- "twi"

  twi$logtwi <- log(twi$twi)
}




## flow direction ----------------------------------------------------------
if (file.exists(here("data", "cop30dem.tif"))) {
  dem <- rast(here("data", "cop30dem.tif")) %>%
    project(crs_nepal$input) %>%
    crop(bnd_out, mask = TRUE)
  dem$dem_km <- dem$output_hh / 1000
} else {
  dem <- rast(here("data", "output_hh.tif")) %>%
    project(crs_nepal$input) %>%
    # crop(st_as_sfc(landslides_bbox), mask = TRUE)
    crop(bnd_out, mask = TRUE)
  # dem <- project(dem, crs_nepal$input)
  # dem <- dem %>% crop(bnd_out, mask = TRUE)
  dem$dem_km <- dem$output_hh / 1000

  writeRaster(dem, here("data", "cop30dem.tif"), overwrite = TRUE)
}
if (FALSE) {
  dem <- rast(here("data", "lsdtt", "lanczos_", "cop30dem.bil")) %>%
    project(crs_nepal$input) %>%
    crop(bnd_out, mask = TRUE)
  flowdir <- terrain(dem, "flowdir")
  weight <- cellSize(dem, unit = "km")
  flowacc_weight <- flowAccumulation(flowdir, weight)
  flowacc <- flowAccumulation(flowdir)
  flowacc$logflowdir <- log(flowacc)
  ggplot() +
    geom_spatraster(data = flowacc["logflowdir"]) +
    scale_fill_viridis_c(na.value = "transparent")
  ggsave("flowacc.pdf",
    width = tw,
    height = tw / 2
  )

  crit <- c("slope", "aspect", "roughness", "flowdir")
  # , "TPI", "TRIriley", "TRIrmsd")

  if (file.exists(here("data", "dem_terrain_mask.tif"))) {
    dem_terrain_mask <- rast(here("data", "dem_terrain_mask.tif")) %>%
      project(crs_nepal$input) %>%
      crop(bnd_out, mask = TRUE)
  } else {
    dem_terrain_mask <-
      terrain(dem$dem_km,
        v = crit, unit = "radians",
        neighbors = 8
      )
    # https://rspatial.org/pkg/5-methods.html
    rclmat <- seq(0, 2 * pi, length.out = 37)
    dem_terrain_mask$asp_gp <-
      classify(dem_terrain_mask$aspect, rclmat, include.lowest = TRUE)

    writeRaster(dem_terrain_mask, here("data", "dem_terrain_mask.tif"))
  }



  # even kernel with larger radius
  # norm_kernel2 <- gkernel(norm_kernel2, norm = TRUE)}

  if (file.exists(here("data", "dem_terrain_focal.tif")) &&
    file.exists(here("data", "dem_terrain_focal2.tif"))) {
    dem_terrain_focal <- rast(here("data", "dem_terrain_focal.tif")) %>%
      project(crs_nepal$input) %>%
      crop(bnd_out, mask = TRUE)
    dem_terrain_focal2 <- rast(here("data", "dem_terrain_focal2.tif")) %>%
      project(crs_nepal$input) %>%
      crop(bnd_out, mask = TRUE)
  } else {
    dem_terrain_mask_ <-
      terrain(dem$dem_km,
        v = "slope", unit = "radians",
        neighbors = 4
      )

    # TODO I reckon the grad easting and northing should be swapped and without negative sign
    dem_terrain_mask_$grad_easting <-
      -sin(dem_terrain_mask$aspect) * tan(dem_terrain_mask$slope)

    dem_terrain_mask_$grad_northing <-
      -cos(dem_terrain_mask$aspect) * tan(dem_terrain_mask$slope)

    # here the kernel is a weight matrix, eg 1,2,4,2,1 convolution matrix, discrete roughly circular
    norm_kernel2 <- norm_kernel <- gkernel(matrix(1, 2, 2), norm = TRUE) # radius 3*30 / 2 = 45 m
    stack_times <- 10 # TODO should be larger than mesh size, or do subdivide mesh into 9

    for (i in 1:(stack_times / 2)) {
      norm_kernel <- gkernel(norm_kernel, norm = TRUE)
    }
    # radius # col of matrix *30/2 =75 m
    for (i in 1:stack_times) {
      norm_kernel2 <- gkernel(norm_kernel2, norm = TRUE)
    }

    dem_terrain_focal <- focal(dem_terrain_mask_,
      w = norm_kernel, fun = sum
    )
    dem_terrain_focal2 <- focal(dem_terrain_mask_,
      w = norm_kernel2, fun = sum
    )

    # dem_terrain_focal$relief <- sqrt(dem_terrain_mask$grad_easting^2 + dem_terrain_mask$grad_northing^2) I did use mask during the meeting but is it mask or focal?
    dem_terrain_focal$relief <- sqrt(dem_terrain_focal$grad_easting^2 + dem_terrain_focal$grad_northing^2)
    dem_terrain_focal2$relief <- sqrt(dem_terrain_focal2$grad_easting^2 + dem_terrain_focal2$grad_northing^2)

    writeRaster(dem_terrain_focal, here("data", "dem_terrain_focal.tif"))
    writeRaster(dem_terrain_focal2, here("data", "dem_terrain_focal2.tif"))
  }


  # nepal
  # curvature/twi/spi/tri/distance to river/stream sediment transport index(STI)
  # https://www.esri.com/arcgis-blog/products/product/imagery/understanding-curvature-rasters/
  # https://www.rdocumentation.org/packages/spatialEco/versions/2.0-2/topics/curvature
  if (file.exists(here("data", "dem_curvature.tif"))) {
    crv_planform <- rast(here("data", "crv_planform.tif")) %>%
      project(crs_nepal$input)
    crv_profile <- rast(here("data", "crv_profile.tif")) %>%
      project(crs_nepal$input)
  } else {
    crv_planform <- spatialEco::curvature(dem$dem_km, type = "planform") %>%
      rename(crv_planform = dem_km)
    crv_profile <- spatialEco::curvature(dem$dem_km, type = "profile") %>%
      rename(crv_profile = dem_km)
    # varnames(crv) <- "crv"
    # longnames(crv) <- "curvature"
    writeRaster(crv_planform, here("data", "crv_planform.tif"), overwrite = TRUE)
    writeRaster(crv_profile, here("data", "crv_profile.tif"), overwrite = TRUE)
  }
}
# crv <- curvature(dem$dem_km, type = "platform")

# geology


# https://www.sciencebase.gov/catalog/item/60c3b89fd34e86b93897ef19
# geo8alg <- st_read(
#   here("data/geo8alg/geo8alg.shp")
# ) %>%
#   st_transform(crs = crs_nepal) %>% # to UTM
#   st_intersection(bnd_out)

# https://catalog.data.gov/dataset/geologic-map-of-south-asia-geo8ag
# geo8apg <- st_read(
#   here("data/geo8apg/geo8apg.shp")
# ) %>%
#   st_transform(crs = crs_nepal) %>% # to UTM
#   st_intersection(st_as_sfc(landslides_bbox))
# st_intersection(bnd_out)

# original file before bru fill missing
# nepal_geo %<-% {st_read(here("data", "Nep_Geo", "Geology", "Nep_geology.shp")) %>%
#   st_transform(crs = crs_nepal) %>%
#   st_intersection(bnd_out)
# }

geology %<-% {
  st_read(here("data", "nepal_geo_fill_.shp")) %>%
    st_transform(crs = crs_nepal) %>%
    st_intersection(bnd_out)
}

# geology_gangetic <- geology[geology$ROCK_TYPES == "Gangetic Plain",]
#
# ggplot() + geom_sf(data = geology_gangetic, aes(fill = ROCK_TYPES)) + geom_sf(data = bnd_out_, fill = NA, col = "red")
#
# bnd_out_ <- st_difference(bnd_out, geology_gangetic)


# Geology without landslides
geology$landslides_count <- lengths(st_intersects(geology, landslides_c))
# geology$ROCK_TYPES_ <-  geology$ROCK_TYPES %in% c("Bhimphedi Group",
#                                                       "Gangetic Plain",
#                                                       "Precambrian Igneous Rocks",
#                                                       "Sub Himalaya")

geology_df <- as.data.frame(geology) %>%
  group_by(ROCK_TYPES) %>%
  summarise(
    total_count = sum(landslides_count),
    .groups = "drop"
  )
# ROCK_TYPES with landslides
geology_ref <- geology_df %>%
  filter(total_count != 0) %>%
  pull(ROCK_TYPES)

geology$ROCK_TYPES_ref <- ifelse(geology$ROCK_TYPES %in% geology_ref,
  0, 1
)

geology$ROCK_TYPES <- as.factor(geology$ROCK_TYPES)

geology <- geology %>%
  #   mutate(ROCK_TYPES = stringr::str_replace(ROCK_TYPES, "Bhimphedi Group", "Nawakot Group")) %>%
  mutate(ROCK_TYPES = stringr::str_replace(ROCK_TYPES, "Gangetic Plain", "Siwalik Group"))
#   mutate(ROCK_TYPES = stringr::str_replace(ROCK_TYPES, "Precambrian Igneous Rocks", "Kuncha Group")) %>%
#   mutate(ROCK_TYPES = stringr::str_replace(ROCK_TYPES, "Sub Himalaya", "Siwalik Group"))


# chr rather than factor, so that we can keep the name instead of showing ID in the summary
# relevel to make certain level as baseline if the hyperparameter is fixed
# geology$ROCK_TYPES <- relevel(as.factor(geology$ROCK_TYPES),
#                                 ref = "Higher Himalaya Crystallines")

# nepal_geof <- system.file("data/Nep_Geo/Geology/Nep_geology.shp",
#                           package="terra") # doesnt work for some reasons


# geology_rast <- rasterize(
#   vect(geology),
#   rast(vect(geology), ncols = 10000, nrows = 20000),
#   "ROCK_TYPES"
# )
# source("nepal_geo_rast_fill.R")
# geology_rast_fill <-
#   rast(here("data", "nepal_geo_rast_fill.tif")) %>%
#                               project(crs_nepal$input) %>%
#                               crop(bnd_out, mask = TRUE)


#########################################
# tifpath <- system.file(here(
#   "data",
#   "Land cover of Nepal 2010",
#   "data",
#   "np_lc_2010_v2f.tif"
# ),
# package = "stars")
# tif <- read_stars(tifpath)
# landcover <- st_as_sf(tif)


# mesh --------------------------------------------------------------------


## spatial mesh ------------------------------------------------------------


system.time({
  nepal_lattice_sfc <- {
    hexagon_lattice(
      bnd = bnd, x_bin = x_bin,
      edge_len_n = edge_len_n
    )
  }
})
plan(sequential)
# system.time({
#   mesh_fm2 <- fm_mesh_2d_inla(loc = landslides, boundary = c(bnd,bnd_out))})

plan(multicore, workers = ncore)

# system.time({
  mesh_fm <- fm_mesh_2d_inla(
    loc = nepal_lattice_sfc$lattice,
    boundary = fm_extensions(bnd, c(
      nepal_lattice_sfc$edge_len,
      10 * nepal_lattice_sfc$edge_len
    )), # a shortcut for bnd1 and bnd2
    # or use boundary, here for comparison with rcdt
    max.edge = c(
      1.1 * nepal_lattice_sfc$edge_len, # avoid numerical overflow
      5 * nepal_lattice_sfc$edge_len
    ),
    # max.n.strict = c(
    # 15300,  # dof 15236 back then
    # as.integer(0.9 * rcdt_fm$n),
    # we want to simplify a bit at the boundary otherwise it keeps running forever
    # 2000
    # ), # arbitrary
    # min.angle = 1,
    cutoff = 0.9 * nepal_lattice_sfc$edge_len, # Filter away adjacent points to avoid Q matrix not positive definite because the boundary resolution is too high
    crs = fm_crs(bnd)
  ) # Offset for extra boundaries, if needed.
# })

# subdivide mesh taking n=2 too much RAM, n=1 does not improve anything
# mesh_fm <- fm_subdivide(mesh_fm, 1)

# fm_pixels for prediction ----------------------------------------------------------
source("pxl.R")

## asp mesh ----------------------------------------------------------------

# mesh_asp %<-% {
#   fm_mesh_1d(
#     loc = seq(0, 2 * pi,
#       length.out = seg + 1
#     ),
#     boundary = "cyclic",
#     degree = 2
#   )
# }
plan(sequential)
