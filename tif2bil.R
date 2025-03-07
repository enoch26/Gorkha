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

tw <- 15.55528
# https://gis.stackexchange.com/questions/354337/tif-file-conversion-to-bil-format

# UTM45N
# https://spaceappnet.wordpress.com/2020/06/24/coordinate-systems-used-in-nepal/
# https://epsg.io/32645
# EPSG:32645


# data boundary box
# Valagussa and others (2021)
# 84.24265675600003, 27.383653885000058,
# 86.69613706100006, 28.736201582000035
# bbox_coords <- c(
#   85.1, 27.5,
#   85.7, 28
# )
# 20250220
# bbox_coords <- c(
#   84, 27.3,
#   87, 29
# )
bbox_coords <- c(
  84, 27.3,
  87, 29
)
names(bbox_coords) <- c("xmin", "ymin", "xmax", "ymax")

landslides_bbox <- st_bbox(bbox_coords, crs = 4326)

# landslides_bbox_ <- st_coordinates(st_as_sfc(landslides_bbox)) %>%
#   as.data.frame() %>%
#   st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
#   st_transform(crs = 32645) %>%
#   st_bbox()
# 
# ggplot() +
#   geom_sf(data = st_as_sfc(landslides_bbox_), fill = NA, col = "red") +
#   coord_sf(crs = 32645)

# ggplot() + geom_sf(data = nepal_nc) +
#   geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, col = "red")
# ggplot() + geom_sf(data = st_as_sfc(landslides_bbox), fill = NA, col = "red")

# extent      : 80.01764, 88.24431, 26.32958, 30.44931  (xmin, xmax, ymin, ymax)
# > dem
# class       : SpatRaster 
# dimensions  : 14831, 29616, 1  (nrow, ncol, nlyr)
# resolution  : 0.0002777778, 0.0002777778  (x, y)
# extent      : 80.01764, 88.24431, 26.32958, 30.44931  (xmin, xmax, ymin, ymax)
# coord. ref. : lon/lat WGS 84 (EPSG:4326) 
# source      : output_hh.tif 
# name        : output_hh 
# > dem <- rast(here("data", "output_hh.tif")) %>%
#   project("epsg:32645") 
# Garbage collection 39 = 30+4+5 (level 0) ... 
# 139.1 Mbytes of cons cells used (52%)
# 31.1 Mbytes of vectors used (48%)
# > dem
# class       : SpatRaster 
# dimensions  : 17046, 29365, 1  (nrow, ncol, nlyr)
# resolution  : 27.99432, 27.99432  (x, y)
# extent      : -197871.2, 624182, 2912173, 3389364  (xmin, xmax, ymin, ymax)
# coord. ref. : WGS 84 / UTM zone 45N (EPSG:32645) 
# source(s)   : memory
# name        :  output_hh 
# min value   :   44.83709 
# max value   : 8737.55859 


dem <- rast(here("data", "output_hh.tif")) %>%
  crop(st_as_sfc(landslides_bbox)) %>% project("epsg:32645")

# dem_terrain <- terrain(dem, v=c("slope", "aspect"), unit="radians")
# dem_hs <- shade(dem_terrain$slope, dem_terrain$aspect)

# st_bbox(st_transform(st_as_sfc(bbox), crs))

# TODO it is not working since bbox still a rectangle not a transformed rectangle
landslides_bbox_ <- st_bbox(
  st_transform(
    st_as_sfc(landslides_bbox),
    32645
  )
)

ggplot() +
  geom_spatraster(data = dem) +
  geom_sf(data = st_as_sfc(landslides_bbox_), fill = NA, col = "red")
ggsave("data/lsdtt/figure/dem_crop.png", width = tw, height = tw / 2)

# ggplot() +
#   geom_spatraster(data = dem_hs) +
#   geom_sf(data = st_as_sfc(landslides_bbox_), fill = NA, col = "red")
# ggsave("data/lsdtt/figure/dem_crop_hs.png", width = tw, height = tw / 2)

names(dem) <- "Band 1"
writeRaster(dem, here("data", "lsdtt", "cop30dem.tif"))
writeRaster(dem, here("data", "lsdtt", "cop30dem.bil"), filetype = "ENVI", gdal = "INTERLEAVE=BIL", overwrite = TRUE)
# writeRaster(dem_hs, here("data", "lsdtt", "cop30dem_hs.bil"), filetype = "ENVI", gdal = "INTERLEAVE=BIL", overwrite = TRUE)


# ggplot() +
#   geom_spatraster(data = dem)
# ggsave("dem.png", width = tw, height = tw / 2)

# writeRaster(dem_bil, here("data", "cop30dem123.bil"), filetype = "ENVI", gdal = "INTERLEAVE=BIL")



# The outlets of Nepal's basins include points where rivers flow out of the basin. These outlets can be used to study the flow of water and sediment in the basin.
# Examples of basin outlets in Nepal
# Majhitar: The outlet of the Tamor sub-basin
# Uwagaon: The outlet of the Arun sub-basin
# Rabuwa Bazar: The outlet of the Dudh Koshi sub-basin
# Busti: The outlet of the Tama Koshi sub-basin
# Pachuwarghat: The outlet of the Indrawati and Sunkoshi-Bhotekoshi sub-basins
# Narayanghat: The outlet of the Narayani River basin
# df <- data.frame(
#   latitude = c(
#     "27.26792327230499N", 
#     "27.691647225855437N",
#     "27.50394742204097N", 
#     "27.29275386099384N",
#     "27.619944537954215N", 
#     "27.77908511928258N",
#     "27.637551979911496N", 
#     "28.204695875945223N",
#     "28.164822309798204N", 
#     "27.74135036247492N",
#     "27.820628149786064N", 
#     "27.92677787571987N"
#   ),
#   longitude = c(
#     "86.65935560270623E",# Rabuwa Bazar
#     "85.30227816866642E", # Bagmati River
#     "85.2350136742367E",# Bagmati River
#     "85.4328005384429E", # Bagmati River
#     "86.07863608498667E",# Tamakoshi Bridge
#     "85.89237203365089E", # Sunkoshi Bridge
#     "85.70783343832754E",# Dolalghat Bridge
#     "85.57146650552049E", # Langtang Khola
#     "85.34204127471634E",# Trishuli Supsension Bridge
#     "84.4222715363851E", # Devghat Suspension Bridge
#     "84.45459197223755E",# Ghumawune Suspension Bridge
#     "84.4929521909953E") # Marsyangdi Dam
# )
# 
# df <- data.frame(
#   latitude = c(
#     27.26792327230499, 
#     27.691647225855437,
#     27.50394742204097, 
#     27.29275386099384,
#     27.619944537954215, 
#     27.77908511928258,
#     27.637551979911496, 
#     28.204695875945223,
#     28.164822309798204, 
#     27.74135036247492,
#     27.820628149786064, 
#     27.92677787571987,
#     28.16465972671633
#   ),
#   longitude = c(
#     86.65935560270623,# Rabuwa Bazar
#     85.30227816866642, # Bagmati River
#     85.2350136742367,# Bagmati River
#     85.4328005384429, # Bagmati River
#     86.07863608498667,# Tamakoshi Bridge
#     85.89237203365089, # Sunkoshi Bridge
#     85.70783343832754,# Dolalghat Bridge
#     85.57146650552049, # Langtang Khola
#     85.34204127471634,# Trishuli Supsension Bridge
#     84.4222715363851, # Devghat Suspension Bridge
#     84.45459197223755,# Ghumawune Suspension Bridge
#     84.4929521909953, # Marsyangdi Dam
#     85.34203737545674 # Old Shaybru Besi
#     ) 
# )
# 
# write.csv(df, here("data", "Outlet.csv"), row.names = FALSE)
# df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32645)

# st_write(df_sf, "data/Outlet.csv", layer_options = "GEOMETRY=AS_XY", delete_dsn = TRUE)
# outlet <- read.csv("data/Outlet.csv", header = TRUE)
# names(outlet) <- c("longitude", "latitude")
# write.csv(outlet, here("data", "Outlet.csv"), row.names = FALSE)


latlong <- c(
27.72595150067279, 85.17295866233664,
27.646133114154395, 85.46919126300668,
27.571127304230945, 85.53680709838397,
27.760805918578054, 85.42438698296229,
27.75803543926662, 85.59684871233557,
27.450728995424715, 85.81523099834659,
27.58316066038444, 85.27822426186233,
27.60001248404984, 85.3760266031217,
27.890483093287198, 85.1515788939148,
27.589157791614134, 85.51251390309575,
27.634367382184976, 85.51640732793733,
27.714843477108005, 85.61406105786985,
27.647034422835564, 85.62069812227458,
27.74202220710575, 85.47337125153481,
27.328734964279494, 85.42073685763788,
27.32648027909765, 85.41650698883332,
27.233751249949574, 85.91719758216611,
27.145364089809018, 85.4824253753123,
27.665680475752698, 85.47770915559434,
27.027365617698788, 86.20722088204853,
27.774779652957605, 85.26200560908545,
27.581433999774823, 85.27579930503603,
27.36231383467847, 85.47029546951669,
27.473649552905, 85.04173649075544,
27.74733994350331, 85.48233513911651,
27.624266563042116, 85.04372340858204,
27.712715415430814, 85.67123259170832,
27.503679504934414, 85.79942803969132,
27.386384316251895, 85.4811524234918,
27.731068718069746, 85.22922097025443,
27.58157303500867, 85.27583136043636
)
# 
# outlet <- cbind.data.frame(split(latlong, rep(1:2, times=length(latlong)/2)), stringsAsFactors=F) 
# # 
# names(outlet) <- c("latitude", "longitude")
# outlet <- outlet %>% mutate(id = 1:nrow(.), .before = latitude)
# write.csv(outlet, here("data", "lsdtt","Outlet.csv"), row.names = FALSE)

# 28.181745421754837, 85.34282689026497 # chilime bridge
# 27.676079216466963, 86.58943904051256 # Solu River
# 27.54890991823224, 86.57979750921687 # Solu River
# 27.149122229285048, 86.43351870934896 # Dhudh Koshi River


# outlet ------------------------------------------------------------------

# latlong <- c(
#   27.77613123395356, 85.58137700779119,
#   27.75288069251241, 85.10399122433392,
#   27.88674997513437, 85.14209070996796,
#   27.991785255854875, 84.81377790993888,
#   27.890750945689035, 85.14660207307465,
#   27.46082705058156, 86.19008816257936,
#   27.497271971987423, 86.11365064746279,
#   27.540817218492307, 86.12564013550178,
#   27.84116348202301, 85.63191764894185,
#   28.02576852714209, 84.81812374615723
# )
# 
# outlet <- cbind.data.frame(split(latlong, rep(1:2, times=length(latlong)/2)), stringsAsFactors = F)
# names(outlet) <- c("latitude", "longitude")
# outlet <- outlet %>% mutate(id = 1:nrow(.), .before = latitude)
# write.csv(outlet, here("data", "lsdtt",fdr, "Outlet.csv"), row.names = FALSE)



# outlet processing -------------------------------------------------------
if(FALSE){
outlet_df <- read.csv(here("data", "lsdtt", "lanczos_threshold10" , "cop30dem_AllBasinsInfo.csv"), header = TRUE)
outlet_df <- outlet_df %>% select(longitude, latitude)
# outlet_df <- outlet_df %>% select(outlet_longitude, outlet_latitude)
# names(outlet_df) <- c("longitude", "latitude")

ggplot() + geom_spatraster(data = dem, na.rm = TRUE) +
  geom_sf(data = st_as_sf(outlet_df, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
                             st_transform(32645), 
          fill = "red", col = "red", size = 0.05) 
ggsave(here("data", "lsdtt", fdr, "outlet.pdf"))

outlet_df <- outlet_df %>% subset(latitude < 27.6|latitude > 27.7|longitude < 85.1|longitude > 85.5)
# outlet_df_ <- outlet_df %>% filter(latitude > 27, latitude < 27.7, longitude > 84, longitude < 84.7) 
outlet_df_ <- outlet_df %>% filter(latitude > 27.69) 
outlet_df_1 <- outlet_df %>% filter(latitude > 27.125, longitude > 86) 
outlet_df_2 <- outlet_df %>% filter(latitude < 27.69, latitude > 27.35, longitude > 85.1, longitude < 86) 
outlet_df_3 <- outlet_df %>% filter(!latitude < 27.6, !latitude > 27.7, !longitude < 85.1, !longitude > 85.5)

# ggplot() + geom_spatraster(data = dem, na.rm = TRUE) +
#   geom_sf(data = st_as_sf(outlet_df, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
#             st_transform(32645), fill = "green", col = "green", size = 0.05) +
#   geom_sf(data = st_as_sf(outlet_df_3, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
#             st_transform(32645), fill = "red", col = "red", size = 0.05) +
#   geom_sf(data = bnd, fill = NA, col = "purple")
# ggsave(here("data", "lsdtt", fdr, "outlet_3.pdf"))

outlet_df_ <- unique(bind_rows(outlet_df_, outlet_df_1, outlet_df_2)) 

ggplot() + geom_spatraster(data = dem, na.rm = TRUE) +
  geom_sf(data = st_as_sf(outlet_df, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
            st_transform(32645), fill = "green", col = "green", size = 0.05) +
  geom_sf(data = st_as_sf(outlet_df_, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
            st_transform(32645), fill = "red", col = "red", size = 0.05) +
  geom_sf(data = bnd, fill = NA, col = "purple")

ggplot() + geom_spatraster(data = dem_basin, na.rm = TRUE) +
  geom_sf(data = st_as_sf(outlet_df, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
            st_transform(32645), fill = "green", col = "green", size = 0.05) +
  geom_sf(data = st_as_sf(outlet_df_, coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
            st_transform(32645), fill = "red", col = "red", size = 0.05) +
  geom_sf(data = bnd, fill = NA, col = "purple")
ggsave(here("data", "lsdtt", fdr, "outlet_.pdf"))

write.csv(outlet_df_, here("data", "lsdtt", fdr, "Outlet.csv"), row.names = FALSE)
}
# 27 - 29 N
# 84 - 87 E


# 27.26792327230499, 86.65935560270623 # Rabuwa Bazar
# 27.691647225855437, 85.30227816866642 # Bagmati River
# 27.50394742204097, 85.2350136742367
# 27.29275386099384, 85.4328005384429
# 27.619944537954215, 86.07863608498667 # Tamakoshi Bridge
# 27.77908511928258, 85.89237203365089 # Sunkoshi Bridge
# 27.637551979911496, 85.70783343832754 # Dolalghat Bridge
# 28.204695875945223, 85.57146650552049 # Langtang Khola
# 28.164822309798204, 85.34204127471634 # Trishuli Supsension Bridge
# 27.74135036247492, 84.4222715363851 # Devghat Suspension Bridge
# 27.820628149786064, 84.45459197223755 # Ghumawune Suspension Bridge
# 27.92677787571987, 84.4929521909953 #Marsyangdi Dam


# 27.18177488788203, 88.50519354408408 # Majitar
# 25.709968301513467, 84.87755560504337 # Chatar
# 26.71882845632201, 86.50629806355848 # Balan River
# 26.532446644038988, 86.93379814756752 # Koshi Barrage
# 26.949842876225432, 86.27697653478874 # Dudhauli
