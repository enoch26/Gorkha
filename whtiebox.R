# whitebox ----------------------------------------------------------------
# https://vt-hydroinformatics.github.io/rgeoraster.html#introduction-2
# https://gis.stackexchange.com/questions/488117/r-terra-flow-accumulation-resulting-raster-with-dn-1
library(whitebox)
wbt_init(exe_path='/home/s1872841/.local/share/R/whitebox/WBT/whitebox_tools')  
terra::flowAccumulation()

wbt_breach_depressions_least_cost(
  dem = "./data/lsdtt/cop30dem.tif",
  output = "./data/lsdtt/cop30dem_br.tif",
  dist = 5,
  fill = TRUE)

wbt_d_inf_flow_accumulation("./data/lsdtt/cop30dem_br.tif",
                            "./data/lsdtt/DinfFAsca.tif")

wbt_d_inf_flow_accumulation(input = "./data/lsdtt/cop30dem_br.tif",
                            output = "./data/lsdtt/DinfFAsca.tif",
                            out_type = "Specific Contributing Area")

wbt_slope(dem = "./data/lsdtt/cop30dem.tif",
          output = "./data/lsdtt/demslope.tif",
          units = "degrees")

wbt_wetness_index(sca = "./data/lsdtt/DinfFAsca.tif",
                  slope = "./data/lsdtt/demslope.tif",
                  output = "./data/lsdtt/TWI.tif")

twi <- raster("./data/lsdtt/TWI.tif")

twi[twi > 0] <- NA

tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(twi)+
  tm_raster(style = "cont", palette = "PuOr", legend.show = TRUE, alpha = 0.5)+
  tm_scale_bar()