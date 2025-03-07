# https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
# https://rpubs.com/MRufino/world_freshwater
# https://wiki.openstreetmap.org/wiki/Downloading_data
# http://gaia.geosci.unc.edu/rivers/
library(osmdata)
# bb <- getbb('Greater London, U.K.')
# The 2015 Gorkha earthquake occurred on April 25 at 11:26 a.m. UTC, local time 11:56 a.m.
if(file.exists(here("data", "osm_waterways.osm"))) {
  waterway <- osmdata_sf(opq(bbox_coords) %>% add_osm_feature(key = "waterway", value = "river")
                         , doc = here("data", "osm_waterways.osm"))
} else {
  waterway <- osmdata::opq(
    bbox = bbox_coords,
    datetime = "2015-04-25T11:26:00Z", timeout = 1000
    # datetime = "2015-04-24T00:00:00Z",
    # datetime2 = "2015-04-25T11:26:00Z"
  ) %>% 
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_xml(filename = "data/osm_waterways.osm") %>% 
    osmdata_sf()
}

waterway <- osmdata_sf(opq(bbox_coords) %>% add_osm_feature(key = "waterway", value = "river")
                       , doc = here("data", "osm_waterways.osm"))
  

ggplot() + 
  geom_sf(data = waterway$osm_lines, col = "blue") + 
  geom_sf(data = waterway$osm_polygons, fill = "green") + 
  geom_sf(data = waterway$osm_multilines, col = "blue") + 
  geom_sf(data = bnd_out, fill = NA, col = "red")
ggsave("figures/waterway_osm.png", width = tw, height = tw/2)



# %>%
  # trim_osmdata(bbox_coords)

# https://milospopovic.net/mapping-raster-files-with-terra-in-r/https://milospopovic.net/mapping-raster-files-with-terra-in-r/
