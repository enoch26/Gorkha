# Gorkha Earthquake Data Resources

## Description


This repository documents and references various geospatial datasets used for environmental and hazard analysis in Nepal, particularly for landslide and earthquake studies. Data includes administrative boundaries, geology, hydrology, elevation, land cover, rainfall, and landslide inventories.

> **Note:** Some administrative boundaries and datasets are updated periodically. Always verify the latest versions via official portals (e.g., Nepal Survey Department, ICIMOD, USGS, FAO, etc.).

---

## 📁 Administrative Boundaries (Shapefiles)

**Main source:**  
- [Nepal Administrative Boundaries (WGS84)](https://opendatanepal.com/dataset/30a0bbef-a5df-43e9-b87f-b088fb502331/resource/016b2ecc-d890-4573-a29c-1d4b163475da/download/local_unit.zip)

---

## Landslide Inventories

### Valagussa et al. (2021)
- [Inventory & Bounding Box](https://www.sciencebase.gov/catalog/item/61f040e1d34e8b818adc3251)

---

---

## USGS Shape Map 

### USGS ShakeMap Atlas v4 
- [PGA]({https://earthquake.usgs.gov/earthquakes/eventpage/us20002926/shakemap/pga)

---

## Land Cover Data

**FAO (2021):**  
_The Himalaya Regional Land Cover Database_  
- [Metadata & Access](https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/46d3c2ef-72c3-4f96-8e32-40723cd1847b)  

---

## 🪨 Geology

- [Geological Data of Nepal](https://www.researchgate.net/publication/259636889_Regional-scale_landslide_activity_and_landslide_susceptibility_zonation_in_the_Nepal_Himalaya)

---

## 🏔️ Digital Elevation Model (DEM)

- [Copernicus 30m DEM (OpenTopography)](https://opentopography.org/news/updated-copernicus-30m-DEM-available)

---

## Relief Maps

- Channel steepness index and distance metric to channel raster maps of the Gorkha Earthquake 2015-affected area computed from DEM and processed with LSDTopoTools (https://lsdtopotools.github.io/), see `lsdtopotools_driver` folder for scripts and details.

---

## 🌧️ Annual Rainfall Data

- [CHIRPS v3 Rainfall Data](https://www.chc.ucsb.edu/data/chirps3)

---

## 🗺️ Raster Processing Note

- To fill the gaps caused by buffering between the study area and `nep_geo.shp`, use the script `nepal_geo_rast_fill.R`. This applies nearest-neighbour interpolation to ensure full coverage in the Gorkha district, making the raster suitable for subsequent spatial analysis.

---
## R code

`compiler.R`: compiles and processes various geospatial datasets into a unified format for analysis.
`tile_ldsize.R`: Plots the landslide inventory with PGA contour lines and histogram for landslides. 
`mchi.R`: Plots the normalised channel steepness index (ksn) and channel profile analysis.
`pred_zm.R`: Plots the posterior susceptibility map zoom-out.
`coefvar.R`: Plots the coefficient of variation for the intensity and covariate effect maps.