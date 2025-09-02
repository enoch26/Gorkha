# Gorkha Earthquake Data Resources

## Description


This repository documents and references various geospatial datasets used for environmental and hazard analysis in Nepal, particularly for landslide and earthquake studies. Data includes administrative boundaries, geology, hydrology, elevation, land cover, rainfall, and landslide inventories.

> **Note:** Some administrative boundaries and datasets are updated periodically. Always verify the latest versions via official portals (e.g., Nepal Survey Department, ICIMOD, USGS, FAO, etc.).

---

## 📁 Administrative Boundaries (Shapefiles)

**Main source:**  
- [Nepal Administrative Boundaries (WGS84)](https://download.hermes.com.np/nepal-administrative-boundary-wgs/)

**Layers included:**
- National Boundary
- Province Boundary
- District Boundary
- Local Level (Municipality) Boundary
- Ward Map
- Settlement Names
- River Network (Line and Polygon)

**Additional sources:**
- [Nepal SHP Archive - Google Drive](https://sites.google.com/view/maze215/Maps/Nepal-Shp)
- [National Geoportal of Nepal](https://nationalgeoportal.gov.np/#/dataset)

---

## Landslide Inventories

### Valagussa et al. (2021)
- [Inventory & Bounding Box](https://www.sciencebase.gov/catalog/item/61f040e1d34e8b818adc3251)

---

## Land Cover Data

**FAO (2021):**  
_The Himalaya Regional Land Cover Database_  
- [Metadata & Access](https://data.apps.fao.org/map/catalog/srv/eng/catalog.search#/metadata/46d3c2ef-72c3-4f96-8e32-40723cd1847b)  

---

## 🪨 Geology

- [Geological Data of Nepal (ScienceBase)](https://www.sciencebase.gov/catalog/item/60c3b89fd34e86b93897ef19)
- [Province and Regional Geological Maps – Department of Mines and Geology, Nepal](https://dmgnepal.gov.np/en/resources/province-and-regional-geological-maps-6665)
- [ESCAP 1993 Map (UN Repository)](https://repository.unescap.org/handle/20.500.12870/4866)
- [Geological Data for Gorkha Earthquake Area (ICIMOD)](https://rds.icimod.org/Home/DataDetail?metadataId=24676&searchlist=True)

---

## 🏔️ Digital Elevation Model (DEM)

- [Copernicus 30m DEM (OpenTopography)](https://opentopography.org/news/updated-copernicus-30m-DEM-available)

---

## Relief Maps

- Channel steepness index and distance metric to channel raster maps of the Gorkha Earthquake 2015-affected area computed from DEM and processed with LSDTopoTools, see `lsdtopotools_driver` folder for scripts and details.

---

## 🌧️ Rainfall Data

- [CHIRPS v3 Rainfall Data](https://www.chc.ucsb.edu/data/chirps3)

---

## 🗺️ Raster Processing Note

- To fill the gaps caused by buffering between the study area and `nep_geo.shp`, use the script `nepal_geo_rast_fill.R`. This applies nearest-neighbor interpolation to ensure full coverage in the Gorkha district, making the raster suitable for subsequent spatial analysis.

---

## 📌 Additional Dataset (Partial Coverage)

- [USGS ScienceBase Item (smaller spatial extent)](https://www.sciencebase.gov/catalog/item/582c74fbe4b04d580bd377e8)

---

## ⚙️ Resources for Raster Interpolation Comparison

- [Geometry Operations in R (cubic vs. Lanczos)](https://r.geocompx.org/geometry-operations)

## R code
`compiler.R`: compiles and processes various geospatial datasets into a unified format for analysis.
`nepal_geo_rast_fill.R`:fills gaps between Nep_geo.shp and the study area boundary—caused by buffering—using nearest-neighbor values, ensuring complete and consistent geographical data for the Gorkha district for subsequent analysis.