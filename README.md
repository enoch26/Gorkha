# Gorkha Earthquake Data Resources

## Description


This repository documents and references various geospatial datasets used for earthquake-induced landslide (EQIL) modelling in Nepal for Gorkha Earthquake. Data includes administrative boundaries, geology, elevation, land cover, rainfall, and landslide inventories.

> **Note:** Some datasets are updated periodically. Always verify the latest versions via official portals (e.g. USGS, FAO, etc.).

---

## 📁 Administrative Boundaries (Shapefiles)

**Main source:**  
- [Nepal Administrative Boundaries (WGS84)](https://opendatanepal.com/dataset/30a0bbef-a5df-43e9-b87f-b088fb502331/resource/016b2ecc-d890-4573-a29c-1d4b163475da/download/local_unit.zip)

---

## Landslide Inventories

### Valagussa et al. (2021)
- [Inventory & Bounding Box](https://www.sciencebase.gov/catalog/item/61f040e1d34e8b818adc3251)

---

## USGS Shape Map 

### USGS ShakeMap Atlas v4 
- [PGA](https://earthquake.usgs.gov/earthquakes/eventpage/us20002926/shakemap/pga)

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

## Channel Steepness Data

- Channel steepness index and distance metric to channel raster maps of the Gorkha Earthquake 2015-affected area computed from DEM and processed with [LSDTopoTools](https://lsdtopotools.github.io/), see `lsdtopotools_driver` folder for scripts and details.

---

## 🌧️ Annual Rainfall Data

- [CHIRPS v3 Rainfall Data](https://www.chc.ucsb.edu/data/chirps3)

---

## 🗺️ Raster Processing Note

- To fill the gaps caused by buffering between the study area and `nep_geo.shp`, use the script `nepal_geo_rast_fill.R`. This applies nearest-neighbour interpolation to ensure full coverage in the Gorkha district, making the raster suitable for subsequent spatial analysis.

---
## R code

- `compiler.R`: compiles and processes various geospatial datasets into a unified format for analysis and INLA spatial modelling.
- `tile_ldsize.R`: Plots the landslide inventory with PGA contour lines and histogram for landslides. 
- `mchi.R`: Plots the normalised channel steepness index (ksn) and channel profile analysis.
- `pred_zm.R`: Plots the posterior susceptibility map zoom-out.
- `coefvar.R`: Plots the coefficient of variation for the intensity and covariate effect maps.

---
## Citation

For attribution, please cite this work as: Suen, M. H., Naylor, M., Mudd, S., & Lindgren, F. (2025). Influence of river incision on landslides triggered in Nepal by the Gorkha earthquake: Results from a pixel-based susceptibility model using inlabru. arXiv preprint arXiv:2507.08742.

@article{suen2025influence,
  title={Influence of river incision on landslides triggered in Nepal by the Gorkha earthquake: Results from a pixel-based susceptibility model using inlabru},
  author={Suen, Man Ho and Naylor, Mark and Mudd, Simon and Lindgren, Finn},
  journal={arXiv preprint arXiv:2507.08742},
  year={2025}
}

---
## Session Information

The code is currently developed and tested in R 4.5.1. Below is the session information for reproducibility:
```
R version 4.5.1 (2025-06-13)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

time zone: Etc/UTC
tzcode source: system (glibc)

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base 

other attached packages:
 [1] future_1.58.0       tidyterra_0.7.2     terra_1.8-54       
 [4] ggplot2_3.5.2       here_1.0.1          inlabru_2.13.0.9009
 [7] INLA_25.08.29       Matrix_1.7-3        fmesher_0.5.0.9004 
[10] stars_0.6-8         sf_1.0-21           abind_1.4-8        
[13] dplyr_1.1.4        

loaded via a namespace (and not attached):
 [1] generics_0.1.4     tidyr_1.3.1        xml2_1.3.8         class_7.3-23      
 [5] KernSmooth_2.23-26 stringi_1.8.7      lattice_0.22-7     listenv_0.9.1     
 [9] digest_0.6.37      magrittr_2.0.3     grid_4.5.1         RColorBrewer_1.1-3
[13] nanoarrow_0.6.0-1  rprojroot_2.0.4    e1071_1.7-16       DBI_1.2.3         
[17] gdalraster_2.0.0   purrr_1.0.4        scales_1.4.0       codetools_0.2-20  
[21] cli_3.6.5          rlang_1.1.6        units_0.8-7        parallelly_1.44.0 
[25] bit64_4.6.0-1      splines_4.5.1      withr_3.0.2        tools_4.5.1       
[29] parallel_4.5.1     globals_0.18.0     vctrs_0.6.5        R6_2.6.1          
[33] proxy_0.4-27       lifecycle_1.0.4    classInt_0.4-11    stringr_1.5.1     
[37] bit_4.6.0          pkgconfig_2.0.3    pillar_1.10.2      gtable_0.3.6      
[41] glue_1.8.0         Rcpp_1.0.14        splancs_2.01-45    tibble_3.3.0      
[45] tidyselect_1.2.1   dichromat_2.0-0.1  farver_2.1.2       wk_0.9.4          
[49] compiler_4.5.1     sp_2.2-0          
```
