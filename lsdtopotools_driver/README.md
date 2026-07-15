# COP30DEM Topographic Analysis Workflow

This repository contains two driver files for processing a digital elevation model (DEM) named `cop30dem`.

The workflow is split into two stages:

1. **`driver_01.driver`** — creates a hillshade, extracts basins, junctions, channels, and segmented M-chi data.
2. **`driver_02.driver`** — uses the M-chi output from Stage 1 to calculate hillslope and relief metrics.

## Tutorial

Watch the tutorial here:

[https://www.youtube.com/watch?v=LLiD4MhVDMc](https://www.youtube.com/watch?v=LLiD4MhVDMc)

If your DEM is currently in GeoTIFF format, it can be converted to BIL format before running the workflow. For example, this can be done in R using rgdal. Example R code:

```
library(rgdal)

dem <- readGDAL("cop30dem.tif")
writeGDAL(dem, "cop30dem.bil", drivername = "EHdr")
```



---

## Files

This repository contains:

```text
README.md
driver_01.driver
driver_02.driver
cop30dem.bil    # user-provided DEM file
cop30dem.hdr    # header file associated with the BIL raster
