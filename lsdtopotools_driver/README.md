# COP30DEM Topographic Analysis Workflow

This repository contains two driver files for processing a digital elevation model (DEM) named `cop30dem`.

The workflow is split into two stages:

1. **`driver_01.driver`** — creates hillshade, extracts basins, junctions, channels, and segmented M-chi data.
2. **`driver_02.driver`** — uses the M-chi output from Stage 1 to calculate hillslope and relief metrics.

## Tutorial

Watch the tutorial here:

[https://www.youtube.com/watch?v=LLiD4MhVDMc](https://www.youtube.com/watch?v=LLiD4MhVDMc)

---

## Files

This repository should contain:

```text
README.md
driver_01.driver
driver_02.driver
cop30dem.bil 
