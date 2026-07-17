# COP30DEM Topographic Analysis Workflow

This repository contains two driver files for processing a digital elevation model (DEM) named `cop30dem` using [LSDTopoTools](https://lsdtopotools.github.io/).

The workflow is split into two stages:

1. **`driver_01.driver`** — creates a hillshade, extracts basins, junctions, channels, and segmented M-chi data.
2. **`driver_02.driver`** — uses the M-chi output from Stage 1 to calculate hillslope and relief metrics.

## Tutorial

Watch the tutorial here:

[https://www.youtube.com/watch?v=LLiD4MhVDMc](https://www.youtube.com/watch?v=LLiD4MhVDMc)

If your DEM is currently in GeoTIFF format, it can be converted to BIL format before running the workflow. For example, this can be done in R using `rgdal`.

Example R code:

```r
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
```

The DEM should use the base name:

```text
cop30dem
```

---

## Workflow Summary

### Stage 1: `driver_01.driver`

This driver reads the DEM and creates the main drainage and basin outputs.

It does the following:

- Reads `cop30dem`
- Writes outputs with the prefix `cop30dem`
- Creates a hillshade
- Carves channels before filling depressions
- Removes sea areas
- Extracts drainage basins
- Finds complete basins in the DEM window
- Prints basin raster
- Prints junctions to CSV
- Prints segmented M-chi data to CSV

The important output from this stage is:

```text
cop30dem_MChiSegmented.csv
```

This file is used by `driver_02.driver`.

---

### Stage 2: `driver_02.driver`

This driver uses the output from Stage 1 to calculate hillslope and terrain metrics.

It does the following:

- Reads `cop30dem`
- Uses `cop30dem_MChiSegmented.csv`
- Tags hillslopes with channel values
- Uses the `m_chi` column from the CSV
- Calculates topographic wetness index
- Calculates channel-ridge relief

---

## Driver Files

### `driver_01.driver`

```text
read fname: cop30dem
write fname: cop30dem
channel heads fname: NULL

write_hillshade: true
carve_before_fill: true

# We remove parts of the landscape below 250 metres elevation to remove some alluvial fans.
# minimum_elevation: 250
# maximum_elevation: 7250
remove_seas: true

print_segmented_M_chi_map_to_csv: true
find_basins: true
# get_basins_from_outlets: true
# basin_outlet_csv: Outlet.csv

threshold_contributing_pixels: 1000
minimum_basin_size_pixels: 5000
maximum_basin_size_pixels: 1000000000
only_take_largest_basin: false
search_radius_nodes: 4
print_basin_raster: true
find_complete_basins_in_window: true
print_junctions_to_csv: true

# end of file
# https://www.youtube.com/watch?v=LLiD4MhVDMc
```

### `driver_02.driver`

```text
read fname: cop30dem
write fname: cop30dem
channel heads fname: NULL

carve_before_fill: true

# We remove parts of the landscape below 250 metres elevation to remove some alluvial fans.
# minimum_elevation: 250
# maximum_elevation: 7250
remove_seas: true

threshold_contributing_pixels: 1000
minimum_basin_size_pixels: 5000
maximum_basin_size_pixels: 1000000000
fixed_channel_csv_name: cop30dem_MChiSegmented.csv
tag_value_column_string: m_chi

tag_hillslopes_with_channel_values: true
calculate_topographic_wetness_index: true
calculate_channel_ridge_relief: true

# end of file
```

---

## How to Run
After installing LSDTopoTools, 
Run `driver_01.driver` first.

Example:

```bash
lsdtt-chi-mapping driver_01.driver
```

Then run `driver_02.driver`.

Example:

```bash
lsdtt-basic-metrics driver_02.driver
```

Depending on your installation, the command may instead be:

```bash
./lsdtt-chi-mapping driver_01.driver
./lsdtt-chi-mapping driver_02.driver
```

Use the command required by your installed topographic analysis tool.

---

## Expected Output Files

After running `driver_01.driver`, check for:

```text
cop30dem_MChiSegmented.csv
```

This file is required by `driver_02.driver`.

Other possible outputs include:

```text
cop30dem_HS.bil
cop30dem_Basins.bil
cop30dem_Junctions.csv
cop30dem_TWI.bil
cop30dem_Relief.bil
```

Actual filenames may vary depending on the software version.

---

## Notes

- `driver_01.driver` must be run before `driver_02.driver`.
- Both drivers use the same DEM base name: `cop30dem`.
- `driver_02.driver` depends on `cop30dem_MChiSegmented.csv`.
- Basin sizes are measured in pixels.
- If `cop30dem_MChiSegmented.csv` is missing, run `driver_01.driver` first.
- If the output CSV has a different name, edit `fixed_channel_csv_name` in `driver_02.driver`.

---

