
# summary statistics for table 1 ------------------------------------------------------


CV_thin <- FALSE; CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
source("read_data.R")


# Typical landslide scale and elevation are suggested to be given --------------------------------------------------------

library(terra)
library(sf)
library(dplyr)
library(knitr)

# If elevation is not already in the sf object, extract from DEM
ls_v <- vect(landslides_c)
elev <- terra::extract(dem[[2]], ls_v)

landslides_c$elev_km <- elev[,2]
landslides_c$elev_m  <- landslides_c$elev_km * 1000
fmt <- function(x) format(x, scientific = TRUE, digits = 3)

summary_table_fmt <- data.frame(
  Variable = c("Area (m$^2$)", "Elevation (m a.s.l.)"),
  Min    = c(fmt(min(landslides_c$Area_m2, na.rm = TRUE)),
             fmt(min(landslides_c$elev_m, na.rm = TRUE))),
  Q1     = c(fmt(quantile(landslides_c$Area_m2, 0.25, na.rm = TRUE)),
             fmt(quantile(landslides_c$elev_m, 0.25, na.rm = TRUE))),
  Median = c(fmt(median(landslides_c$Area_m2, na.rm = TRUE)),
             fmt(median(landslides_c$elev_m, na.rm = TRUE))),
  Mean   = c(fmt(mean(landslides_c$Area_m2, na.rm = TRUE)),
             fmt(mean(landslides_c$elev_m, na.rm = TRUE))),
  Q3     = c(fmt(quantile(landslides_c$Area_m2, 0.75, na.rm = TRUE)),
             fmt(quantile(landslides_c$elev_m, 0.75, na.rm = TRUE))),
  Max    = c(fmt(max(landslides_c$Area_m2, na.rm = TRUE)),
             fmt(max(landslides_c$elev_m, na.rm = TRUE)))
)

latex_tab <- knitr::kable(
  summary_table_fmt,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "Summary statistics of landslide area and elevation."
)

cat(latex_tab)


## alternative xtable ------------------------------------------------------

library(xtable)
# Build summary table
summary_table <- data.frame(
  Variable = c("Area (m$^2$)", "Elevation (m a.s.l.)"),
  Min    = c(min(landslides_c$Area_m2, na.rm = TRUE),
             min(landslides_c$elev_m, na.rm = TRUE)),
  Q1     = c(quantile(landslides_c$Area_m2, 0.25, na.rm = TRUE),
             quantile(landslides_c$elev_m, 0.25, na.rm = TRUE)),
  Median = c(median(landslides_c$Area_m2, na.rm = TRUE),
             median(landslides_c$elev_m, na.rm = TRUE)),
  Mean   = c(mean(landslides_c$Area_m2, na.rm = TRUE),
             mean(landslides_c$elev_m, na.rm = TRUE)),
  Q3     = c(quantile(landslides_c$Area_m2, 0.75, na.rm = TRUE),
             quantile(landslides_c$elev_m, 0.75, na.rm = TRUE)),
  Max    = c(max(landslides_c$Area_m2, na.rm = TRUE),
             max(landslides_c$elev_m, na.rm = TRUE))
)

# Round values
summary_table[, -1] <- round(summary_table[, -1], 2)

summary_table
xt <- xtable(summary_table,
             caption = "Summary statistics of landslide area and elevation.")

print(xt, include.rownames = FALSE, booktabs = TRUE,
      file = "landslide_summary_table.tex")

