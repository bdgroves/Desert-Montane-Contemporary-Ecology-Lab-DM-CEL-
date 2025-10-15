# 1) Recreate the example data frame from your example script
library(tidyverse)
library(lubridate)

sites <- tribble(
  ~site_name,        ~lat,    ~lon,
  "Eureka Dunes",    37.095, -117.670,
  "White Mountains", 37.550, -118.230,
  "Tuolumne County", 38.030, -120.080
)

set.seed(42)
df <- map_dfr(1:nrow(sites), function(i) {
  tibble(
    site_name = sites$site_name[i],
    date = seq.Date(as.Date("2001-01-01"), as.Date("2024-12-31"), by = "6 months"),
    ndvi = scales::rescale(sin(seq(0, 20, length.out = 48)) + rnorm(48, sd = 0.2), to = c(0.1, 0.8))
  )
})

# 2) Write processed outputs
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(df, "data/processed/ndvi_timeseries.csv")

# (optional) an sf layer of site points saved to GeoPackage
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
library(sf)
sites_sf <- st_as_sf(sites, coords = c("lon","lat"), crs = 4326)
st_write(sites_sf, "data/processed/sites.gpkg", delete_dsn = TRUE)  # overwrite if exists

message("Wrote: data/processed/ndvi_timeseries.csv and data/processed/sites.gpkg")
