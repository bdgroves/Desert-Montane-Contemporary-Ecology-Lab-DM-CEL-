# scripts/make_processed_sample2.R
# Creates small "processed" artifacts for CI + OSF sync:
# - CSV: monthly NDVI summary per site
# - GPKG: site points + 10km buffers
# - TIF: tiny mock NDVI raster (toy)

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales")

library(tidyverse)
library(lubridate)
library(sf)
library(terra)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# --- Sites ------------------------------------------------------------
sites <- tribble(
  ~site_name,        ~lat,    ~lon,
  "Eureka Dunes",    37.095, -117.670,
  "White Mountains", 37.550, -118.230,
  "Tuolumne County", 38.030, -120.080
)

# --- Time series (toy NDVI) ------------------------------------------
set.seed(123)
df <- map_dfr(1:nrow(sites), function(i) {
  tibble(
    site_name = sites$site_name[i],
    date = seq.Date(as.Date("2018-01-01"), as.Date("2024-12-31"), by = "1 month"),
    ndvi = scales::rescale(
      sin(seq(0, 12*pi, length.out = 84)) + rnorm(84, sd = 0.15) + runif(1, -0.1, 0.1),
      to = c(0.05, 0.85)
    )
  )
})

monthly <- df %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(site_name, year, month) %>%
  summarise(ndvi_mean = mean(ndvi), .groups = "drop") %>%
  arrange(site_name, year, month)

readr::write_csv(monthly, "data/processed/site_monthly_ndvi.csv")

# --- GeoPackage: points + 10km buffers (EPSG:3310) -------------------
sites_sf <- st_as_sf(sites, coords = c("lon","lat"), crs = 4326)
sites_ca <- st_transform(sites_sf, 3310)
buffers  <- st_buffer(sites_ca, dist = 10000) %>% st_transform(4326)
st_write(sites_sf, "data/processed/sites.gpkg", layer = "sites", delete_dsn = TRUE, quiet = TRUE)
st_write(buffers,  "data/processed/sites.gpkg", layer = "buffers_10km", append = TRUE, quiet = TRUE)

# --- Tiny mock raster near White Mountains ---------------------------
# Create a 50x50 raster grid centered roughly on White Mountains
ctr <- sites %>% filter(site_name == "White Mountains")
ext <- ext(ctr$lon - 0.15, ctr$lon + 0.15, ctr$lat - 0.15, ctr$lat + 0.15)
r <- rast(ext, nrows = 50, ncols = 50, crs = "EPSG:4326")

# Make a smooth “greenness” blob field
xy <- crds(r, df = TRUE)
cx <- mean(range(xy$x)); cy <- mean(range(xy$y))
val <- exp(-(((xy$x - cx)^2 + (xy$y - cy)^2) / (2 * 0.03^2))) + rnorm(nrow(xy), sd = 0.02)
values(r) <- scales::rescale(val, to = c(0, 1))
writeRaster(r, "data/processed/mock_ndvi_white_mtns.tif", overwrite = TRUE)

message("Wrote: ",
        "\n - data/processed/site_monthly_ndvi.csv",
        "\n - data/processed/sites.gpkg (layers: sites, buffers_10km)",
        "\n - data/processed/mock_ndvi_white_mtns.tif")
