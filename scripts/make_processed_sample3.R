# scripts/make_processed_sample3.R
# Creates a timestamped processed CSV and a new figure so CI has fresh files to upload.

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")

library(tidyverse)
library(lubridate)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# simple toy dataset
set.seed(777)
sites <- c("Eureka Dunes", "White Mountains", "Tuolumne County")
df <- tibble(
  site_name = sample(sites, 300, replace = TRUE),
  date = seq.Date(Sys.Date() - 299, Sys.Date(), by = "1 day"),
  ndvi = pmin(pmax(rnorm(300, mean = 0.45, sd = 0.15), 0), 1)
)

# timestamped filenames so they’re obviously “new”
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

csv_path <- file.path("data/processed", paste0("ndvi_daily_", stamp, ".csv"))
readr::write_csv(df, csv_path)

# quick figure (histogram) saved to outputs/figures
p <- ggplot(df, aes(ndvi)) +
  geom_histogram(bins = 30) +
  labs(title = paste("NDVI distribution (", stamp, ")"), x = "NDVI", y = "Count") +
  theme_minimal()

png_path <- file.path("outputs/figures", paste0("ndvi_hist_", stamp, ".png"))
ggsave(png_path, p, width = 7, height = 4, dpi = 150)

message("Wrote:\n - ", csv_path, "\n - ", png_path)
