# scripts/example_analysis.R
# Minimal example to structure data and a plot

library(tidyverse)
library(lubridate)

sites <- tribble(
  ~site_name,        ~lat,    ~lon,
  "Eureka Dunes",    37.095, -117.670,
  "White Mountains", 37.550, -118.230,
  "Tuolumne County", 38.030, -120.080
)

# Placeholder time series
set.seed(42)
df <- map_dfr(1:nrow(sites), function(i) {
  tibble(
    site_name = sites$site_name[i],
    date = seq.Date(as.Date("2001-01-01"), as.Date("2024-12-31"), by = "6 months"),
    ndvi = scales::rescale(sin(seq(0, 20, length.out = 48)) + rnorm(48, sd = 0.2), to = c(0.1, 0.8))
  )
})

p <- ggplot(df, aes(date, ndvi, color = site_name)) +
  geom_line(alpha = 0.8) +
  labs(title = "NDVI (placeholder) across desert–montane gradient",
       x = "Date", y = "NDVI (unitless)") +
  theme_minimal()

# Save a figure into outputs
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
ggsave("outputs/figures/ndvi_placeholder.png", p, width = 8, height = 4, dpi = 150)

message("Wrote outputs/figures/ndvi_placeholder.png")