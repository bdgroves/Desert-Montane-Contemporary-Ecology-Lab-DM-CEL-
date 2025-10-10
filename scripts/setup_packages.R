# scripts/setup_packages.R
pkgs <- c(
  "tidyverse", "sf", "terra", "MODISTools",
  "targets", "broom", "janitor", "tmap",
  "plotly", "osfr", "lubridate"
)
install.packages(setdiff(pkgs, rownames(installed.packages())), repos = "https://cloud.r-project.org")