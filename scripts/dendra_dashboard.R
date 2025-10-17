# scripts/dendra_current_dashboard.R
# Get current conditions formatted like the Dendra website

quiet_install <- function(pkgs){
  need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
}
quiet_install(c("httr","jsonlite","dplyr","readr","tibble","stringr","lubridate"))

library(httr); library(jsonlite); library(dplyr); library(readr); library(tibble); library(stringr); library(lubridate)

BASE <- "https://api.dendra.science/v2"

get_json <- function(path, query=list()) {
  url <- paste0(BASE, path)
  r <- GET(url, query = query, timeout(60))
  stop_for_status(r)
  fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
}

# ============================================================
# CONFIGURATION
# ============================================================
STATION_SLUG <- "whitemt-barcroft"
ELEVATION_M <- 3782.568  # meters

# Define which datastreams to show (like the website)
DASHBOARD_DATASTREAMS <- list(
  "Battery Voltage" = list(pattern = "Battery Voltage Max", label = "Battery voltage", units = "V"),
  "Air Temperature" = list(pattern = "^Air Temp Avg$", label = "Air temperature", units = "degC"),
  "Relative Humidity" = list(pattern = "Relative Humidity.*Avg", label = "Relative humidity", units = "%"),
  "Barometric Pressure" = list(pattern = "Barometric Pressure.*Avg", label = "Barometric pressure", units = "hPa"),
  "PAR" = list(pattern = "Photosynthetically Active Radiation.*Avg", label = "PAR", units = "µmol/m²"),
  "Total Solar" = list(pattern = "Total Solar Radiation.*Avg", label = "Total solar", units = "W/m²"),
  "Wind Speed" = list(pattern = "Wind Speed.*Avg", label = "Wind speed", units = "m/s"),
  "Wind Direction" = list(pattern = "Wind Direction.*Avg", label = "Wind direction", units = "°")
)

# ============================================================
# GET STATION & DATASTREAMS
# ============================================================

cat("=== ", STATION_SLUG, " - Current Conditions ===\n\n", sep="")

# Find station
stn <- get_json("/stations", query = list(`$limit` = 500))
if ("data" %in% names(stn)) stations_df <- as_tibble(stn$data) else stations_df <- as_tibble(stn)

station <- stations_df %>% filter(slug == STATION_SLUG)
if (nrow(station) == 0) stop("Station not found")

station_id <- station$`_id`[1]

# Display station info
if ("geo" %in% names(station)) {
  geo <- station$geo[[1]]
  if (!is.null(geo$coordinates)) {
    cat(sprintf("Location: %.5f°, %.5f°\n", geo$coordinates[2], geo$coordinates[1]))
  }
}
cat(sprintf("Elevation: %.1f m\n\n", ELEVATION_M))

# Get all datastreams
ds <- get_json("/datastreams", query = list(station_id = station_id, `$limit` = 200))
if ("data" %in% names(ds)) datastreams_df <- as_tibble(ds$data) else datastreams_df <- as_tibble(ds)

# ============================================================
# GET LATEST VALUE FOR EACH DASHBOARD DATASTREAM
# ============================================================

dashboard_data <- list()

for (key in names(DASHBOARD_DATASTREAMS)) {
  info <- DASHBOARD_DATASTREAMS[[key]]
  
  # Find matching datastream
  matching <- datastreams_df %>%
    filter(str_detect(name, regex(info$pattern, ignore_case = TRUE)))
  
  if (nrow(matching) > 0) {
    datastream_id <- matching$`_id`[1]
    datastream_name <- matching$name[1]
    
    # Get latest datapoint
    dp <- tryCatch({
      get_json("/datapoints", query = list(
        datastream_id = datastream_id,
        `$limit` = 1,
        `$sort[time]` = -1
      ))
    }, error = function(e) NULL)
    
    if (!is.null(dp)) {
      if ("data" %in% names(dp)) datapoints <- dp$data else datapoints <- dp
      
      if (length(datapoints) > 0 && nrow(as.data.frame(datapoints)) > 0) {
        dp_df <- as_tibble(datapoints)
        
        dashboard_data[[key]] <- tibble(
          measurement = info$label,
          value = dp_df$v[1],
          units = info$units,
          datastream_source = datastream_name,
          timestamp_utc = as.POSIXct(dp_df$t[1], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
        )
      }
    }
  }
}

dashboard_df <- bind_rows(dashboard_data)

# ============================================================
# GET RAINFALL DATA (Today, Yesterday, WY to date)
# ============================================================

# Find rainfall cumulative datastream
rainfall_ds <- datastreams_df %>%
  filter(str_detect(name, regex("Rainfall.*Cumulative", ignore_case = TRUE)))

if (nrow(rainfall_ds) > 0) {
  datastream_id <- rainfall_ds$`_id`[1]
  
  # Get last 200 points to calculate daily totals
  dp <- get_json("/datapoints", query = list(
    datastream_id = datastream_id,
    `$limit` = 200,
    `$sort[time]` = -1
  ))
  
  if ("data" %in% names(dp)) rainfall_data <- as_tibble(dp$data) else rainfall_data <- as_tibble(dp)
  
  if (nrow(rainfall_data) > 0) {
    rainfall_data <- rainfall_data %>%
      mutate(timestamp = as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
             date = as.Date(timestamp))
    
    # Get latest value (WY to date / current cumulative)
    latest_rainfall <- rainfall_data$v[1]
    latest_time <- rainfall_data$timestamp[1]
    
    # Today's rainfall (current value)
    today_rainfall <- rainfall_data %>%
      filter(date == as.Date(latest_time)) %>%
      summarise(value = first(v)) %>%
      pull(value)
    
    # Yesterday's rainfall
    yesterday_rainfall <- rainfall_data %>%
      filter(date == as.Date(latest_time) - 1) %>%
      summarise(value = first(v)) %>%
      pull(value)
    
    # Add to dashboard
    dashboard_df <- bind_rows(
      dashboard_df,
      tibble(
        measurement = c("Today's rainfall", "Yesterday's rainfall", "WY precipitation to date"),
        value = c(today_rainfall, yesterday_rainfall, latest_rainfall),
        units = "mm",
        datastream_source = rainfall_ds$name[1],
        timestamp_utc = latest_time
      )
    )
  }
}

# ============================================================
# CALCULATE DERIVED VALUES
# ============================================================

# Mean sea-level pressure (simplified barometric formula)
baro_row <- which(dashboard_df$measurement == "Barometric pressure")
if (length(baro_row) > 0) {
  station_pressure <- dashboard_df$value[baro_row]
  
  # Simplified formula: P0 = P * exp(h / H) where H ≈ 8400m
  msl_pressure <- station_pressure * exp(ELEVATION_M / 8400)
  
  # Insert after barometric pressure
  dashboard_df <- dashboard_df %>%
    add_row(
      measurement = "Mean sea-level pressure",
      value = msl_pressure,
      units = "hPa",
      datastream_source = "Calculated from barometric pressure",
      timestamp_utc = dashboard_df$timestamp_utc[baro_row],
      .after = baro_row
    )
}

# ============================================================
# DISPLAY RESULTS
# ============================================================

cat("Current Conditions (as of ", format(max(dashboard_df$timestamp_utc), "%Y-%m-%d %H:%M UTC"), ")\n", sep="")
cat(strrep("=", 80), "\n\n")

for (i in 1:nrow(dashboard_df)) {
  cat(sprintf("%-30s %10.2f %-10s [%s]\n",
              dashboard_df$measurement[i],
              dashboard_df$value[i],
              dashboard_df$units[i],
              dashboard_df$datastream_source[i]))
}

cat("\n")

# ============================================================
# SAVE TO CSV
# ============================================================

output_file <- paste0("dendra_", STATION_SLUG, "_dashboard.csv")
write_csv(dashboard_df, output_file)
cat("Dashboard data saved to:", output_file, "\n")

# ============================================================
# CREATE SUMMARY DISPLAY (like website)
# ============================================================

cat("\n=== SUMMARY (Website Format) ===\n\n")

summary_items <- c(
  "Battery voltage", "Air temperature", "Relative humidity", 
  "Barometric pressure", "Mean sea-level pressure",
  "PAR", "Total solar",
  "Today's rainfall", "Yesterday's rainfall", "WY precipitation to date",
  "Wind speed", "Wind direction"
)

for (item in summary_items) {
  row <- dashboard_df %>% filter(measurement == item)
  if (nrow(row) > 0) {
    timestamp_local <- format(row$timestamp_utc[1] - hours(8), "%Y-%m-%d %H:%M")  # PST = UTC-8
    cat(sprintf("%-30s %8.2f\t%-10s\t%s\t%s\n",
                row$measurement[1],
                row$value[1],
                row$units[1],
                row$datastream_source[1],
                timestamp_local))
  }
}

cat("\n=== DONE ===\n")

# Return the dashboard data
invisible(dashboard_df)