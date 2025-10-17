# scripts/dendra_pull_multiple.R
# Download multiple datastreams from Barcroft station into a single dataframe

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

# --- Configuration ---
STATION_SLUG <- "whitemt-barcroft"
START_DATE <- "2025-10-03"  # Change as needed
END_DATE <- "2025-10-17"    # Change as needed

# Datastreams you want to download (search terms)
DESIRED_DATASTREAMS <- c(
  "Air Temp.*Avg",
  "Air Temp.*Max",
  "Air Temp.*Min",
  "Relative Humidity",
  "Wind Speed",
  "Solar Radiation",
  "Rainfall"
)

cat("=== Dendra Data Pull ===\n")
cat("Station:", STATION_SLUG, "\n")
cat("Date range:", START_DATE, "to", END_DATE, "\n\n")

# --- 1) Find station by slug ---
stn <- get_json("/stations", query = list(`$limit` = 500))

if ("data" %in% names(stn)) {
  stations_df <- as_tibble(stn$data)
} else {
  stations_df <- as_tibble(stn)
}

station <- stations_df %>%
  filter(slug == STATION_SLUG)

if (nrow(station) == 0) {
  stop("Station '", STATION_SLUG, "' not found")
}

station_id <- station$`_id`[1]
cat("Found station:", station$full_name[1], "\n")
cat("Station ID:", station_id, "\n\n")

# --- 2) Get datastreams ---
ds <- get_json("/datastreams", query = list(station_id = station_id, `$limit` = 200))

if ("data" %in% names(ds)) {
  datastreams_df <- as_tibble(ds$data)
} else {
  datastreams_df <- as_tibble(ds)
}

cat("Total datastreams available:", nrow(datastreams_df), "\n")

# Filter to desired datastreams
selected_datastreams <- datastreams_df %>%
  filter(sapply(name, function(nm) {
    any(sapply(DESIRED_DATASTREAMS, function(pattern) {
      str_detect(nm, regex(pattern, ignore_case = TRUE))
    }))
  }))

cat("Selected", nrow(selected_datastreams), "datastreams:\n")
print(selected_datastreams %>% select(`_id`, name))
cat("\n")

# Convert dates to timestamps (milliseconds since epoch)
start_time <- as.numeric(as.POSIXct(START_DATE, tz = "UTC")) * 1000
end_time <- as.numeric(as.POSIXct(paste(END_DATE, "23:59:59"), tz = "UTC")) * 1000

# --- 3) Download and combine all datastreams ---
all_data <- list()

for (i in 1:nrow(selected_datastreams)) {
  datastream_id <- selected_datastreams$`_id`[i]
  datastream_name <- selected_datastreams$name[i]
  
  cat("[", i, "/", nrow(selected_datastreams), "] Downloading:", datastream_name, "\n", sep="")
  
  # Query with time range
  dp <- get_json("/datapoints", query = list(
    datastream_id = datastream_id,
    `time[$gte]` = start_time,
    `time[$lte]` = end_time,
    `$limit` = 10000,
    `$sort[time]` = 1  # ascending
  ))
  
  if ("data" %in% names(dp)) {
    datapoints <- as_tibble(dp$data)
  } else {
    datapoints <- as_tibble(dp)
  }
  
  cat("  Retrieved", nrow(datapoints), "datapoints\n")
  
  if (nrow(datapoints) > 0) {
    # Parse timestamp from 't' field (ISO string)
    datapoints <- datapoints %>%
      mutate(
        timestamp = as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        datastream = datastream_name
      ) %>%
      select(timestamp, value = v, datastream)
    
    all_data[[datastream_name]] <- datapoints
  }
}

# --- 4) Combine into wide format ---
if (length(all_data) > 0) {
  # Stack all datastreams
  combined_long <- bind_rows(all_data)
  
  cat("\n=== Creating wide format dataframe ===\n")
  
  # Pivot to wide format (one column per datastream)
  combined_wide <- combined_long %>%
    pivot_wider(
      names_from = datastream,
      values_from = value,
      values_fn = mean  # If multiple values at same timestamp, take mean
    ) %>%
    arrange(timestamp)
  
  cat("Final dataframe: ", nrow(combined_wide), " rows × ", ncol(combined_wide), " columns\n\n")
  
  # Clean column names
  names(combined_wide) <- gsub(" ", "_", names(combined_wide))
  names(combined_wide) <- gsub("[^A-Za-z0-9_]", "", names(combined_wide))
  
  # --- 5) Save outputs ---
  
  # Save wide format CSV
  output_file_wide <- paste0("dendra_", STATION_SLUG, "_", START_DATE, "_to_", END_DATE, "_wide.csv")
  write_csv(combined_wide, output_file_wide)
  cat("Saved wide format to:", output_file_wide, "\n")
  
  # Save long format CSV
  output_file_long <- paste0("dendra_", STATION_SLUG, "_", START_DATE, "_to_", END_DATE, "_long.csv")
  write_csv(combined_long, output_file_long)
  cat("Saved long format to:", output_file_long, "\n")
  
  # --- 6) Display summary ---
  cat("\n=== Data Summary ===\n")
  cat("Date range:", min(combined_wide$timestamp), "to", max(combined_wide$timestamp), "\n")
  cat("Number of observations:", nrow(combined_wide), "\n")
  cat("\nColumns:\n")
  print(names(combined_wide))
  
  cat("\n=== Preview (first 10 rows) ===\n")
  print(head(combined_wide, 10))
  
  cat("\n=== Summary Statistics ===\n")
  print(summary(combined_wide))
  
  cat("\n=== SUCCESS ===\n")
  cat("Wide format:", output_file_wide, "\n")
  cat("Long format:", output_file_long, "\n")
  
  # Return the dataframe for further analysis
  invisible(combined_wide)
  
} else {
  cat("\nNo data retrieved.\n")
}