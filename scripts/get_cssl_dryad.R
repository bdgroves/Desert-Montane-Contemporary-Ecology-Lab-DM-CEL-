# scripts/get_cssl_dryad.R
# CSSL Dryad (DOI: 10.6078/D1941T) robust downloader:
# - keeps session cookies
# - scrapes /downloads/file_stream/ links
# - downloads with same cookie handle + Referer (avoids 403)
# - falls back to archive if per-file CSVs aren’t exposed
# - combines to data/processed/cssl_historic_combined.csv

quiet_install <- function(pkgs){
  need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
}
quiet_install(c("rvest","httr","xml2","stringr","purrr","dplyr","readr"))

suppressPackageStartupMessages({
  library(rvest); library(httr); library(xml2)
  library(stringr); library(purrr); library(dplyr); library(readr)
})

dataset_url <- "https://datadryad.org/dataset/doi:10.6078/D1941T"
raw_dir  <- "data/raw/cssl_dryad"
proc_dir <- "data/processed"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

ua   <- httr::user_agent("CSSL-fetch/1.0 (R; Windows)")
sess <- rvest::session(dataset_url, httr_config = ua)

to_abs <- function(href) if (str_starts(href, "http")) href else paste0("https://datadryad.org", href)

# Download using the SAME cookie handle as the page session.
download_with_handle <- function(sess, url, dest, referer){
  for (i in 1:4) {
    resp <- try(
      httr::GET(
        url,
        handle = sess$handle,                         # <-- reuse cookies
        httr::add_headers(
          Referer = referer,
          `Accept` = "*/*",
          `Accept-Language` = "en-US,en;q=0.8"
        ),
        httr::write_disk(dest, overwrite = TRUE),     # stream to file
        httr::timeout(120),
        ua
      ),
      silent = TRUE
    )
    if (!inherits(resp, "try-error") && httr::status_code(resp) == 200L) return(dest)
    Sys.sleep(1 + i)
  }
  NA_character_
}

# Sniff & extract archives (ZIP or tar.*)
smart_extract <- function(archive_path, out_dir){
  con <- file(archive_path, "rb"); on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 6)
  is_zip <- length(sig) >= 2 && as.integer(sig[1:2]) == c(0x50, 0x4B) # PK
  is_gz  <- length(sig) >= 2 && as.integer(sig[1:2]) == c(0x1F, 0x8B) # gzip
  
  ok <- FALSE
  if (is_zip) {
    ok <- tryCatch({ utils::unzip(archive_path, exdir = out_dir); TRUE }, error = function(e) FALSE)
  }
  if (!ok) {
    ok <- tryCatch({ utils::untar(archive_path, exdir = out_dir, compressed = is_gz); TRUE }, error = function(e) FALSE)
  }
  if (!ok) stop("Could not extract archive: ", archive_path)
}

# ---- 1) Scrape all anchors and keep file_stream links ----
html <- xml2::read_html(sess$response)
hrefs <- html %>% html_elements("a[href]") %>% html_attr("href") %>% unique() %>% {\(x) x[!is.na(x)]}()

file_links <- hrefs[str_detect(hrefs, "/downloads/file_stream/|/stash/downloads/file_stream/")]
file_urls  <- to_abs(file_links)

# Heuristic: prioritize CSV-looking links
csv_mask <- str_detect(tolower(file_urls), "\\.csv($|\\?)") | str_detect(file_urls, "WY\\d{4}\\.csv")
csv_urls  <- file_urls[csv_mask]

downloaded <- character(0)

if (length(csv_urls)) {
  message("Found ", length(csv_urls), " CSV-like file_stream link(s). Downloading…")
  csv_names <- basename(csv_urls) %>% str_replace("\\?.*$","")
  csv_names <- ifelse(
    str_detect(csv_names, "\\.csv$"),
    csv_names,
    ifelse(str_detect(csv_urls, "WY\\d{4}"), str_extract(csv_urls, "WY\\d{4}\\.csv"), paste0(csv_names, ".csv"))
  )
  for (i in seq_along(csv_urls)) {
    u <- csv_urls[i]; nm <- csv_names[i]
    dest <- file.path(raw_dir, nm)
    got  <- download_with_handle(sess, u, dest, dataset_url)
    if (!is.na(got) && file.exists(got)) downloaded <- c(downloaded, got)
  }
}

# ---- 2) Fallback: fetch the first file_stream (often the full dataset archive) ----
if (!length(downloaded)) {
  if (!length(file_urls)) stop("No downloadable file_stream links found on the page.")
  arch_url  <- file_urls[1]
  arch_dest <- file.path(raw_dir, "dryad_dataset.bin")
  message("No per-file CSV links visible; downloading archive candidate…")
  got <- download_with_handle(sess, arch_url, arch_dest, dataset_url)
  if (is.na(got) || !file.exists(got)) stop("Failed to fetch any dataset file from Dryad.")
  message("Extracting archive (auto-detect)…")
  smart_extract(arch_dest, raw_dir)
}

# ---- 3) Combine CSVs ----
all_files <- list.files(raw_dir, recursive = TRUE, full.names = TRUE)
message("Found ", length(all_files), " file(s) under ", normalizePath(raw_dir, winslash = "/"))
if (length(all_files)) {
  cat("Sample files:\n - ", paste(basename(head(all_files, 12)), collapse = "\n - "), "\n", sep = "")
}

csvs <- list.files(raw_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
if (!length(csvs))
  stop("No CSV files found under ", normalizePath(raw_dir, winslash = "/"),
       ". Re-run later; Dryad occasionally throttles per-file downloads.")

message("Combining ", length(csvs), " CSV(s)…")
read_safe <- function(p){
  tryCatch(readr::read_csv(p, show_col_types = FALSE) %>% mutate(.source_file = basename(p)),
           error = function(e){ message("Skip: ", basename(p), " (", e$message, ")"); NULL })
}
cssl_hist <- csvs %>% map(read_safe) %>% compact() %>% bind_rows()

# Standardize Date if present
date_col <- intersect(names(cssl_hist), c("Date","date","DATE","date_time","datetime"))
if (length(date_col)){
  cssl_hist <- cssl_hist %>% mutate(Date = as.Date(.data[[date_col[1]]])) %>% relocate(Date, .before = 1)
}

out_csv <- file.path(proc_dir, "cssl_historic_combined.csv")
readr::write_csv(cssl_hist, out_csv)
message("Wrote combined CSV: ", normalizePath(out_csv, winslash = "/"),
        " (rows=", nrow(cssl_hist), ", cols=", ncol(cssl_hist), ")")
