# scripts/osf_sync.R
# Upload selected outputs/reports to OSF.
# Reads credentials from env vars:
# - OSF_TOKEN (preferred for CI) or OSF_PAT (local osfr default)
# - OSF_PROJECT (required, e.g., "evr38")
# - OSF_DATA_COMPONENT (optional; default "01_Data")
# - OSF_REPORTS_COMPONENT (optional; default "05_Reports")

suppressPackageStartupMessages({
  library(osfr)
  library(glue)
  library(fs)
})

# -------- Auth / Config --------
# Prefer OSF_TOKEN (CI). If missing, fall back to OSF_PAT (local).
token <- Sys.getenv("OSF_TOKEN", unset = Sys.getenv("OSF_PAT"))
project_id <- Sys.getenv("OSF_PROJECT")
data_component_name <- Sys.getenv("OSF_DATA_COMPONENT", "01_Data")
reports_component_name <- Sys.getenv("OSF_REPORTS_COMPONENT", "05_Reports")

if (!nzchar(token)) stop("No OSF token found. Set OSF_TOKEN (CI) or OSF_PAT (local).")
if (!nzchar(project_id)) stop("No OSF project id found. Set OSF_PROJECT (e.g., 'evr38').")

message(glue("Authenticating to OSF project '{project_id}'..."))
osf_auth(token = token)

# -------- Small retry helper for transient errors (e.g., 5xx) --------
retry <- function(expr, tries = 4, wait = 2) {
  for (i in seq_len(tries)) {
    out <- try(eval.parent(substitute(expr)), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    msg <- conditionMessage(attr(out, "condition"))
    if (i < tries) {
      message(glue("Retry {i}/{tries} after error: {msg}\nWaiting {wait}s..."))
      Sys.sleep(wait); wait <- min(wait * 2, 30)
    } else stop(out)
  }
}

# -------- Project & components --------
proj <- retry(osf_retrieve_node(project_id))

get_or_create_component <- function(project, name) {
  comps <- retry(osf_ls_nodes(project))
  hit <- comps[comps$name == name, , drop = FALSE]
  if (nrow(hit) == 1) return(osf_retrieve_node(hit$id))
  message(glue("Component '{name}' not found. Creating it..."))
  retry(osf_create_component(project, name))
}

data_comp <- get_or_create_component(proj, data_component_name)
reports_comp <- get_or_create_component(proj, reports_component_name)

message(glue("Using components: data = '{data_component_name}', reports = '{reports_component_name}'"))

# -------- Collect files to upload --------
report_files <- c(
  dir("reports", recursive = TRUE, full.names = TRUE, pattern = "\\.(html|pdf|qmd|md)$"),
  dir("outputs/figures", recursive = TRUE, full.names = TRUE, pattern = "\\.(png|jpg|jpeg|svg)$"),
  dir("outputs/maps", recursive = TRUE, full.names = TRUE, pattern = "\\.(tif|tiff|png|jpg|jpeg|gpkg)$")
)

processed_files <- dir("data/processed", recursive = TRUE, full.names = TRUE)

# -------- Upload --------
uploaded_any <- FALSE

if (length(report_files)) {
  message(glue("Uploading {length(report_files)} file(s) to '{reports_component_name}'..."))
  retry(osf_upload(reports_comp, report_files, conflicts = "overwrite"))
  uploaded_any <- TRUE
} else {
  message("No report/output files found to upload (reports/, outputs/figures, outputs/maps).")
}

if (length(processed_files)) {
  message(glue("Uploading {length(processed_files)} processed data file(s) to '{data_component_name}'..."))
  retry(osf_upload(data_comp, processed_files, conflicts = "overwrite"))
  uploaded_any <- TRUE
} else {
  message("No processed data files found in data/processed/.")
}

if (uploaded_any) {
  message("OSF sync complete ✅")
} else {
  message("Nothing to upload. OSF sync finished with no changes.")
}
