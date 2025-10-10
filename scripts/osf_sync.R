# scripts/osf_sync.R
# Upload selected outputs/reports to OSF.
# Requires environment variables:
# - OSF_TOKEN (Personal Access Token)
# - OSF_PROJECT (Project ID, e.g., 'abc12')
# - OSF_DATA_COMPONENT (optional component name, default '01_Data')
# - OSF_REPORTS_COMPONENT (optional component name, default '05_Reports')

library(osfr)
library(glue)
library(fs)

token <- Sys.getenv("OSF_TOKEN")
project_id <- Sys.getenv("OSF_PROJECT")
data_component_name <- Sys.getenv("OSF_DATA_COMPONENT", "01_Data")
reports_component_name <- Sys.getenv("OSF_REPORTS_COMPONENT", "05_Reports")

stopifnot(nchar(token) > 0, nchar(project_id) > 0)

osf_auth(token = token)

proj <- osf_retrieve_node(project_id)

# Helper to get/create a component by name
get_or_create_component <- function(project, name) {
  comps <- osf_ls_nodes(project)
  hit <- comps[comps$name == name, ]
  if (nrow(hit) == 1) return(osf_retrieve_node(hit$id))
  osf_create_component(project, name)
}

data_comp <- get_or_create_component(proj, data_component_name)
reports_comp <- get_or_create_component(proj, reports_component_name)

# Upload reports (HTML, PDF, etc.) and figures
paths_to_upload <- c(
  dir("reports", recursive = TRUE, full.names = TRUE, pattern = "\\.(html|pdf|qmd|md)$"),
  dir("outputs/figures", recursive = TRUE, full.names = TRUE, pattern = "\\.(png|jpg|jpeg|svg)$"),
  dir("outputs/maps", recursive = TRUE, full.names = TRUE, pattern = "\\.(tif|tiff|png|jpg|jpeg|gpkg)$")
)

if (length(paths_to_upload) > 0) {
  message(glue("Uploading {length(paths_to_upload)} files to OSF reports component '{reports_component_name}'..."))
  osf_upload(reports_comp, paths_to_upload, conflicts = "overwrite")
} else {
  message("No report or output files found to upload.")
}

# Example: upload a processed dataset if present
processed <- dir("data/processed", full.names = TRUE, recursive = TRUE)
if (length(processed) > 0) {
  message(glue("Uploading {length(processed)} processed data files to OSF data component '{data_component_name}'..."))
  osf_upload(data_comp, processed, conflicts = "overwrite")
} else {
  message("No processed data files found to upload.")
}

message("OSF sync complete.")