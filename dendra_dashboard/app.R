# app.R
# Dendra Science Weather Station Dashboard - Production Version

# Load required packages (shinyapps.io will install automatically)
library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)
library(stringr)
library(shinyWidgets)

# ============================================================
# UNIT CONVERSION FUNCTIONS
# ============================================================

convert_value <- function(value, datastream_name, to_imperial = TRUE) {
  if (is.na(value) || is.null(value)) return(value)
  
  if (str_detect(datastream_name, regex("temp|Temp", ignore_case = FALSE))) {
    if (to_imperial) return(value * 9/5 + 32)
  }
  
  if (str_detect(datastream_name, regex("wind.*speed", ignore_case = TRUE))) {
    if (to_imperial) return(value * 2.23694)
  }
  
  if (str_detect(datastream_name, regex("pressure", ignore_case = TRUE))) {
    if (to_imperial) return(value * 0.02953)
  }
  
  if (str_detect(datastream_name, regex("rain|precip", ignore_case = TRUE))) {
    if (to_imperial) return(value * 0.0393701)
  }
  
  if (str_detect(datastream_name, regex("solar|radiation", ignore_case = TRUE))) {
    if (to_imperial) return(value * 0.316998)
  }
  
  return(value)
}

get_unit_label <- function(datastream_name, imperial = FALSE) {
  if (str_detect(datastream_name, regex("temp|Temp", ignore_case = FALSE))) {
    return(ifelse(imperial, "°F", "°C"))
  }
  
  if (str_detect(datastream_name, regex("wind.*speed", ignore_case = TRUE))) {
    return(ifelse(imperial, "mph", "m/s"))
  }
  
  if (str_detect(datastream_name, regex("pressure", ignore_case = TRUE))) {
    return(ifelse(imperial, "inHg", "hPa"))
  }
  
  if (str_detect(datastream_name, regex("rain|precip", ignore_case = TRUE))) {
    return(ifelse(imperial, "in", "mm"))
  }
  
  if (str_detect(datastream_name, regex("solar.*radiation", ignore_case = TRUE)) &&
      !str_detect(datastream_name, regex("PAR|Photosynthetically", ignore_case = TRUE))) {
    return(ifelse(imperial, "BTU/(hr·ft²)", "W/m²"))
  }
  
  if (str_detect(datastream_name, regex("humidity", ignore_case = TRUE))) {
    return("%")
  }
  
  if (str_detect(datastream_name, regex("PAR|Photosynthetically", ignore_case = TRUE))) {
    return("µmol/m²/s")
  }
  
  if (str_detect(datastream_name, regex("voltage|battery", ignore_case = TRUE))) {
    return("V")
  }
  
  if (str_detect(datastream_name, regex("wind.*direction", ignore_case = TRUE))) {
    return("°")
  }
  
  return("")
}

# ============================================================
# API FUNCTIONS
# ============================================================

BASE <- "https://api.dendra.science/v2"

get_json <- function(path, query=list()) {
  url <- paste0(BASE, path)
  r <- GET(url, query = query, timeout(60))
  stop_for_status(r)
  fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
}

get_all_stations <- function() {
  stn <- get_json("/stations", query = list(`$limit` = 500))
  if ("data" %in% names(stn)) {
    stations_df <- as_tibble(stn$data)
  } else {
    stations_df <- as_tibble(stn)
  }
  
  stations_df %>%
    filter(is_active == TRUE) %>%
    arrange(name)
}

get_current_conditions <- function(station_id) {
  ds <- get_json("/datastreams", query = list(station_id = station_id, `$limit` = 200))
  if ("data" %in% names(ds)) {
    datastreams_df <- as_tibble(ds$data)
  } else {
    datastreams_df <- as_tibble(ds)
  }
  
  current_list <- list()
  
  for (i in 1:nrow(datastreams_df)) {
    dp <- tryCatch({
      get_json("/datapoints", query = list(
        datastream_id = datastreams_df$`_id`[i],
        `$limit` = 1,
        `$sort[time]` = -1
      ))
    }, error = function(e) NULL)
    
    if (!is.null(dp)) {
      if ("data" %in% names(dp)) datapoints <- dp$data else datapoints <- dp
      
      if (length(datapoints) > 0 && nrow(as.data.frame(datapoints)) > 0) {
        dp_df <- as_tibble(datapoints)
        
        current_list[[i]] <- tibble(
          datastream = datastreams_df$name[i],
          value_metric = dp_df$v[1],
          timestamp_utc = as.POSIXct(dp_df$t[1], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          datastream_id = datastreams_df$`_id`[i]
        )
      }
    }
  }
  
  bind_rows(current_list)
}

get_historical_data <- function(datastream_id, days = 7) {
  end_time <- as.numeric(Sys.time()) * 1000
  start_time <- end_time - (days * 24 * 60 * 60 * 1000)
  
  dp <- get_json("/datapoints", query = list(
    datastream_id = datastream_id,
    `time[$gte]` = start_time,
    `time[$lte]` = end_time,
    `$limit` = 5000,
    `$sort[time]` = 1
  ))
  
  if ("data" %in% names(dp)) {
    datapoints <- as_tibble(dp$data)
  } else {
    datapoints <- as_tibble(dp)
  }
  
  if (nrow(datapoints) > 0) {
    datapoints %>%
      mutate(timestamp = as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
             value_metric = v) %>%
      select(timestamp, value_metric)
  } else {
    tibble(timestamp = as.POSIXct(character()), value_metric = numeric())
  }
}

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Dendra Weather",
    titleWidth = 300,
    tags$li(class = "dropdown",
            style = "padding: 15px; margin-right: 10px;",
            tags$div(
              style = "display: inline-block;",
              radioGroupButtons(
                inputId = "unit_toggle",
                label = NULL,
                choices = c(
                  `<i class='fa fa-flag-usa'></i> Imperial` = TRUE,
                  `<i class='fa fa-globe'></i> Metric` = FALSE
                ),
                selected = TRUE,
                justified = FALSE,
                status = "primary",
                size = "sm",
                individual = TRUE,
                checkIcon = list(
                  yes = icon("check")
                )
              )
            )
    )
  ),
  
  dashboardSidebar(
    width = 300,
    
    tags$div(
      style = "padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               color: white; text-align: center; margin-bottom: 10px;",
      h4(style = "margin: 5px 0;", icon("satellite-dish"), " Station Control"),
      p(style = "margin: 5px 0; font-size: 11px; opacity: 0.9;", 
        "Real-time weather data")
    ),
    
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", 
               icon = icon("tachometer-alt"),
               badgeLabel = "live", badgeColor = "green"),
      menuItem("Historical Data", tabName = "historical", 
               icon = icon("chart-line")),
      menuItem("Data Export", tabName = "export", 
               icon = icon("download")),
      menuItem("About", tabName = "about", 
               icon = icon("info-circle"))
    ),
    
    hr(style = "border-color: rgba(255,255,255,0.1);"),
    
    tags$div(
      style = "padding: 0 15px;",
      
      tags$label(
        style = "color: #b8c7ce; font-weight: 600; font-size: 13px; margin-bottom: 8px; display: block;",
        icon("search"), " SEARCH STATION"
      ),
      
      selectizeInput("station_select", 
                     label = NULL,
                     choices = NULL,
                     width = "100%"),
      
      uiOutput("selected_station_display"),
      
      tags$div(
        style = "padding: 8px 10px; font-size: 11px; color: #8aa4af; 
                 background-color: rgba(0,0,0,0.1); border-radius: 4px; margin-top: 8px;",
        icon("lightbulb"), " Tip: Type 'Barcroft' or 'White Mountain'"
      )
    ),
    
    hr(style = "border-color: rgba(255,255,255,0.1);"),
    
    uiOutput("unit_display_info"),
    
    hr(style = "border-color: rgba(255,255,255,0.1);"),
    
    tags$div(
      style = "padding: 0 15px;",
      actionButton("refresh_btn", 
                   label = span(icon("sync-alt"), " Refresh Data"),
                   class = "btn-block",
                   style = "background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); 
                            color: white; border: none; font-weight: 600; 
                            padding: 10px; border-radius: 5px;
                            box-shadow: 0 4px 6px rgba(0,0,0,0.1);")
    ),
    
    tags$div(
      style = "position: absolute; bottom: 10px; width: 100%; text-align: center; 
               padding: 10px; color: #8aa4af; font-size: 11px;",
      p(style = "margin: 0;", icon("code"), " Built with R Shiny"),
      p(style = "margin: 0;", "© 2024 Dendra Science")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$title("Dendra Weather Dashboard"),
      
      tags$style(HTML("
        /* Main color theme */
        .skin-blue .main-header .navbar {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }
        
        .skin-blue .main-header .logo {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          font-weight: 600;
        }
        
        .skin-blue .main-header .logo:hover {
          background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
        }
        
        /* Sidebar styling */
        .skin-blue .main-sidebar {
          background-color: #2c3e50;
        }
        
        .skin-blue .sidebar-menu > li.active > a {
          border-left-color: #43e97b;
          background: rgba(67, 233, 123, 0.1);
        }
        
        .skin-blue .sidebar-menu > li:hover > a {
          background: rgba(255, 255, 255, 0.05);
        }
        
        /* Value boxes */
        .info-box { 
          min-height: 100px; 
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
        .info-box-icon { 
          height: 100px; 
          line-height: 100px; 
          border-radius: 8px 0 0 8px;
        }
        .info-box-content { 
          padding-top: 10px; 
          padding-bottom: 10px; 
        }
        
        /* Small boxes */
        .small-box { 
          min-height: 120px; 
          border-radius: 8px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        
        /* Box styling */
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .box-header {
          border-radius: 8px 8px 0 0;
        }
        
        /* Selectize styling */
        .selectize-input {
          font-size: 14px;
          padding: 10px 12px;
          border-radius: 5px;
          border: 1px solid #34495e;
          background-color: #34495e;
          color: white;
          min-height: 42px;
        }
        
        .selectize-input.focus {
          border-color: #667eea;
          box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
        }
        
        .selectize-input .item {
          background-color: #667eea;
          color: white;
          padding: 4px 8px;
          border-radius: 3px;
          margin-right: 5px;
        }
        
        .selectize-input input {
          color: white !important;
        }
        
        .selectize-input input::placeholder {
          color: rgba(255,255,255,0.5);
        }
        
        .selectize-dropdown {
          font-size: 13px;
          max-height: 300px;
          border-radius: 5px;
          border-color: #34495e;
          background-color: #fff;
        }
        
        .selectize-dropdown-content {
          max-height: 280px;
        }
        
        .selectize-dropdown-content .option {
          padding: 8px 12px;
        }
        
        .selectize-dropdown .active {
          background-color: #667eea;
          color: white;
        }
        
        /* Radio button styling */
        .btn-group-toggle .btn {
          padding: 8px 16px;
          font-size: 13px;
          font-weight: 600;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
        }
        
        .btn-primary:hover {
          background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
        }
        
        .btn-primary.active {
          background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
          box-shadow: 0 4px 8px rgba(67, 233, 123, 0.3);
        }
        
        /* Download buttons */
        .btn-success {
          background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
          border: none;
          font-weight: 600;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #38f9d7 0%, #43e97b 100%);
          box-shadow: 0 6px 12px rgba(0,0,0,0.15);
        }
        
        /* Content wrapper */
        .content-wrapper {
          background-color: #ecf0f5;
        }
        
        /* Data table styling */
        .dataTables_wrapper {
          padding: 10px;
        }
        
        /* Plotly styling */
        .js-plotly-plot {
          border-radius: 8px;
        }
        
        /* Link styling */
        .location-link, .wiki-link {
          display: inline-block;
          margin: 5px 10px 5px 0;
          padding: 8px 12px;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white !important;
          text-decoration: none;
          border-radius: 5px;
          font-size: 13px;
          font-weight: 600;
          transition: all 0.3s ease;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        .location-link:hover, .wiki-link:hover {
          background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
          transform: translateY(-2px);
        }
        
        .selected-station-box {
          background-color: rgba(102, 126, 234, 0.1);
          border-left: 4px solid #667eea;
          padding: 10px;
          border-radius: 5px;
          margin-top: 10px;
          color: #b8c7ce;
          font-size: 13px;
        }
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = span(icon("map-marker-alt"), " ", textOutput("station_title", inline = TRUE)),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  htmlOutput("station_info_with_links"),
                  br(),
                  textOutput("last_update")
                )
              ),
              
              fluidRow(
                valueBoxOutput("temp_box", width = 3),
                valueBoxOutput("humidity_box", width = 3),
                valueBoxOutput("pressure_box", width = 3),
                valueBoxOutput("wind_box", width = 3)
              ),
              
              fluidRow(
                box(
                  title = span(icon("temperature-high"), " Temperature (Last 24 Hours)"),
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("temp_plot", height = 300)
                ),
                box(
                  title = span(icon("tint"), " Humidity (Last 24 Hours)"),
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("humidity_plot", height = 300)
                )
              ),
              
              fluidRow(
                box(
                  title = span(icon("table"), " All Current Measurements"),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  DTOutput("current_table")
                )
              )
      ),
      
      # Historical Data Tab
      tabItem(tabName = "historical",
              fluidRow(
                box(
                  title = span(icon("cog"), " Historical Data Settings"),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    column(4,
                           selectizeInput("hist_datastream", 
                                          "Select Datastream:",
                                          choices = NULL,
                                          options = list(
                                            placeholder = 'Search datastreams...'
                                          ))
                    ),
                    column(4,
                           selectInput("hist_days",
                                       "Time Period:",
                                       choices = c("Last 24 Hours" = 1,
                                                   "Last 3 Days" = 3,
                                                   "Last 7 Days" = 7,
                                                   "Last 14 Days" = 14,
                                                   "Last 30 Days" = 30),
                                       selected = 7)
                    ),
                    column(4,
                           br(),
                           actionButton("load_hist_btn", 
                                        span(icon("chart-line"), " Load Data"),
                                        class = "btn-success btn-block",
                                        style = "margin-top: 0;")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = span(icon("chart-area"), " Time Series Plot"),
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("historical_plot", height = 400)
                )
              ),
              
              fluidRow(
                box(
                  title = span(icon("calculator"), " Statistics"),
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE,
                  tableOutput("hist_stats")
                ),
                box(
                  title = span(icon("table"), " Data Table"),
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE,
                  DTOutput("hist_table")
                )
              )
      ),
      
      # Export Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = span(icon("file-download"), " Export Current Conditions"),
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  p("Download current conditions for the selected station."),
                  tags$div(
                    style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; 
                             border-left: 4px solid #17a2b8; margin-bottom: 15px;",
                    icon("info-circle"), 
                    strong(" Note: "), 
                    "Data will be exported in ",
                    tags$strong(textOutput("export_unit_system", inline = TRUE)),
                    " units."
                  ),
                  downloadButton("download_current", 
                                 span(icon("download"), " Download CSV"),
                                 class = "btn-success btn-block")
                ),
                box(
                  title = span(icon("file-download"), " Export Historical Data"),
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  selectizeInput("export_datastreams",
                                 "Select Datastreams:",
                                 choices = NULL,
                                 multiple = TRUE,
                                 options = list(
                                   placeholder = 'Search and select multiple...',
                                   plugins = list('remove_button')
                                 )),
                  selectInput("export_days",
                              "Time Period:",
                              choices = c("Last 7 Days" = 7,
                                          "Last 14 Days" = 14,
                                          "Last 30 Days" = 30,
                                          "Last 90 Days" = 90),
                              selected = 7),
                  tags$div(
                    style = "background-color: #d1ecf1; padding: 10px; border-radius: 5px; 
                             border-left: 4px solid #17a2b8; margin-bottom: 15px;",
                    icon("info-circle"),
                    strong(" Note: "),
                    "Data will be exported in ",
                    tags$strong(textOutput("export_unit_system_hist", inline = TRUE)),
                    " units."
                  ),
                  downloadButton("download_historical",
                                 span(icon("download"), " Download CSV"),
                                 class = "btn-success btn-block")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = span(icon("info-circle"), " About Dendra Science Dashboard"),
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  
                  tags$div(
                    style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                             padding: 30px; border-radius: 8px; color: white; text-align: center;
                             margin-bottom: 20px;",
                    h2(style = "margin: 0; font-weight: 600;", 
                       icon("cloud-sun"), " Dendra Weather Dashboard"),
                    p(style = "margin: 10px 0 0 0; font-size: 16px; opacity: 0.9;",
                      "Real-time weather data from research stations worldwide")
                  ),
                  
                  h4(icon("star"), " Features:"),
                  tags$div(
                    style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); 
                             gap: 15px; margin-bottom: 20px;",
                    
                    tags$div(
                      style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px;
                               border-left: 4px solid #667eea;",
                      h5(icon("search"), " Smart Search"),
                      p("Type-ahead station finder with instant filtering")
                    ),
                    tags$div(
                      style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px;
                               border-left: 4px solid #43e97b;",
                      h5(icon("ruler"), " Unit Conversion"),
                      p("Toggle between Imperial and Metric units")
                    ),
                    tags$div(
                      style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px;
                               border-left: 4px solid #f093fb;",
                      h5(icon("map-marker-alt"), " Location Links"),
                      p("View station locations on Google Maps")
                    ),
                    tags$div(
                      style = "padding: 15px; background-color: #f8f9fa; border-radius: 8px;
                               border-left: 4px solid #fad961;",
                      h5(icon("download"), " Data Export"),
                      p("Download CSV files in your preferred units")
                    )
                  ),
                  
                  hr(),
                  
                  h4(icon("exchange-alt"), " Unit Conversions:"),
                  tags$ul(
                    style = "columns: 2; -webkit-columns: 2; -moz-columns: 2;",
                    tags$li("Temperature: °C ⟷ °F"),
                    tags$li("Wind Speed: m/s ⟷ mph"),
                    tags$li("Pressure: hPa ⟷ inHg"),
                    tags$li("Rainfall: mm ⟷ inches"),
                    tags$li("Solar Radiation: W/m² ⟷ BTU/(hr·ft²)")
                  ),
                  
                  hr(),
                  
                  h4(icon("question-circle"), " How to Use:"),
                  tags$ol(
                    tags$li("Use the ", tags$strong("Imperial/Metric toggle"), " in the top-right"),
                    tags$li("Search for your station in the ", tags$strong("sidebar")),
                    tags$li("Click ", tags$strong("location links"), " to view on Google Maps"),
                    tags$li("View ", tags$strong("live conditions"), " on the Dashboard"),
                    tags$li("Explore ", tags$strong("historical trends"), " with custom date ranges"),
                    tags$li("Export data in your ", tags$strong("preferred format"))
                  ),
                  
                  hr(),
                  
                  tags$div(
                    style = "background-color: #e7f3ff; padding: 20px; border-radius: 8px;
                             border-left: 4px solid #2196F3;",
                    h4(icon("database"), " Data Source"),
                    p("Data is provided by ",
                      tags$a(href = "https://dendra.science", 
                             "Dendra Science", 
                             target = "_blank",
                             style = "color: #667eea; font-weight: 600;"),
                      " via their public API."),
                    p(tags$code("https://api.dendra.science/v2"))
                  ),
                  
                  hr(),
                  
                  tags$div(
                    style = "text-align: center; color: #6c757d;",
                    p(icon("code"), " Built with R Shiny & shinydashboard"),
                    p("© 2024 Dendra Science • Last updated: ", Sys.Date())
                  )
                )
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    stations = NULL,
    current_data = NULL,
    selected_station = NULL,
    datastreams = NULL,
    historical_data = NULL,
    historical_datastream_name = NULL
  )
  
  use_imperial <- reactive({
    as.logical(input$unit_toggle)
  })
  
  output$selected_station_display <- renderUI({
    req(rv$selected_station)
    
    tags$div(
      class = "selected-station-box",
      icon("check-circle"), " Selected: ",
      tags$strong(rv$selected_station$name)
    )
  })
  
  output$unit_display_info <- renderUI({
    tags$div(
      style = "padding: 12px 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
               border-radius: 8px; margin: 0 15px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        tags$div(
          icon("ruler", style = "font-size: 20px; margin-right: 10px;"),
          tags$span(style = "font-weight: 600; font-size: 13px;", "CURRENT UNITS")
        ),
        tags$div(
          style = "background-color: rgba(255,255,255,0.2); padding: 5px 12px; 
                   border-radius: 15px; font-size: 12px; font-weight: 600;",
          if(use_imperial()) {
            span(icon("flag-usa"), " Imperial")
          } else {
            span(icon("globe"), " Metric")
          }
        )
      ),
      tags$div(
        style = "margin-top: 8px; font-size: 11px; opacity: 0.9;",
        if(use_imperial()) {
          "°F • mph • inHg • inches"
        } else {
          "°C • m/s • hPa • mm"
        }
      )
    )
  })
  
  output$export_unit_system <- renderText({
    if(use_imperial()) "Imperial" else "Metric"
  })
  
  output$export_unit_system_hist <- renderText({
    if(use_imperial()) "Imperial" else "Metric"
  })
  
  observe({
    withProgress(message = 'Loading stations...', {
      rv$stations <- get_all_stations()
      
      station_choices <- setNames(
        rv$stations$`_id`, 
        paste0(rv$stations$name, 
               ifelse(!is.na(rv$stations$full_name) & rv$stations$full_name != rv$stations$name,
                      paste0(" - ", rv$stations$full_name),
                      ""))
      )
      
      updateSelectizeInput(
        session, 
        "station_select", 
        choices = station_choices,
        server = FALSE
      )
    })
  })
  
  observeEvent(input$station_select, {
    req(input$station_select)
    
    withProgress(message = 'Loading station data...', {
      
      rv$selected_station <- rv$stations %>%
        filter(`_id` == input$station_select)
      
      incProgress(0.5, detail = "Fetching current conditions...")
      rv$current_data <- get_current_conditions(input$station_select)
      
      if (nrow(rv$current_data) > 0) {
        datastream_choices <- setNames(
          rv$current_data$datastream_id, 
          rv$current_data$datastream
        )
        
        updateSelectizeInput(session, "hist_datastream", 
                             choices = datastream_choices)
        updateSelectizeInput(session, "export_datastreams", 
                             choices = datastream_choices)
      }
    })
  }, ignoreNULL = TRUE)
  
  observeEvent(input$refresh_btn, {
    req(input$station_select)
    
    withProgress(message = 'Refreshing data...', {
      rv$current_data <- get_current_conditions(input$station_select)
    })
  })
  
  current_data_converted <- reactive({
    req(rv$current_data)
    
    rv$current_data %>%
      mutate(
        value = mapply(convert_value, value_metric, datastream, 
                       MoreArgs = list(to_imperial = use_imperial())),
        units = sapply(datastream, get_unit_label, imperial = use_imperial())
      )
  })
  
  output$station_title <- renderText({
    req(rv$selected_station)
    rv$selected_station$full_name
  })
  
  output$station_info_with_links <- renderUI({
    req(rv$selected_station)
    
    info_parts <- list()
    
    if ("geo" %in% names(rv$selected_station)) {
      geo <- rv$selected_station$geo[[1]]
      if (!is.null(geo$coordinates)) {
        lat <- geo$coordinates[2]
        lon <- geo$coordinates[1]
        
        google_maps_url <- sprintf("https://www.google.com/maps?q=%.5f,%.5f", lat, lon)
        
        info_parts[[length(info_parts) + 1]] <- tags$a(
          href = google_maps_url,
          target = "_blank",
          class = "location-link",
          icon("map-marked-alt"),
          sprintf(" View on Map (%.5f°, %.5f°)", lat, lon)
        )
      }
    }
    
    if ("full_name" %in% names(rv$selected_station)) {
      station_name <- rv$selected_station$full_name
      search_name <- str_replace_all(station_name, "Weather Station|Station|Field|Research", "")
      search_name <- str_trim(search_name)
      wiki_url <- paste0("https://en.wikipedia.org/wiki/Special:Search?search=", 
                         URLencode(search_name))
      
      info_parts[[length(info_parts) + 1]] <- tags$a(
        href = wiki_url,
        target = "_blank",
        class = "wiki-link",
        icon("wikipedia-w"),
        " Search Wikipedia"
      )
    }
    
    if ("time_zone" %in% names(rv$selected_station)) {
      info_parts[[length(info_parts) + 1]] <- tags$span(
        style = "display: block; margin-top: 10px;",
        icon("clock"), " Time Zone: ", tags$strong(rv$selected_station$time_zone)
      )
    }
    
    if ("slug" %in% names(rv$selected_station)) {
      info_parts[[length(info_parts) + 1]] <- tags$span(
        style = "display: block; margin-top: 5px;",
        icon("tag"), " Slug: ", tags$code(rv$selected_station$slug)
      )
    }
    
    tags$div(info_parts)
  })
  
  output$last_update <- renderText({
    req(current_data_converted())
    paste("🔄 Last updated:", format(max(current_data_converted()$timestamp_utc), "%Y-%m-%d %H:%M UTC"))
  })
  
  output$temp_box <- renderValueBox({
    req(current_data_converted())
    
    temp_data <- current_data_converted() %>%
      filter(str_detect(datastream, regex("Air Temp.*Avg", ignore_case = TRUE)))
    
    if (nrow(temp_data) > 0) {
      unit_label <- get_unit_label("Air Temp Avg", use_imperial())
      valueBox(
        sprintf("%.1f%s", temp_data$value[1], unit_label),
        "Temperature",
        icon = icon("temperature-high"),
        color = "red"
      )
    } else {
      valueBox("N/A", "Temperature", icon = icon("temperature-high"), color = "red")
    }
  })
  
  output$humidity_box <- renderValueBox({
    req(current_data_converted())
    
    hum_data <- current_data_converted() %>%
      filter(str_detect(datastream, regex("Humidity.*Avg", ignore_case = TRUE)))
    
    if (nrow(hum_data) > 0) {
      valueBox(
        sprintf("%.1f%%", hum_data$value[1]),
        "Humidity",
        icon = icon("tint"),
        color = "blue"
      )
    } else {
      valueBox("N/A", "Humidity", icon = icon("tint"), color = "blue")
    }
  })
  
  output$pressure_box <- renderValueBox({
    req(current_data_converted())
    
    press_data <- current_data_converted() %>%
      filter(str_detect(datastream, regex("Barometric.*Avg", ignore_case = TRUE)))
    
    if (nrow(press_data) > 0) {
      unit_label <- get_unit_label("Barometric Pressure Avg", use_imperial())
      valueBox(
        sprintf("%.2f %s", press_data$value[1], unit_label),
        "Pressure",
        icon = icon("gauge"),
        color = "green"
      )
    } else {
      valueBox("N/A", "Pressure", icon = icon("gauge"), color = "green")
    }
  })
  
  output$wind_box <- renderValueBox({
    req(current_data_converted())
    
    wind_data <- current_data_converted() %>%
      filter(str_detect(datastream, regex("Wind Speed.*Avg", ignore_case = TRUE)))
    
    if (nrow(wind_data) > 0) {
      unit_label <- get_unit_label("Wind Speed Avg", use_imperial())
      valueBox(
        sprintf("%.1f %s", wind_data$value[1], unit_label),
        "Wind Speed",
        icon = icon("wind"),
        color = "yellow"
      )
    } else {
      valueBox("N/A", "Wind Speed", icon = icon("wind"), color = "yellow")
    }
  })
  
  output$temp_plot <- renderPlotly({
    req(rv$current_data)
    
    temp_ds <- rv$current_data %>%
      filter(str_detect(datastream, regex("Air Temp.*Avg", ignore_case = TRUE)))
    
    if (nrow(temp_ds) > 0) {
      hist_data <- get_historical_data(temp_ds$datastream_id[1], days = 1)
      
      if (nrow(hist_data) > 0) {
        hist_data <- hist_data %>%
          mutate(value = convert_value(value_metric, temp_ds$datastream[1], use_imperial()))
        
        unit_label <- get_unit_label("Air Temp Avg", use_imperial())
        
        plot_ly(hist_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(255, 99, 71)', width = 3)) %>%
          layout(xaxis = list(title = "Time"),
                 yaxis = list(title = paste("Temperature (", unit_label, ")")),
                 hovermode = 'x unified',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)')
      } else {
        plot_ly() %>% layout(title = "No data available")
      }
    } else {
      plot_ly() %>% layout(title = "No temperature data available")
    }
  })
  
  output$humidity_plot <- renderPlotly({
    req(rv$current_data)
    
    hum_ds <- rv$current_data %>%
      filter(str_detect(datastream, regex("Humidity.*Avg", ignore_case = TRUE)))
    
    if (nrow(hum_ds) > 0) {
      hist_data <- get_historical_data(hum_ds$datastream_id[1], days = 1)
      
      if (nrow(hist_data) > 0) {
        hist_data <- hist_data %>%
          mutate(value = value_metric)
        
        plot_ly(hist_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(65, 105, 225)', width = 3)) %>%
          layout(xaxis = list(title = "Time"),
                 yaxis = list(title = "Humidity (%)"),
                 hovermode = 'x unified',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 paper_bgcolor = 'rgba(0,0,0,0)')
      } else {
        plot_ly() %>% layout(title = "No data available")
      }
    } else {
      plot_ly() %>% layout(title = "No humidity data available")
    }
  })
  
  output$current_table <- renderDT({
    req(current_data_converted())
    
    current_data_converted() %>%
      mutate(timestamp_local = format(timestamp_utc, "%Y-%m-%d %H:%M UTC"),
             value_display = sprintf("%.2f", value)) %>%
      select(Datastream = datastream, 
             Value = value_display,
             Units = units,
             Timestamp = timestamp_local) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE)
  })
  
  observeEvent(input$load_hist_btn, {
    req(input$hist_datastream, input$hist_days)
    
    withProgress(message = 'Loading historical data...', {
      rv$historical_data <- get_historical_data(input$hist_datastream, 
                                                as.numeric(input$hist_days))
      
      ds_match <- rv$current_data %>% filter(datastream_id == input$hist_datastream)
      rv$historical_datastream_name <- if(nrow(ds_match) > 0) ds_match$datastream[1] else ""
    })
  })
  
  historical_data_converted <- reactive({
    req(rv$historical_data, rv$historical_datastream_name)
    
    rv$historical_data %>%
      mutate(
        value = convert_value(value_metric, rv$historical_datastream_name, use_imperial())
      )
  })
  
  output$historical_plot <- renderPlotly({
    req(historical_data_converted(), rv$historical_datastream_name)
    
    if (nrow(historical_data_converted()) > 0) {
      unit_label <- get_unit_label(rv$historical_datastream_name, use_imperial())
      
      plot_ly(historical_data_converted(), x = ~timestamp, y = ~value, 
              type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(102, 126, 234)', width = 2),
              marker = list(size = 5, color = 'rgb(118, 75, 162)')) %>%
        layout(xaxis = list(title = "Time"),
               yaxis = list(title = paste("Value (", unit_label, ")")),
               hovermode = 'x unified',
               plot_bgcolor = 'rgba(0,0,0,0)',
               paper_bgcolor = 'rgba(0,0,0,0)')
    } else {
      plot_ly() %>% layout(title = "No data available")
    }
  })
  
  output$hist_stats <- renderTable({
    req(historical_data_converted())
    
    if (nrow(historical_data_converted()) > 0) {
      unit_label <- get_unit_label(rv$historical_datastream_name, use_imperial())
      
      data.frame(
        Statistic = c("Count", "Mean", "Median", "Min", "Max", "Std Dev"),
        Value = c(
          nrow(historical_data_converted()),
          round(mean(historical_data_converted()$value, na.rm = TRUE), 2),
          round(median(historical_data_converted()$value, na.rm = TRUE), 2),
          round(min(historical_data_converted()$value, na.rm = TRUE), 2),
          round(max(historical_data_converted()$value, na.rm = TRUE), 2),
          round(sd(historical_data_converted()$value, na.rm = TRUE), 2)
        ),
        Units = c("", unit_label, unit_label, unit_label, unit_label, unit_label)
      )
    }
  })
  
  output$hist_table <- renderDT({
    req(historical_data_converted(), rv$historical_datastream_name)
    
    if (nrow(historical_data_converted()) > 0) {
      unit_label <- get_unit_label(rv$historical_datastream_name, use_imperial())
      
      historical_data_converted() %>%
        mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M"),
               value = round(value, 2)) %>%
        rename(Timestamp = timestamp, 
               !!paste0("Value (", unit_label, ")") := value) %>%
        datatable(options = list(pageLength = 10),
                  rownames = FALSE)
    }
  })
  
  output$download_current <- downloadHandler(
    filename = function() {
      unit_sys <- if(use_imperial()) "imperial" else "metric"
      paste0("current_conditions_", 
             rv$selected_station$slug, "_",
             unit_sys, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      export_data <- current_data_converted() %>%
        mutate(
          unit_system = if(use_imperial()) "Imperial" else "Metric",
          timestamp_local = format(timestamp_utc, "%Y-%m-%d %H:%M UTC")
        ) %>%
        select(Datastream = datastream, 
               Value = value, 
               Units = units,
               Unit_System = unit_system,
               Timestamp_UTC = timestamp_local)
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$download_historical <- downloadHandler(
    filename = function() {
      unit_sys <- if(use_imperial()) "imperial" else "metric"
      paste0("historical_data_", 
             rv$selected_station$slug, "_",
             unit_sys, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      req(input$export_datastreams, input$export_days)
      
      all_data <- list()
      
      withProgress(message = 'Exporting data...', {
        for (i in seq_along(input$export_datastreams)) {
          ds_id <- input$export_datastreams[i]
          
          ds_match <- rv$current_data %>% filter(datastream_id == ds_id)
          ds_name <- if(nrow(ds_match) > 0) ds_match$datastream[1] else ds_id
          
          incProgress(1/length(input$export_datastreams), 
                      detail = paste("Loading", ds_name))
          
          hist_data <- get_historical_data(ds_id, as.numeric(input$export_days))
          
          if (nrow(hist_data) > 0) {
            hist_data <- hist_data %>%
              mutate(
                value = convert_value(value_metric, ds_name, use_imperial()),
                datastream = ds_name,
                units = get_unit_label(ds_name, use_imperial()),
                unit_system = if(use_imperial()) "Imperial" else "Metric"
              ) %>%
              select(timestamp, datastream, value, units, unit_system)
            
            all_data[[i]] <- hist_data
          }
        }
      })
      
      combined <- bind_rows(all_data)
      write.csv(combined, file, row.names = FALSE)
    }
  )
}

# ============================================================
# RUN APP
# ============================================================

shinyApp(ui, server)