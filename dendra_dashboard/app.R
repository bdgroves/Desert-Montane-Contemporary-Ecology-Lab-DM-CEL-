# app.R
# Shiny Dashboard for Dendra Science Weather Stations

# Install and load packages
quiet_install <- function(pkgs){
  need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(need)) install.packages(need, repos = "https://cloud.r-project.org")
}
quiet_install(c("shiny", "shinydashboard", "httr", "jsonlite", "dplyr", 
                "plotly", "DT", "lubridate", "stringr"))

library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)
library(stringr)

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

# Get all stations
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

# Get current conditions for a station
get_current_conditions <- function(station_id) {
  # Get datastreams
  ds <- get_json("/datastreams", query = list(station_id = station_id, `$limit` = 200))
  if ("data" %in% names(ds)) {
    datastreams_df <- as_tibble(ds$data)
  } else {
    datastreams_df <- as_tibble(ds)
  }
  
  # Get latest value from each datastream
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
          value = dp_df$v[1],
          timestamp_utc = as.POSIXct(dp_df$t[1], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
          datastream_id = datastreams_df$`_id`[i]
        )
      }
    }
  }
  
  bind_rows(current_list)
}

# Get historical data for a datastream
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
      mutate(timestamp = as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>%
      select(timestamp, value = v)
  } else {
    tibble(timestamp = as.POSIXct(character()), value = numeric())
  }
}

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(title = "Dendra Weather Stations"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Historical Data", tabName = "historical", icon = icon("chart-line")),
      menuItem("Data Export", tabName = "export", icon = icon("download")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    
    hr(),
    
    # Searchable selectize input
    selectizeInput("station_select", 
                   "Search & Select Station:",
                   choices = NULL,
                   options = list(
                     placeholder = 'Type to search stations...',
                     onInitialize = I('function() { this.setValue(""); }')
                   ),
                   width = "100%"),
    
    # Add helpful text
    tags$div(
      style = "padding: 10px; font-size: 11px; color: #999;",
      icon("info-circle"),
      " Type station name to filter"
    ),
    
    hr(),
    
    actionButton("refresh_btn", "Refresh Data", 
                 icon = icon("refresh"),
                 class = "btn-primary",
                 width = "100%")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-box { min-height: 100px; }
        .info-box-icon { height: 100px; line-height: 100px; }
        .info-box-content { padding-top: 10px; padding-bottom: 10px; }
        .small-box { min-height: 120px; }
        
        /* Better styling for selectize */
        .selectize-input {
          font-size: 14px;
          padding: 8px 12px;
        }
        .selectize-dropdown {
          font-size: 13px;
          max-height: 300px;
        }
        .selectize-dropdown-content {
          max-height: 280px;
        }
        .selectize-dropdown-content .option {
          padding: 8px 12px;
        }
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = textOutput("station_title"),
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  textOutput("station_info"),
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
                  title = "Temperature (Last 24 Hours)",
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("temp_plot", height = 300)
                ),
                box(
                  title = "Humidity (Last 24 Hours)",
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("humidity_plot", height = 300)
                )
              ),
              
              fluidRow(
                box(
                  title = "All Current Measurements",
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
                  title = "Historical Data Settings",
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
                                        "Load Data",
                                        icon = icon("chart-line"),
                                        class = "btn-success")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Time Series Plot",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("historical_plot", height = 400)
                )
              ),
              
              fluidRow(
                box(
                  title = "Statistics",
                  width = 6,
                  status = "warning",
                  solidHeader = TRUE,
                  tableOutput("hist_stats")
                ),
                box(
                  title = "Data Table",
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
                  title = "Export Current Conditions",
                  width = 6,
                  status = "success",
                  solidHeader = TRUE,
                  p("Download current conditions for the selected station."),
                  downloadButton("download_current", "Download CSV", class = "btn-success")
                ),
                box(
                  title = "Export Historical Data",
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
                  downloadButton("download_historical", "Download CSV", class = "btn-success")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About Dendra Science",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  h4("Dendra Science Weather Station Dashboard"),
                  p("This interactive dashboard provides real-time and historical data from weather stations 
                    in the Dendra Science network."),
                  hr(),
                  h4("Features:"),
                  tags$ul(
                    tags$li("🔍 Searchable station selector - type to find stations quickly"),
                    tags$li("📊 View current conditions from any active station"),
                    tags$li("📈 Interactive time series plots"),
                    tags$li("📉 Historical data analysis with statistics"),
                    tags$li("💾 Export data to CSV format"),
                    tags$li("🔄 Real-time data refresh")
                  ),
                  hr(),
                  h4("How to Use:"),
                  tags$ol(
                    tags$li("Type in the station search box to find your station (e.g., 'Barcroft', 'White Mountain')"),
                    tags$li("Select a station from the dropdown"),
                    tags$li("View current conditions on the Dashboard tab"),
                    tags$li("Explore historical data in the Historical Data tab"),
                    tags$li("Export data from the Data Export tab")
                  ),
                  hr(),
                  h4("Data Source:"),
                  p("Data is provided by ", 
                    tags$a(href = "https://dendra.science", "Dendra Science", target = "_blank"),
                    " via their public API."),
                  p("API Endpoint: ", tags$code("https://api.dendra.science/v2")),
                  hr(),
                  p(tags$small("Built with R Shiny • Last updated: ", Sys.Date()))
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
  
  # Reactive values
  rv <- reactiveValues(
    stations = NULL,
    current_data = NULL,
    selected_station = NULL,
    datastreams = NULL,
    historical_data = NULL
  )
  
  # Load stations on startup
  observe({
    withProgress(message = 'Loading stations...', {
      rv$stations <- get_all_stations()
      
      # Create searchable labels with name and full_name
      station_choices <- setNames(
        rv$stations$`_id`, 
        paste0(rv$stations$name, 
               ifelse(!is.na(rv$stations$full_name) & rv$stations$full_name != rv$stations$name,
                      paste0(" - ", rv$stations$full_name),
                      ""))
      )
      
      # Update with all choices at once (client-side search)
      updateSelectizeInput(
        session, 
        "station_select", 
        choices = station_choices,
        selected = character(0)
      )
    })
  })
  
  # Load data when station selected or refresh clicked
  observeEvent(c(input$station_select, input$refresh_btn), {
    req(input$station_select)
    
    withProgress(message = 'Loading station data...', {
      
      # Get selected station info
      rv$selected_station <- rv$stations %>%
        filter(`_id` == input$station_select)
      
      # Get current conditions
      incProgress(0.5, detail = "Fetching current conditions...")
      rv$current_data <- get_current_conditions(input$station_select)
      
      # Update datastream choices with search
      if (nrow(rv$current_data) > 0) {
        datastream_choices <- setNames(
          rv$current_data$datastream_id, 
          rv$current_data$datastream
        )
        
        updateSelectizeInput(session, "hist_datastream", 
                             choices = datastream_choices,
                             selected = character(0))
        updateSelectizeInput(session, "export_datastreams", 
                             choices = datastream_choices,
                             selected = character(0))
      }
    })
  }, ignoreNULL = TRUE)
  
  # Station title
  output$station_title <- renderText({
    req(rv$selected_station)
    rv$selected_station$full_name
  })
  
  # Station info
  output$station_info <- renderText({
    req(rv$selected_station)
    
    info_parts <- c()
    
    if ("geo" %in% names(rv$selected_station)) {
      geo <- rv$selected_station$geo[[1]]
      if (!is.null(geo$coordinates)) {
        info_parts <- c(info_parts, 
                        sprintf("📍 Location: %.5f°, %.5f°", 
                                geo$coordinates[2], geo$coordinates[1]))
      }
    }
    
    if ("time_zone" %in% names(rv$selected_station)) {
      info_parts <- c(info_parts, 
                      sprintf("🕐 Time Zone: %s", rv$selected_station$time_zone))
    }
    
    if ("slug" %in% names(rv$selected_station)) {
      info_parts <- c(info_parts, 
                      sprintf("🔗 Slug: %s", rv$selected_station$slug))
    }
    
    paste(info_parts, collapse = " | ")
  })
  
  # Last update time
  output$last_update <- renderText({
    req(rv$current_data)
    paste("🔄 Last updated:", format(max(rv$current_data$timestamp_utc), "%Y-%m-%d %H:%M UTC"))
  })
  
  # Value boxes
  output$temp_box <- renderValueBox({
    req(rv$current_data)
    
    temp_data <- rv$current_data %>%
      filter(str_detect(datastream, regex("Air Temp.*Avg", ignore_case = TRUE)))
    
    if (nrow(temp_data) > 0) {
      valueBox(
        sprintf("%.1f°C", temp_data$value[1]),
        "Temperature",
        icon = icon("temperature-high"),
        color = "red"
      )
    } else {
      valueBox("N/A", "Temperature", icon = icon("temperature-high"), color = "red")
    }
  })
  
  output$humidity_box <- renderValueBox({
    req(rv$current_data)
    
    hum_data <- rv$current_data %>%
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
    req(rv$current_data)
    
    press_data <- rv$current_data %>%
      filter(str_detect(datastream, regex("Barometric.*Avg", ignore_case = TRUE)))
    
    if (nrow(press_data) > 0) {
      valueBox(
        sprintf("%.1f hPa", press_data$value[1]),
        "Pressure",
        icon = icon("gauge"),
        color = "green"
      )
    } else {
      valueBox("N/A", "Pressure", icon = icon("gauge"), color = "green")
    }
  })
  
  output$wind_box <- renderValueBox({
    req(rv$current_data)
    
    wind_data <- rv$current_data %>%
      filter(str_detect(datastream, regex("Wind Speed.*Avg", ignore_case = TRUE)))
    
    if (nrow(wind_data) > 0) {
      valueBox(
        sprintf("%.1f m/s", wind_data$value[1]),
        "Wind Speed",
        icon = icon("wind"),
        color = "yellow"
      )
    } else {
      valueBox("N/A", "Wind Speed", icon = icon("wind"), color = "yellow")
    }
  })
  
  # Temperature plot (24 hours)
  output$temp_plot <- renderPlotly({
    req(rv$current_data)
    
    temp_ds <- rv$current_data %>%
      filter(str_detect(datastream, regex("Air Temp.*Avg", ignore_case = TRUE)))
    
    if (nrow(temp_ds) > 0) {
      hist_data <- get_historical_data(temp_ds$datastream_id[1], days = 1)
      
      if (nrow(hist_data) > 0) {
        plot_ly(hist_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(255, 99, 71)')) %>%
          layout(xaxis = list(title = "Time"),
                 yaxis = list(title = "Temperature (°C)"),
                 hovermode = 'x unified')
      } else {
        plot_ly() %>% layout(title = "No data available")
      }
    } else {
      plot_ly() %>% layout(title = "No temperature data available")
    }
  })
  
  # Humidity plot (24 hours)
  output$humidity_plot <- renderPlotly({
    req(rv$current_data)
    
    hum_ds <- rv$current_data %>%
      filter(str_detect(datastream, regex("Humidity.*Avg", ignore_case = TRUE)))
    
    if (nrow(hum_ds) > 0) {
      hist_data <- get_historical_data(hum_ds$datastream_id[1], days = 1)
      
      if (nrow(hist_data) > 0) {
        plot_ly(hist_data, x = ~timestamp, y = ~value, type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(65, 105, 225)')) %>%
          layout(xaxis = list(title = "Time"),
                 yaxis = list(title = "Humidity (%)"),
                 hovermode = 'x unified')
      } else {
        plot_ly() %>% layout(title = "No data available")
      }
    } else {
      plot_ly() %>% layout(title = "No humidity data available")
    }
  })
  
  # Current measurements table
  output$current_table <- renderDT({
    req(rv$current_data)
    
    rv$current_data %>%
      mutate(timestamp_local = format(timestamp_utc, "%Y-%m-%d %H:%M UTC")) %>%
      select(Datastream = datastream, 
             Value = value, 
             Timestamp = timestamp_local) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE)
  })
  
  # Load historical data
  observeEvent(input$load_hist_btn, {
    req(input$hist_datastream, input$hist_days)
    
    withProgress(message = 'Loading historical data...', {
      rv$historical_data <- get_historical_data(input$hist_datastream, 
                                                as.numeric(input$hist_days))
    })
  })
  
  # Historical plot
  output$historical_plot <- renderPlotly({
    req(rv$historical_data)
    
    if (nrow(rv$historical_data) > 0) {
      plot_ly(rv$historical_data, x = ~timestamp, y = ~value, 
              type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(75, 192, 192)'),
              marker = list(size = 4)) %>%
        layout(xaxis = list(title = "Time"),
               yaxis = list(title = "Value"),
               hovermode = 'x unified')
    } else {
      plot_ly() %>% layout(title = "No data available")
    }
  })
  
  # Historical statistics
  output$hist_stats <- renderTable({
    req(rv$historical_data)
    
    if (nrow(rv$historical_data) > 0) {
      data.frame(
        Statistic = c("Count", "Mean", "Median", "Min", "Max", "Std Dev"),
        Value = c(
          nrow(rv$historical_data),
          round(mean(rv$historical_data$value, na.rm = TRUE), 2),
          round(median(rv$historical_data$value, na.rm = TRUE), 2),
          round(min(rv$historical_data$value, na.rm = TRUE), 2),
          round(max(rv$historical_data$value, na.rm = TRUE), 2),
          round(sd(rv$historical_data$value, na.rm = TRUE), 2)
        )
      )
    }
  })
  
  # Historical data table
  output$hist_table <- renderDT({
    req(rv$historical_data)
    
    if (nrow(rv$historical_data) > 0) {
      rv$historical_data %>%
        mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M")) %>%
        datatable(options = list(pageLength = 10),
                  rownames = FALSE)
    }
  })
  
  # Download current conditions
  output$download_current <- downloadHandler(
    filename = function() {
      paste0("current_conditions_", 
             rv$selected_station$slug, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      write.csv(rv$current_data, file, row.names = FALSE)
    }
  )
  
  # Download historical data
  output$download_historical <- downloadHandler(
    filename = function() {
      paste0("historical_data_", 
             rv$selected_station$slug, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"),
             ".csv")
    },
    content = function(file) {
      req(input$export_datastreams, input$export_days)
      
      all_data <- list()
      
      withProgress(message = 'Exporting data...', {
        for (i in seq_along(input$export_datastreams)) {
          ds_id <- input$export_datastreams[i]
          
          # Find datastream name
          ds_match <- rv$current_data %>% filter(datastream_id == ds_id)
          ds_name <- if(nrow(ds_match) > 0) ds_match$datastream[1] else ds_id
          
          incProgress(1/length(input$export_datastreams), 
                      detail = paste("Loading", ds_name))
          
          hist_data <- get_historical_data(ds_id, as.numeric(input$export_days))
          
          if (nrow(hist_data) > 0) {
            hist_data$datastream <- ds_name
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