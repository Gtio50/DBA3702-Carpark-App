# Load required libraries
library(shiny)
library(dplyr)
library(data.table)
library(httr)
library(RcppSimdJson)
library(tidyr)
library(DT)
library(readr)

# Define region classification mappings
REGION_MAPPING <- list(
  "Central" = c("BISHAN", "BUKIT MERAH", "BUKIT TIMAH", "CENTRAL AREA", "DOWNTOWN CORE", "GEYLANG", "KALLANG", "MARINA EAST", "MARINA SOUTH", "MARINE PARADE", "MUSEUM", "NEWTON", "NOVENA", "ORCHARD", "OUTRAM", "QUEENSTOWN", "RIVER VALLEY", "ROCHOR", "SINGAPORE RIVER", "SOUTHERN ISLANDS", "STRAITS VIEW", "TANGLIN", "TOA PAYOH"),
  "East" = c("BEDOK", "CHANGI", "CHANGI BAY", "PASIR RIS", "PAYA LEBAR", "SIMEI", "TAMPINES"),
  "North" = c("CENTRAL WATER CATCHMENT", "LIM CHU KANG", "MANDAI", "SEMBAWANG", "SIMPANG", "SUNGEI KADUT", "WOODLANDS", "YISHUN"),
  "North-East" = c("ANG MO KIO", "HOUGANG", "NORTH-EASTERN ISLANDS", "PUNGGOL", "SELETAR", "SENGKANG", "SERANGOON"),
  "West" = c("BOON LAY", "BUKIT BATOK", "BUKIT PANJANG", "CHOA CHU KANG", "CLEMENTI", "JURONG EAST", "JURONG WEST", "PIONEER", "TENGAH", "TUAS", "WESTERN ISLANDS", "WESTERN WATER CATCHMENT")
)

# Create lookup for faster classification
AREA_TO_REGION <- unlist(lapply(names(REGION_MAPPING), function(region) setNames(rep(region, length(REGION_MAPPING[[region]])), REGION_MAPPING[[region]])))

# Optimized classification function
classify_region <- function(region_name) {
  AREA_TO_REGION[region_name] %||% "Others"
}

# Fetch carpark data from API
fetch_carpark_data <- function() {
  raw_data <- GET("https://api.data.gov.sg/v1/transport/carpark-availability")
  data <- RcppSimdJson::fparse(raw_data$content)
  carpark_data <- setDT(data$items$carpark_data[[1]])
  carpark_data$total_lots <- sapply(carpark_data$carpark_info, `[[`, 1)
  carpark_data$lot_type <- sapply(carpark_data$carpark_info, `[[`, 2)
  carpark_data$lots_available <- sapply(carpark_data$carpark_info, `[[`, 3)
  carpark_data <- carpark_data %>% unnest(cols = c(total_lots, lot_type, lots_available)) %>% 
    select(-carpark_info, -update_datetime) %>%
    mutate(total_lots = as.numeric(total_lots), lots_available = as.numeric(lots_available))
  setDT(carpark_data)
  return(carpark_data)
}

# Load carpark info
carpark_info <- fread("cleaned_data.txt") %>%
  mutate(region = classify_region(region_name)) %>%
  select(carpark_number, region_name, region) %>%
  distinct()

# Combine Real-Time and Static Data
get_combined_carpark_data <- function() {
  api_data <- fetch_carpark_data()
  combined_data <- carpark_info %>%
    inner_join(api_data, by = c("carpark_number" = "carpark_number")) %>%
    mutate(vacancy_percentage = pmin((lots_available / total_lots) * 100, 100)) %>%
    arrange(desc(vacancy_percentage))
  
  print("Combined data rows after join:")
  print(nrow(combined_data))  # Check number of rows after join
  
  return(combined_data)
}

# Shiny App UI
ui <- fluidPage(
  titlePanel("Singapore HDB Carpark Availability by Region and Area"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", choices = c("All", names(REGION_MAPPING), "Others"), selected = "All"),
      uiOutput("area_selector")
    ),
    mainPanel(DTOutput("carpark_table"))
  )
)

# Shiny App Server
server <- function(input, output, session) {
  # Generate area options based on region
  output$area_selector <- renderUI({
    areas <- if (input$region == "All") {
      unique(carpark_info$region_name)
    } else {
      REGION_MAPPING[[input$region]] %||% character(0)
    }
    selectInput("area", "Select Planning Area:", choices = c("All", areas), selected = "All")
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- get_combined_carpark_data()
    if (input$region != "All") {
      data <- data %>% filter(region == input$region)
    }
    if (input$area != "All") {
      data <- data %>% filter(region_name == input$area)
    }
    print("Data after filtering:")
    print(nrow(data))  # Debug row count after filtering
    return(data)
  })
  
  # Render DataTable
  output$carpark_table <- renderDT({
    data <- filtered_data()
    req(nrow(data) > 0)
    datatable(data %>% select(
      `Carpark Number` = carpark_number,
      Region = region,
      `Planning Area` = region_name,
      `Total Lots` = total_lots,
      `Available Lots` = lots_available,
      `Vacancy %` = vacancy_percentage
    ), options = list(pageLength = 15, autoWidth = TRUE))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
