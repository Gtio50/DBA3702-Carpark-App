# Load required libraries
library(shiny)
library(dplyr)
library(data.table)
library(httr)
library(RcppSimdJson)
library(tidyr)
library(DT)
library(readr)


# Define region classification based on region_name
classify_region <- function(region_name) {
  case_when(
    region_name %in% c("BISHAN", "BUKIT MERAH", "BUKIT TIMAH", "CENTRAL AREA", "DOWNTOWN CORE", "GEYLANG", "KALLANG", "MARINA EAST", "MARINA SOUTH", "MARINE PARADE", "MUSEUM", "NEWTON", "NOVENA", "ORCHARD", "OUTRAM", "QUEENSTOWN", "RIVER VALLEY", "ROCHOR", "SINGAPORE RIVER", "SOUTHERN ISLANDS", "STRAITS VIEW", "TANGLIN", "TOA PAYOH") ~ "Central",
    region_name %in% c("BEDOK", "CHANGI", "CHANGI BAY", "PASIR RIS", "PAYA LEBAR", "SIMEI", "TAMPINES") ~ "East",
    region_name %in% c("CENTRAL WATER CATCHMENT", "LIM CHU KANG", "MANDAI", "SEMBAWANG", "SIMPANG", "SUNGEI KADUT", "WOODLANDS", "YISHUN") ~ "North",
    region_name %in% c("ANG MO KIO", "HOUGANG", "NORTH-EASTERN ISLANDS", "PUNGGOL", "SELETAR", "SENGKANG", "SERANGOON") ~ "North-East",
    region_name %in% c("BOON LAY", "BUKIT BATOK", "BUKIT PANJANG", "CHOA CHU KANG", "CLEMENTI", "JURONG EAST", "JURONG WEST", "PIONEER", "TENGAH", "TUAS", "WESTERN ISLANDS", "WESTERN WATER CATCHMENT") ~ "West",
    TRUE ~ "Others"  # Default if no match found
  )
}

# Define a mapping for regions to areas
region_mapping <- list(
  "Central" = c("BISHAN", "BUKIT MERAH", "BUKIT TIMAH", "CENTRAL AREA", "DOWNTOWN CORE", "GEYLANG", "KALLANG", "MARINA EAST", "MARINA SOUTH", "MARINE PARADE", "MUSEUM", "NEWTON", "NOVENA", "ORCHARD", "OUTRAM", "QUEENSTOWN", "RIVER VALLEY", "ROCHOR", "SINGAPORE RIVER", "SOUTHERN ISLANDS", "STRAITS VIEW", "TANGLIN", "TOA PAYOH"),
  "East" = c("BEDOK", "CHANGI", "CHANGI BAY", "PASIR RIS", "PAYA LEBAR", "SIMEI", "TAMPINES"),
  "North" = c("CENTRAL WATER CATCHMENT", "LIM CHU KANG", "MANDAI", "SEMBAWANG", "SIMPANG", "SUNGEI KADUT", "WOODLANDS", "YISHUN"),
  "North-East" = c("ANG MO KIO", "HOUGANG", "NORTH-EASTERN ISLANDS", "PUNGGOL", "SELETAR", "SENGKANG", "SERANGOON"),
  "West" = c("BOON LAY", "BUKIT BATOK", "BUKIT PANJANG", "CHOA CHU KANG", "CLEMENTI", "JURONG EAST", "JURONG WEST", "PIONEER", "TENGAH", "TUAS", "WESTERN ISLANDS", "WESTERN WATER CATCHMENT")
)

# API URL
url <- "https://api.data.gov.sg/v1/transport/carpark-availability"

# Fetch and parse carpark data
fetch_carpark_data <- function() {
  raw_data <- GET(url)
  data <- RcppSimdJson::fparse(raw_data$content)
  
  carpark_data <- setDT(data$items$carpark_data[[1]])
  
  carpark_data$total_lots <- sapply(carpark_data$carpark_info, `[[`, 1)
  carpark_data$lot_type <- sapply(carpark_data$carpark_info, `[[`, 2)
  carpark_data$lots_available <- sapply(carpark_data$carpark_info, `[[`, 3)
  
  carpark_data <- carpark_data %>%
    unnest(cols = c(total_lots, lot_type, lots_available)) %>%
    select(-carpark_info, -update_datetime) %>% 
    mutate(
      total_lots = as.numeric(total_lots),
      lots_available = as.numeric(lots_available)
    )
  
  carpark_data <- setDT(carpark_data)
  return(carpark_data)
}

# Load and process carpark info
carpark_info_path <- "cleaned_data.txt"
carpark_info <- fread(carpark_info_path)

# Apply region classification to carpark_info
carpark_info <- carpark_info %>%
  mutate(region = classify_region(region_name))

# Simplify carpark_info to only unique carpark numbers, areas, and regions
unique_carparks <- carpark_info %>%
  select(carpark_number, region_name, region) %>%
  distinct()

# Combine Real-Time and Static Data
get_combined_carpark_data <- function() {
  api_data <- fetch_carpark_data()
  
  combined_data <- unique_carparks %>%
    inner_join(api_data, by = c("carpark_number" = "carpark_number")) %>%
    mutate(vacancy_percentage = pmin((lots_available / total_lots) * 100, 100)) %>%
    arrange(desc(vacancy_percentage))
  
  return(combined_data)
}
#get_combined_carpark_data()

# Shiny App UI
ui <- fluidPage(
  titlePanel("Singapore HDB Carpark Availability by Region and Area"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:", 
                  choices = c("All", c("Central", "North", "Northeast", "East", "West")), 
                  selected = "All"),
      uiOutput("area_selector")
    ),
    mainPanel(
      DTOutput("carpark_table")
    )
  )
)

# Shiny App Server
server <- function(input, output, session) {
  # Dynamic area selector based on selected region
  output$area_selector <- renderUI({
    areas <- if (input$region == "All") {
      sort(unique(carpark_info$region_name))
    } else {
      sort(region_mapping[[input$region]])
    }
    selectInput("area", "Select Planning Area:", 
                choices = c("All", areas),
                selected = "All")
  })
  
  # Reactive data filtering
  carpark_data <- reactive({
    data <- get_combined_carpark_data()
    
    # Filter based on selected region
    if (input$region != "All") {
      data <- data %>% filter(region == input$region)
    }
    
    # Further filter based on selected area
    if (!is.null(input$area) && input$area != "All") {
      data <- data %>% filter(region_name == input$area)
    }
    
    # Select only relevant columns for display
    data %>% 
      select(carpark_number, region, region_name, total_lots, lots_available, vacancy_percentage)
  })
  
  # Render interactive table
  output$carpark_table <- renderDT({
    req(nrow(carpark_data()) > 0)  # Ensure data exists before rendering
    datatable(
      carpark_data(),
      options = list(pageLength = 15, autoWidth = TRUE),
      rownames = FALSE
    ) %>%
      formatRound('vacancy_percentage', 2) %>%
      formatStyle(
        'vacancy_percentage',
        backgroundColor = styleInterval(c(25, 50, 75), c("red", "orange", "yellow", "green"))
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
