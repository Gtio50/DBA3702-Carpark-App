library(jsonlite)
library(sf)
library(shiny)
library(ggplot2)
library(shinyTime)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(shinycssloaders)
library(dplyr)
library(jsonlite)
library(curl)
library(tidyr)
library(data.table)
library(httr)
library(RcppSimdJson)
library(rbenchmark)
library(furrr)
library(future)
library(purrr)
library(parallel)
library(rlang)

# Made by: YH

# Maps 1: Finding out the Points of interest(POI) near a carpark (YH)
# Input:
#   - time (as above)
#   - exact carpark they want
# Output:
#   - map with markers of possible POI
#   - table listing down the POI
#   - POI
#   - Postal code
#   - Short description

####################################################################################################################

# Functions and elements for map generation
# The api key
hdb_url <- "https://api.data.gov.sg/v1/transport/carpark-availability"

# Retrieve and clean api data for carpark availability data
cp_extract_data <- function(datetime, url){
  datetime_char <- as.character(datetime)
  url <- paste0(url, "?date_time=", 
                substr(datetime_char, 1, 10), "T", 
                substr(datetime_char, 12, 19))
  
  data <- RcppSimdJson::fparse(GET(url)$content)
  
  carpark <- setDT(data$items$carpark_data[[1]][,2:3])
  carpark_info <- setDT(data$items$carpark_data[[1]][,1])
  carpark$update_datetime <- rep(datetime, length(carpark[[1]]))
  carpark$total_lots <- sapply(carpark_info, `[[`, 1)
  carpark$lot_type <- sapply(carpark_info, `[[`, 2)
  carpark$lots_available <- sapply(carpark_info, `[[`, 3)
  
  carpark <- carpark %>% 
    unnest(cols = c(total_lots, lot_type, lots_available))
  
  setDT(carpark)
  
  carpark[, c("total_lots", "lots_available") := lapply(.SD, as.numeric), .SDcols = c("total_lots", "lots_available")]
  
  return(carpark)
}

# Takes in a datetime sequence and calls the api for the carpark data
carpark_historic_data_get <- function(datetime_seq){
  data <- furrr::future_map(datetime_seq, 
                            ~ retry_function(cp_extract_data, .x, hdb_url), 
                            .progress = T, .options = furrr_options(seed = T))
  bool <- sapply(data, is.data.frame)
  res <- rbindlist(data[bool])
  error_dates <- rbind(data[!bool])
  return(list(results = res, error = error_dates))
}


####################################################################################################################

# making the UI
ui <- fluidPage(
  
  titlePanel("Points of Interests near a carpark"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId = "date", "Date:", value = Sys.Date()),
      timeInput(inputId = "time", "Time:", value = Sys.time()),
      selectizeInput(inputID = "carpark", "Enter Carpark:", choices = NULL, 
                     options = list(placeholder = 'Name of carpark',
                                    maxItems = 1,
                                    create = TRUE)),
      sliderInput(inputID = "distance", "Distance:", 
                  min = 0, max = 5, value = 0), 
    ),
    mainPanel(
      # To plot the map from server
      leafletOutput(outputId = "plotPOI")
    )
  )
)

# Define server logic  
server <- function(input, output, session) {
  #######################################################
  
  # making a list for the selectizeInput so that users can select their desired carpark, 
  # as well as the coordinates of it to plot on the map
  
  # setting up the URl for APi call
  url_carpark_info <- "https://data.gov.sg/api/action/datastore_search?resource_id=d_23f946fa557947f93a8043bbef41dd09"
  
  # calling the API
  data_availability <- RcppSimdJson::fparse(GET(url_carpark_info)$content)
  
  # pulling out the addresses only from the API 
  ##  Note: the API only pulls out the first 100 entries, so we will just work with that.
  data_availability_list <- data_availability$result$records$address
  
  # Update the selectize input choices dynamically with the custom list of HDB carparks generated
  updateSelectizeInput(session, 'text_input', choices = data_availability_list, server = TRUE)
  
  #######################################################
  
  # Calls the data.gov API to make a list of tourist attractions
  # Output: A dataframe of names of the tourist attractions, as well as their coordinates to plot on the map
  # 3 NOV 2024: Note: I can't seem to get it to work, used the CSV as a workaround instead.

  # setting up the URl for APi call
  url_tourist_info<- "https://api-open.data.gov.sg/v1/public/api/datasets/d_0f2f47515425404e6c9d2a040dd87354/poll-download"
  
  # calling the API
  data_availability <- RcppSimdJson::fparse(GET(url_tourist_info)$content)
  
  # pulling out the addresses only from the API 
  ##  Note: the API only pulls out the first 100 entries, so we will just work with that.
  data_availability_list <- data_availability$result$records$address
  
  # Update the selectize input choices dynamically with your custom list
  updateSelectizeInput(session, 'text_input', choices = data_availability_list, server = TRUE)
  
  ### Alternative method: Using the CSV ###
  
  # Reading the CSV
  tourist_attraction_info <- read.csv("Tourist & Attractions.csv")
  
  
  #######################################################
  
  # Calculates the distance between the selected carpark
  # Input: 
  #   - Carpark name from the UI
  #   - Distance limit from the UI
  # Output: A list of attractions that are within the distance limit
  
  ### insert code here ###
  
  #######################################################
  
  
  # Plots the Map on leaflet of the nearest
  # Input: 
  #   - The list of tourist attractions based on the distance limit set
  # Output: A map of the tourist attractions, as well as the description + distance from the carpark, along with the carpark itself
  
  ### insert code here ###
  
  #######################################################
  
  # making the API to see the availability of the carpark that they have chosen
  ### need work:
  #### 1) update so that we do the available slots, % capacity, and maybe traffic light
  get_carpark_availability_reactive <- reactive({
    #taking the date and time from the input and appending it into the API URL
    date <- as.character(input$date)
    time <- strftime(input$time, "%T")
    url = "https://api.data.gov.sg/v1/transport/carpark-availability?date_time="
    url_availability <- paste0(url,date,"T",time)
    # checks
    print(date)
    print(time)
    print(url_availability)
    
    # calling the API
    data_availability <- RcppSimdJson::fparse(GET(url_availability)$content)
    
    carpark <- setDT(data$items$carpark_data[[1]][,2:3])
    carpark_info <- setDT(data$items$carpark_data[[1]][,1])
    carpark$update_datetime <- rep(datetime, length(carpark[[1]]))
    carpark$total_lots <- sapply(carpark_info, `[[`, 1)
    carpark$lot_type <- sapply(carpark_info, `[[`, 2)
    carpark$lots_available <- sapply(carpark_info, `[[`, 3)
    
    carpark <- carpark %>% 
      unnest(cols = c(total_lots, lot_type, lots_available))
    
    setDT(carpark)
    
    carpark[, c("total_lots", "lots_available") := lapply(.SD, as.numeric), .SDcols = c("total_lots", "lots_available")]
    
    return(carpark)
  })
  
  carpark_historic_data_get_reactive <- reactive({
    print(class(input$date))
    
    date <- as.character(input$date)
    time <- strftime(input$time, "%T")
    
    print("New data")
    
    url = "https://api.data.gov.sg/v1/transport/taxi-availability?date_time="
    url <- paste0(url,date,"T",time)
    data <- fromJSON(url)
    data <- as.data.frame(data$features$geometry$coordinates)
    colnames(data) <- c("long","lat")
    data
  })
  
  
  # plotting out the map
  output$plotAvailability <-renderLeaflet(
    {
      
      if(date>today)
        retrun
      # must be the same as the inputID in the ui i.e. input$date
      date <- as.character(input$date)
      time <- strftime(input$time, "%T")
      
      
      data <- getAvailabilityData(date,time)
      
      
      if(input$format=='Point') {
        
        m <- addCircleMarkers(m, 
                              lng=data$long,
                              lat=data$lat, 
                              radius=3,
                              stroke = FALSE, 
                              fillOpacity = 1
        )
      }
      else if (input$format == 'Heatmap'){
        
        m<- addWebGLHeatmap(m,
                            lng=data$long, 
                            lat=data$lat, 
                            size = 600)
      }
      else {print("Wrong format name")}
      m
    }
  )
}

shinyApp(ui = ui, server = server)