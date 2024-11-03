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
library(geosphere)

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

####################################################################################################################

# making the UI
ui <- fluidPage(
  
  titlePanel("Points of Interests near a carpark"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId = "date", "Date:", value = Sys.Date()),
      timeInput(inputId = "time", "Time:", value = Sys.time()),
      selectizeInput(inputId = "carpark", "Enter Carpark:", choices = NULL, 
                     options = list(placeholder = 'Name of carpark',
                                    maxItems = 1,
                                    create = TRUE)),
      sliderInput(inputId = "distance", "Distance:", 
                  min = 0, max = 5, value = 0), 
    ),
    mainPanel(
      # To plot the map from server
      leafletOutput(outputId = "plotPOI"),
      # Add a table output for the attractions list
      tableOutput(outputId = "attraction_list")
    )
  )
)

####################################################################################################################

####################################################################################################################


# Define server logic  
server <- function(input, output, session) {
  #######################################################
  
  # making a list for the selectizeInput so that users can select their desired carpark, 
  # as well as the coordinates of it to plot on the map
  
  # setting up the URl for APi call
  url_carpark_info <- "https://data.gov.sg/api/action/datastore_search?resource_id=d_23f946fa557947f93a8043bbef41dd09"
  
  # calling the API
  carpark_info <- RcppSimdJson::fparse(GET(url_carpark_info)$content)
  
  # pulling out the addresses only from the API 
  ##  Note: the API only pulls out the first 100 entries, so we will just work with that.
  carpark_info_df <- carpark_info$result$records
  
  #converting the x and y coordinates into latitude and longitude
  carpark_info_df_converted <- st_as_sf(carpark_info_df, coords = c("x_coord", "y_coord"), crs = 3414) %>%
    st_transform(crs = 4326)
  
  # add in latitude and longitude into the carpark information
  carpark_info_df <- carpark_info_df %>%
    mutate(latitude = st_coordinates(carpark_info_df_converted)[,2],
           longitude = st_coordinates(carpark_info_df_converted)[,1])
  
  carpark_info_df_names <- carpark_info_df$address
  # print(carpark_info_df_names)
  
  # Update the selectize input choices dynamically with your custom list
  updateSelectizeInput(session, 'carpark', choices = carpark_info_df_names, server = TRUE)
  
  #######################################################
  
  # Calls the data.gov API to make a list of tourist attractions
  # Output: A dataframe of names of the tourist attractions, as well as their coordinates to plot on the map
  # 3 NOV 2024: Note: I can't seem to get it to work, used the CSV as a workaround instead.

  # setting up the URl for APi call
  # url_tourist_info<- "https://api-open.data.gov.sg/v1/public/api/datasets/d_0f2f47515425404e6c9d2a040dd87354/poll-download"
  
  # calling the API
  # data_availability <- RcppSimdJson::fparse(GET(url_tourist_info)$content)
  
  # calling only the table with the necessary information
  ## Note: not sure why it is written in x and y coordinates, don't know how to convert it
  # data_availability_table <- data_availability$result$records
  # print(data_availability_table)
  
  #converting the x and y coordinates into latitude and longitude
  # data_availability_table_converted <- st_as_sf(data_availability_table, coords = c("x_coord", "y_coord"), crs = 3414) %>% st_transform(crs = 4326)
  
  # add in latitude and longitude into the carpark information
  # data_availability_table <- data_availability_table %>% mutate(latitude = st_coordinates(carparks_sf)[,2], longitude = st_coordinates(carparks_sf)[,1])
  
  
  # pulling out the addresses only from the API 
  ##  Note: the API only pulls out the first 100 entries, so we will just work with that.
  # data_availability_list <- data_availability$result$records$address
  
  # Update the selectize input choices dynamically with your custom list
  # updateSelectizeInput(session, 'text_input', choices = data_availability_list, server = TRUE)
  
  ### Alternative method: Using the CSV ###
  
  # Reading the CSV
  tourist_attraction_info <- read.csv("Tourist & Attractions.csv")
  # print(tourist_attraction_info)
  
  
  
  #######################################################
  
  # Calculates the distance between the selected carpark
  # Input: 
  #   - Carpark name from the UI
  #   - Distance limit from the UI
  # Output: A list of attractions that are within the distance limit
  ## Problem: Map doesn't load
  
  # Reactive expression to get selected carpark location
  selected_carpark <- reactive({
    req(input$carpark)
    carpark_info_df %>%
      filter(car_park_no == input$carpark) %>%
      transmute(
        car_park_no,
        carpark_lat = latitude,
        carpark_lon = longitude
      )
  })
  
  # Reactive expression to calculate nearby attractions
  nearby_attractions <- eventReactive(input$update, {
    req(input$distance, selected_carpark())
    
    # Extract carpark coordinates
    carpark_coords <- c(selected_carpark()$carpark_lon, selected_carpark()$carpark_lat)
    
    # Calculate distances to all attractions and filter within input$distance
    # Calculate distances to all attractions
    tourist_attraction_info %>%
      mutate(
        distance = geosphere::distHaversine(
          cbind(longitude, latitude),  # Make sure these column names match your tourist data
          matrix(carpark_coords, ncol = 2, byrow = TRUE)
        ),
        distance_km = round(distance / 1000, 2)
      ) %>%
      filter(distance_km <= input$distance) %>%
      select(name, description, distance_km, latitude, longitude)  # Adjust column names as needed
  })
  
  #######################################################
  
  # Plots the Map on leaflet of the nearest
  # Input: 
  #   - The list of tourist attractions based on the distance limit set
  # Output: A map of the tourist attractions, as well as the description + distance from the carpark, along with the carpark itself
  
  # Render the map with carpark and nearby attractions
  output$plotPOI <- renderLeaflet({
    req(selected_carpark())
    carpark <- selected_carpark()
    attractions <- nearby_attractions()
    
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = carpark$carpark_lon,
        lat = carpark$carpark_lat,
        popup = paste0("Carpark: ", carpark$address),
        label = "Carpark Location",
        icon = icons(iconUrl = "https://example.com/carpark_icon.png", iconSize = c(25, 25))
      ) %>%
      # Add circle for search radius
      addCircles(
        lng = carpark$carpark_lon,
        lat = carpark$carpark_lat,
        radius = input$distance * 1000,  # Convert km to meters
        color = "red",
        fillOpacity = 0.1
      ) %>%
      # Add markers for attractions if any are found
      addMarkers(
        data = attractions,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste(
          "<strong>", name, "</strong><br>",
          "Distance: ", distance_km, " km<br>",
          description
        ),
        label = ~name
      )
  })
  
  # Render a table with nearby attractions
  output$attraction_list <- renderTable({
    nearby_attractions()
  })
  
  #######################################################

}

shinyApp(ui = ui, server = server)