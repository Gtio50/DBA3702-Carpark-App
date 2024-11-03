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



# UI ----------------------------------------------------------------------


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
      sliderInput(inputId = "distance", "Distance/km:", 
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




# Define server logic  
server <- function(input, output, session) {

# Carpark info list -------------------------------------------------------

  
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
  


# Tourist data read -------------------------------------------------------

  
  # Reading the CSV
  tourist_attraction_info <- read.csv("Tourist & Attractions.csv")
  # print(tourist_attraction_info)
  
  # Calculates the distance between the selected carpark
  # Input: 
  #   - Carpark name from the UI
  #   - Distance limit from the UI
  # Output: 
  #   - A list of attractions that are within the distance limit
  
  # Reactive expression to get selected carpark, as well as its coordinates
  selected_carpark <- reactive({
    req(input$carpark)
    # finds matching carpark in the dataframe
    carpark_info_df %>%
      filter(address == input$carpark) %>%
      transmute(
        address,
        carpark_lat = latitude,
        carpark_lon = longitude
      )
  })
  
  # Reactive expression to calculate nearby attractions
  nearby_attractions <- reactive({
    # Check to see if there's any inputted distance from the UI, as well as the selected carpark dataframe
    req(input$distance, selected_carpark())
    
    # Extract carpark coordinates
    carpark_coords <- c(selected_carpark()$carpark_lon, selected_carpark()$carpark_lat)
    
    # Calculate distances to all attractions and filter within input$distance
    tourist_attraction_info %>%
      mutate(
        distance = geosphere::distHaversine(
          cbind(Longitude, Latitude),  
          matrix(carpark_coords, ncol = 2, byrow = TRUE)
        ),
        distance_km = round(distance / 1000, 2)
      ) %>%
      filter(distance_km <= input$distance) %>%
      select(PAGETITLE, ADDRESS, OVERVIEW,  distance_km, Longitude, Latitude) 
  })
  
  # print(nearby_attractions)

# Map plotting ------------------------------------------------------------

  
  # Plots the Map on leaflet of the nearest points of interests based on the carpark selected
  # Input: 
  #   - The list of tourist attractions based on the distance limit set
  # Output: 
  #   - A map of the tourist attractions, as well as the description + distance from the carpark, along with the carpark itself

# Base map ----------------------------------------------------------------

  
  # Modified map rendering with conditional plotting
  output$plotPOI <- renderLeaflet({
    
    # check to see if there is selected carpark or not.
    req(selected_carpark())
    
    # selected carpark is the dataframe of the selected carpark, and their respective latitude and longitude.
    carpark <- selected_carpark()
    
    # Create base map with carpark marker
    map <- leaflet() %>%
      addTiles() %>%
      # Adding and changing the carpark marker to have a different colour and icon from the location markers
      addAwesomeMarkers(
        lng = carpark$carpark_lon,
        lat = carpark$carpark_lat,
        popup = paste0("Carpark: ", carpark$address),
        label = "Carpark Location",
        icon = makeAwesomeIcon(
          icon = 'car',
          markerColor = 'orange',
          iconColor = 'black',
          library = 'fa'
        )
      )

# Adding the distance boundary --------------------------------------------

    
    # Only add circle and attractions if distance is greater than 0
    if (input$distance > 0) {
      attractions <- nearby_attractions()
      
      # Add search radius circle
      map <- map %>%
        addCircles(
          lng = carpark$carpark_lon,
          lat = carpark$carpark_lat,
          radius = input$distance * 1000,  # Convert km to meters
          color = "red",
          fillOpacity = 0.1
        )

# Adding markers for the tourist attractions ------------------------------

      
      # Add attraction markers only if there are any attractions
      if (nrow(attractions) > 0) {
        map <- map %>%
          addMarkers(
            data = attractions,
            lng = ~Longitude,
            lat = ~Latitude,
            popup = ~paste(
              "<strong>", PAGETITLE, "</strong><br>",
              "Distance: ", distance_km, " km<br>",
              OVERVIEW
            ),
            label = ~PAGETITLE
          )
      }
    }
    
    return(map)
  })

# Attraction list table ---------------------------------------------------

  
  # modify the attractions table to show nothing when distance is 0
  output$attraction_list <- renderTable({
    req(selected_carpark())
    if (input$distance > 0) {
      nearby_attractions()
    } else {
      # Return empty data frame with same structure
      data.frame(
        PAGETITLE = character(),
        ADDRESS = character(),
        OVERVIEW = character(),
        distance_km = numeric(),
        Longitude = numeric(),
        Latitude = numeric(),
        stringsAsFactors = FALSE
      )
    }
  })
  
}

shinyApp(ui = ui, server = server)