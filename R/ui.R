library(shiny)
library(leaflet)


# yhMap UI ----------------------------------------------------------------
yhMapUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$head(
      # Leaflet Routing Machine CSS
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet-routing-machine@3.2.12/dist/leaflet-routing-machine.css"),
      tags$script(src = "https://unpkg.com/leaflet-routing-machine@3.2.12/dist/leaflet-routing-machine.js"),
      # Include Mapbox routing (if you're using Mapbox)
      tags$script(src = "https://unpkg.com/leaflet-routing-machine@3.2.12/dist/leaflet-routing-mapbox.js")
    ),
    
    tags$style("
        .leaflet-container {
          background: #f8f9fa;
        }
        .custom-routing-container {
          max-width: 300px;
        }
        .leaflet-popup-content {
          margin: 12px;
        }
        .leaflet-popup-content button:hover {
          background-color: #1557b0 !important;
        }
      "),
    
    titlePanel("Points of Interests near a carpark"),
    
    sidebarLayout(
      sidebarPanel(
        dateInput(inputId = ns("date"), "Date:", value = Sys.Date()),
        timeInput(inputId = ns("time"), "Time:", value = Sys.time()),
        selectizeInput(inputId = ns("carpark"), "Enter Carpark:", choices = NULL, 
                       options = list(placeholder = 'Name of carpark',
                                      maxItems = 1,
                                      create = TRUE)),
        sliderInput(inputId = ns("distance"), "Distance/km:", 
                    min = 0, max = 5, value = 0)
      ),
      mainPanel(
        # To plot the map from server
        leafletOutput(outputId = ns("plotPOI")),
        # Add a table output for the attractions list
        tableOutput(outputId = ns("attraction_list"))
      )
    )
  )
}

