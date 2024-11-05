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

# Load Sources & Datasets -------------------------------------------------
source("R/data manager.R")
source("R/functions.R")
DATASET <- load_datasets()

# yhMap Server ------------------------------------------------------------
yhMapServer <- function(id){
  moduleServer(id, function(input, output, session){
    # Carpark data -------------------------------------------------------
    carpark_info_df <- DATASET$carpark_info
    
    carpark_info_df_names <- carpark_info_df$address
    
    # Update the selectize input choices dynamically with your custom list ----
    updateSelectizeInput(session, 'carpark', choices = carpark_info_df_names, server = TRUE)
    
    # Tourist data read ----
    tourist_attraction_info <- DATASET$tourist
   
    # Filter out carpark ------------------------------------------------------
    selected_carpark <- reactive({
      req(input$carpark)
      filter_single_location(carpark_info_df, input$carpark)
    })
    
    # Calculate distances between selected carpark and pois-------------------
    dist_cp_poi_df <- reactive({
      req(selected_carpark())
      all_nearby_poi(selected_carpark(), tourist_attraction_info)
    })

    # Filter Out attractions that are within dist ----------------------------
    nearby_attractions <- reactive({
      req(input$distance, dist_cp_poi_df())
      dist_cp_poi_df() %>%
        filter(distance_km <= input$distance) 
    })
    
    # Map plotting ------------------------------------------------------------
    
    
    # Plots the Map on leaflet of the nearest points of interests based on the carpark selected
    # Input: 
    #   - The list of tourist attractions based on the distance limit set
    # Output: 
    #   - A map of the tourist attractions, as well as the description + distance from the carpark, along with the carpark itself
    
    # Base map ----------------------------------------------------------------
    
    
    # Modified map rendering with conditional plotting
    output$plotPOI <- renderLeaflet({
      
      req(selected_carpark())
      carpark <- selected_carpark()
      
      carparkIcon <- makeAwesomeIcon(
        icon = 'car',
        markerColor = 'orange',
        iconColor = 'black',
        library = 'fa'
      )
      
      poiIcon <- makeAwesomeIcon(
        icon = 'info-sign',
        markerColor = 'blue',
        iconColor = 'white',
        library = 'glyphicon'  # Using glyphicon instead of fa for POIs
      )
      
      # Create base map with carpark marker
      map <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron, 
                         options = providerTileOptions(opacity = 0.8)) %>%
        
        setView(lng = carpark$longitude, lat = carpark$latitude, zoom = 13) %>%
        
        #Add satellite view option
        addProviderTiles(providers$Esri.WorldImagery,
        group = "Satellite") %>%
        
        # Layer controls
        addLayersControl(
        baseGroups = c("Default", "Satellite"),
        overlayGroups = c("Search Radius"),
        options = layersControlOptions(collapsed = FALSE)
        ) %>%
        
        addScaleBar(position = "bottomleft") %>%
        
        # Adding and changing the carpark marker to have a different colour and icon from the location markers
        addAwesomeMarkers(
          lng = carpark$longitude,
          lat = carpark$latitude,
          popup = paste0("Carpark: ", carpark$address),
          label = "Carpark Location",
          icon = carparkIcon
        ) %>%
        # Initialize routing machine
        htmlwidgets::onRender(sprintf("
          function(el, x) {
            var map = this;
            
            // Initialize global variables
            window.yhMap = {
              map: map,
              carparkLocation: [%s, %s],
              routeGroup: L.featureGroup().addTo(map),
              currentMode: 'mapbox/driving'
            };
            
            // Add route group to layer control
            map.layerManager.addLayer(window.yhMap.routeGroup, 'overlay', 'Routes');
            
             // Add custom styles
            var style = document.createElement('style');
            style.textContent = `
              .leaflet-routing-container {
                background-color: white;
                padding: 10px;
                margin: 10px;
                border-radius: 8px;
                box-shadow: 0 2px 5px rgba(0,0,0,0.1);
                max-height: 400px;
                overflow-y: auto;
                display: none;
              }
              .leaflet-routing-container.show {
                display: block;
              }
              .leaflet-routing-alt {
                max-height: none !important;
              }
              .leaflet-routing-alt h2 {
                font-size: 16px;
                margin: 8px 0;
              }
              .leaflet-routing-alt h3 {
                font-size: 14px;
                margin: 6px 0;
              }
              .leaflet-routing-alt tr:hover {
                background-color: #f5f5f5;
              }
              .leaflet-routing-geocoder input {
                display: none;
              }
              .leaflet-routing-remove-waypoint {
                display: none;
              }
              .leaflet-routing-close {
                position: absolute;
                top: 5px;
                right: 5px;
                width: 24px;
                height: 24px;
                background: #fff;
                border: none;
                border-radius: 4px;
                cursor: pointer;
                box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                display: flex;
                align-items: center;
                justify-content: center;
              }
              .leaflet-routing-close:hover {
                background: #f0f0f0;
              }
              .mode-selector {
                position: absolute;
                top: 100px;
                left: 10px;
                background: white;
                padding: 5px;
                border-radius: 4px;
                box-shadow: 0 1px 3px rgba(0,0,0,0.2);
                z-index: 1000;
                display: none;
              }
              .mode-selector.show {
                display: block;
              }
              .mode-button {
                padding: 6px 12px;
                margin: 2px;
                border: none;
                border-radius: 4px;
                cursor: pointer;
                font-weight: bold;
                transition: background-color 0.2s;
              }
              .mode-button.active {
                background-color: #1a73e8;
                color: white;
              }
              .mode-button:not(.active) {
                background-color: #f0f0f0;
                color: #333;
              }
            `;
            document.head.appendChild(style);
            
            // Create mode selector
            var modeSelector = document.createElement('div');
            modeSelector.className = 'mode-selector';
            modeSelector.innerHTML = `
              <button class='mode-button active' data-mode='mapbox/driving'>🚗 Driving</button>
              <button class='mode-button' data-mode='mapbox/walking'>🚶 Walking</button>
              <button class='mode-button' data-mode='mapbox/cycling'>🚲 Cycling</button>
            `;
            map.getContainer().appendChild(modeSelector);
            
            
            // Add click handlers for mode buttons
            var modeButtons = modeSelector.getElementsByClassName('mode-button');
            for (var button of modeButtons) {
              button.addEventListener('click', function(e) {
                // Update active state
                for (var btn of modeButtons) {
                  btn.classList.remove('active');
                }
                this.classList.add('active');
                
                // Update current mode
                window.yhMap.currentMode = this.dataset.mode;
                
                // Recalculate route if exists
                if (window.yhMap.currentDestination) {
                  window.showRoute(
                    window.yhMap.currentDestination[0],
                    window.yhMap.currentDestination[1]
                  );
                }
              });
            }
            
            // Initialize routing control
            window.yhMap.routingControl = L.Routing.control({
              waypoints: [],
              router: L.Routing.mapbox('pk.eyJ1IjoiZ3h5MjExMSIsImEiOiJjbTMzYmlwYTgxOXkwMmpwdHZvOGpzMDB0In0.8oK_lsM_cieDesH8Tht8rg', {
                profile: 'mapbox/driving'
              }),
              lineOptions: {
                styles: [{color: '#1a73e8', weight: 6}]
              },
              createMarker: function() { return null; },
              addWaypoints: false,
              draggableWaypoints: false,
              routeWhileDragging: false,
              showAlternatives: false,
              show: true,
              collapsible: true,
              units: 'metric'
            }).addTo(map);
            
            // Add close button to routing container
            var container = window.yhMap.routingControl.getContainer();
            var closeButton = document.createElement('button');
            closeButton.className = 'leaflet-routing-close';
            closeButton.innerHTML = '✕';
            closeButton.onclick = function() {
              container.classList.remove('show');
              modeSelector.classList.remove('show');
              window.yhMap.routeGroup.clearLayers();
              window.yhMap.routingControl.setWaypoints([]);
              window.yhMap.currentDestination = null;
            };
            container.appendChild(closeButton);
            
            // Route event handlers
            window.yhMap.routingControl.on('routesfound', function(e) {
              window.yhMap.routeGroup.clearLayers();
              if (e.routes && e.routes.length > 0) {
                var path = L.polyline(e.routes[0].coordinates, {
                  color: '#1a73e8',
                  weight: 6
                });
                window.yhMap.routeGroup.addLayer(path);
                container.classList.add('show');
                modeSelector.classList.add('show');
                map.fitBounds(path.getBounds(), {padding: [50, 50]});
              }
            });
            
            // Global showRoute function
            window.showRoute = function(lat, lng) {
              var from = window.yhMap.carparkLocation;
              window.yhMap.currentDestination = [lat, lng];
              
              // Update router profile
              window.yhMap.routingControl.getRouter().options.profile = window.yhMap.currentMode;
              
              // Set waypoints
              window.yhMap.routingControl.setWaypoints([
                L.latLng(from[0], from[1]),
                L.latLng(lat, lng)
              ]);
            };
          }
        ", carpark$latitude, carpark$longitude))
      
      # Adding the distance boundary --------------------------------------------
      
      
      # Only add circle and attractions if distance is greater than 0
      if (input$distance > 0) {
        attractions <- nearby_attractions()
        
        # Add search radius circle
        map <- map %>%
          addCircles(
            lng = carpark$longitude,
            lat = carpark$latitude,
            radius = input$distance * 1000,  # Convert km to meters
            color = "#1a73e8",
            weight = 2,
            fillColor = "#1a73e8",
            fillOpacity = 0.1,
            group = "Search Radius"
          )
        
        # Adding markers for the tourist attractions ------------------------------
        
        
        # Add attraction markers only if there are any attractions
        if (nrow(attractions) > 0) {
          map <- map %>%
            addAwesomeMarkers(
              data = attractions,
              lng = ~longitude,
              lat = ~latitude,
              icon = poiIcon,
              popup = ~sprintf(
                paste0(
                  "<div style='min-width: 200px;'>",
                  "<h4 style='margin: 0 0 8px 0;'>%s</h4>",
                  "<p style='margin: 0 0 8px 0;'><b>Distance:</b> %.2f km</p>",
                  "<p style='margin: 0 0 8px 0;'>%s</p>",
                  "<button onclick='showRoute(%f, %f)' ",
                  "style='width: 100%%; padding: 8px; background-color: #1a73e8; ",
                  "color: white; border: none; border-radius: 4px; cursor: pointer; ",
                  "font-weight: bold;'>",
                  "Show Route</button>",
                  "</div>"
                ),
                PAGETITLE,
                distance_km,
                OVERVIEW,
                latitude,
                longitude
              )
            )
        }
      }
      map
    })
    
    observe({
      req(selected_carpark())
      cat("Carpark coordinates:", 
          selected_carpark()$latitude, 
          selected_carpark()$longitude, 
          "\n")
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
          longitude = numeric(),
          latitude = numeric(),
          stringsAsFactors = FALSE
        )
      }
    })
  })
}
