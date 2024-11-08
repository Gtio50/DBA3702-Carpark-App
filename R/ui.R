library(shiny)
library(leaflet)


# yhMap UI ----------------------------------------------------------------
yhMapUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tags$head(
      # Leaflet Routing Machine CSS and JS
      tags$link(rel = "stylesheet", href = "https://unpkg.com/leaflet-routing-machine@3.2.12/dist/leaflet-routing-machine.css"),
      tags$script(src = "https://unpkg.com/leaflet-routing-machine@3.2.12/dist/leaflet-routing-machine.js"),
      tags$script(src = "https://unpkg.com/leaflet-routing-machine@3.2.12/dist/leaflet-routing-mapbox.js"),
      
      # Custom CSS
      tags$style(HTML("
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
        
        /* Layout improvements */
        .main-content {
          display: flex;
          gap: 20px;
          margin-top: 20px;
          height: calc(100vh - 200px);
        }
        
        .sidebar {
          width: 250px;
          flex-shrink: 0;
          padding: 15px;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        .map-table-container {
          flex-grow: 1;
          display: flex;
          flex-direction: column;
          gap: 20px;
          height: 100%;
        }
        
        .map-container {
          height: 60%;
          border-radius: 8px;
          overflow: hidden;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        /* Table container styling */
        .table-container {
          height: 40%;
          background: white;
          border-radius: 8px;
          padding: 15px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          overflow: hidden;
          position: relative;
        }
        
         /* DataTables specific styling */
        #attraction_list {
          height: 100%;
        }
        
        /* DataTable specific styling */
        .dataTables_wrapper {
          position: absolute;
          top: 15px;
          left: 15px;
          right: 15px;
          bottom: 15px;
          max-height = 100%;
        }
        
        .dataTables_scroll {
          height: 100%;
        }
        
        .dataTables_scrollBody {
          max-height: 300pxs !important; /* Adjust this value - should be less than table-container height */
          overflow-y: auto !important;
        }
        
        /* Scrollbar styling */
        .dataTables_scrollBody::-webkit-scrollbar {
          width: 6px;
          height: 6px;
        }
        
        .dataTables_scrollBody::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 3px;
        }
        
        .dataTables_scrollBody::-webkit-scrollbar-thumb {
          background: #888;
          border-radius: 3px;
        }
        
        .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
          background: #555;
        }
        
        /* Table styling */
        .dataTable {
          width: 100% !important;
          margin: 0 !important;
          border-collapse: collapse;
        }
        
        .dataTable thead th {
          background: white;
          position: sticky;
          top: 0;
          z-index: 1;
          border-bottom: 2px solid #ddd;
        }
        
        .dataTable tbody tr:hover {
          background-color: #f5f5f5;
        }
        
        /* Leaflet popup styling */
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
      "))
    ),
    
    titlePanel("Points of Interests near a carpark"),
    
    # New flexible layout
    div(class = "main-content",
        # Sidebar
        div(class = "sidebar",
            dateInput(
              inputId = ns("date"), 
              "Date:", 
              value = Sys.Date()
            ),
            timeInput(
              inputId = ns("time"), 
              "Time:", 
              value = Sys.time()
            ),
            selectizeInput(
              inputId = ns("carpark"), 
              "Enter Carpark:", 
              choices = NULL, 
              options = list(
                placeholder = 'Name of carpark',
                maxItems = 1,
                create = TRUE
              )
            ),
            sliderInput(
              inputId = ns("distance"), 
              "Distance/km:", 
              min = 0, 
              max = 5, 
              value = 0
            )
        ),
        
        # Map and Table Container
        div(class = "map-table-container",
            # Map
            div(class = "map-container",
                leafletOutput(
                  outputId = ns("plotPOI"),
                  height = "100%"
                )
            ),
            # Table
            div(class = "table-container",
                DTOutput(
                  outputId = ns("attraction_list")
                )
            )
        )
    )
  )
}


