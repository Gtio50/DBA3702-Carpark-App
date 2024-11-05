# Load required libraries
library(shiny)
library(dplyr)
library(data.table)
library(httr)
library(RcppSimdJson)
library(DT)
library(stringr)

# API URL
url <- "https://api.data.gov.sg/v1/transport/carpark-availability"

# Fetch and parse carpark data
fetch_carpark_data <- function() {
  raw_data <- GET(url)
  data <- RcppSimdJson::fparse(raw_data$content)
  
  # Extract carpark data and convert to data.table
  carpark_data <- setDT(data$items$carpark_data[[1]])
  
  # Flatten carpark_info
  carpark_data$total_lots <- sapply(carpark_data$carpark_info, `[[`, 1)
  carpark_data$lot_type <- sapply(carpark_data$carpark_info, `[[`, 2)
  carpark_data$lots_available <- sapply(carpark_data$carpark_info, `[[`, 3)
  
  # Unnest and clean up
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

# Paths to your CSV files
carpark_info_path <- "HDB Carpark Information.csv"
carpark_info <- read_csv(carpark_info_path)

# Load unique carpark locations based on car_park_no and address
unique_carparks <- carpark_info %>%
  select(car_park_no, address) %>%
  distinct()

# Define the classify_planning_area function
classify_planning_area <- function(address) {
  address <- toupper(address)
  area_patterns <- list(
    "BISHAN" = "\\bBISHAN|SIN MING|PINE LANE|SHUNFU ROAD|BRIGHT HILL\\b",
    "BUKIT MERAH" = "\\bBUKIT MERAH|HENDERSON|TELOK BLANGAH|DEPOT|REDHILL|JALAN BUKIT HO SWEE|HOY FATT|TIONG BAHRU|KIM TIAN|TIONG POH|SENG POH|JALAN RUMAH TINGGI|GUAN CHUAN|KIM PONG ROAD|LENGKOK BAHRU|KAMPONG BAHRU|BUKIT PURMEI|ENG WATT|ENG HOON|CHAY YAN|MOH GUAN|PENG NGUAN|YONG SIAK|DELTA|JALAN MEMBINA|CLARENCE LANE|ZION ROAD|HAVELOCK ROAD|GANGES AVENUE|BEO CRESCENT|INDUS ROAD|TAMAN HO SWEE\\b",
    "BUKIT TIMAH" = "\\bBUKIT TIMAH|PARK CRESCENT|CLEMENTI RD|SIXTH AVENUE|DUNEARN|JALAN JURONG KECHIL|TOH YI|QUEEN'S ROAD|EMPRESS ROAD\\b",
    "CENTRAL AREA" = "\\bORCHARD|CLARKE QUAY|MARINA BAY|SHENTON|BRAS BASAH|ALBERT|BENCOLEN|KRETA AYER|WATERLOO|BEACH ROAD|SELEGIE\\b",
    "DOWNTOWN CORE" = "\\bRAFFLES PLACE|MARINA BOULEVARD|ROBINSON|BEACH ROAD|CLARKE QUAY|MARINA BAY\\b",
    "GEYLANG" = "\\bGEYLANG|ALJUNIED|SIMS|EUNOS|JOO CHIAT|LORONG LIMAU|POTONG PASIR|JALAN DUA|JALAN TIGA|JALAN EMPAT|JALAN SATU|OLD AIRPORT ROAD|PINE CLOSE|DAKOTA CRESCENT\\b",
    "KALLANG" = "\\bKALLANG|MAUDE|KING GEORGE AVENUE|JELLICOE|JALAN AYER|JALAN BATU| BOON KENG|BENDEMEER|GEYLANG BAHRU|LAVENDER|CRAWFORD|JALAN BESAR|JALAN SULTAN|TOWNER|KIM KEAT|JALAN MA'MOR|ST MICHAEL|DORSET\\b",
    "MARINA EAST" = "\\bMARINA EAST\\b",
    "MARINA SOUTH" = "\\bMARINA SOUTH\\b",
    "MARINE PARADE" = "\\bMARINE PARADE|MARINE TERRACE|KAMPONG ARANG|KAMPONG KAYU|MARINE DRIVE|EAST COAST|SIGLAP|HAIG ROAD|CASSIA CRESCENT|MARINE CRESCENT\\b",
    "MUSEUM" = "\\bMUSEUM|BRAS BASAH\\b",
    "NEWTON" = "\\bNEWTON|DURHAM|KENT ROAD|GLOUCESTER\\b",
    "NOVENA" = "\\bNOVENA|THOMSON|MOULMEIN|BALESTIER|JALAN DUSUN|CAMBRIDGE|GLOUCESTER|SAINT MICHAEL\\b",
    "ORCHARD" = "\\bORCHARD|SCOTTS\\b",
    "OUTRAM" = "\\bOUTRAM|SPOONER ROAD|SAGO LANE|JALAN MINYAK|JALAN KUKOH|TELOK AYER|SPOTTISWOODE|EVERTON|BOON TIONG|KRETA AYER|CANTONMENT|YORK HILL|CHIN SWEE|TANJONG PAGAR|UPPER CROSS\\b",
    "QUEENSTOWN" = "\\bQUEENSTOWN|COMMONWEALTH|MEI LING|QUEENS CLOSE|STIRLING|ALEXANDRA|DAWSON|MARGARET DRIVE|GHIM MOH|STRATHMORE|QUEENSWAY|HOLLAND|MEI CHIN|QUEEN'S CLOSE|NORTH BUONA VISTA\\b",
    "RIVER VALLEY" = "\\bRIVER VALLEY|MOHAMED SULTAN|KIM SENG\\b",
    "ROCHOR" = "\\bROCHOR|BENCOOLEN|ALBERT|SUNGEI|WATERLOO|SELEGIE|CHENG YAN|NORTH BRIDGE|CIRCUIT | VEERASAMY|KELANTAN|RACE COURSE|CHANDER|ROWELL|PERUMAL|BUFFALO|KLANG|FRENCH|HONG LIM\\b",
    "SINGAPORE RIVER" = "\\bSINGAPORE RIVER\\b",
    "SOUTHERN ISLANDS" = "\\bSOUTHERN ISLANDS\\b",
    "STRAITS VIEW" = "\\bSTRAITS VIEW\\b",
    "TANGLIN" = "\\bTANGLIN|DEMPSEY|NASSIM|FARRER|EMPEROR\\b",
    "TOA PAYOH" = "\\bTOA PAYOH|LORONG 8 INDUSTRIAL PARK|BRADDELL|JALAN RAJA|JOO SENG|JALAN TENTERAM|WHAMPOA|JALAN BAHAGIA|BALAM|PIPIT|MACPHERSON|MCNAIR|BIDADARI|WOODLEIGH|KIM KEAT|SAINT GEORGE|ALKAFF\\b",
    
    # East Region
    "BEDOK" = "\\bBEDOK|NEW UPPER CHANGI|SIGLAP|LENGKONG TIGA|JALAN TENAGA|JALAN DAMAI|CHAI CHEE|BEDOK SOUTH|BEDOK SOUTH AVENUE 2 | BED0K SOUTH\\b",
    "CHANGI" = "\\bCHANGI|LOYANG|TANAH MERAH|TELOK PAKU\\b",
    "CHANGI BAY" = "\\bCHANGI BAY\\b",
    "PASIR RIS" = "\\bPASIR RIS|ELIAS\\b",
    "PAYA LEBAR" = "\\bPAYA LEBAR|UBI|EUNOS\\b",
    "SIMEI" = "\\bSIMEI\\b",
    "TAMPINES" = "\\bTAMPINES|BEDOK RESERVOIR\\b",
    
    # North Region
    "CENTRAL WATER CATCHMENT" = "\\bCENTRAL WATER CATCHMENT\\b",
    "LIM CHU KANG" = "\\bLIM CHU KANG|NEO TIEW\\b",
    "MANDAI" = "\\bMANDAI\\b",
    "SEMBAWANG" = "\\bSEMBAWANG|CANBERRA|WELLINGTON|MONTREAL\\b",
    "SIMPANG" = "\\bSIMPANG\\b",
    "SUNGEI KADUT" = "\\bSUNGEI KADUT\\b",
    "WOODLANDS" = "\\bWOODLANDS|MARSILING|ADMIRALTY|CHAMPIONS\\b",
    "YISHUN" = "\\bYISHUN\\b",
    
    # North-East Region
    "ANG MO KIO" = "\\bANG MO KIO\\b",
    "HOUGANG" = "\\bHOUGANG|DEFU|KOVAN|UPPER SERANGOON|BUANGKOK\\b",
    "NORTH-EASTERN ISLANDS" = "\\bNORTH-EASTERN ISLANDS\\b",
    "PUNGGOL" = "\\bPUNGGOL|PLANTATION CRESCENT|EDGEDALE|EDGEFIELD WALK|SUMANG|NORTHSHORE|WATERWAY|EDGEFIELD PLAINS\\b",
    "SELETAR" = "\\bSELETAR|WEST CAMP\\b",
    "SENGKANG" = "\\bSENGKANG|ANCHORVALE|COMPASSVALE|RIVERVALE|FERNVALE|JALAN KAYU\\b",
    "SERANGOON" = "\\bSERANGOON|LORONG LEW LIAN|LORONG AH SOO\\b",
    
    # West Region
    "BOON LAY" = "\\bBOON LAY|JURONG WEST\\b",
    "BUKIT BATOK" = "\\bBUKIT BATOK|BT BATOK\\b",
    "BUKIT PANJANG" = "\\bBUKIT PANJANG|BANGKIT|FAJAR|PENDING|JELAPANG|SENJA|PETIR|LOMPANG|SEGAR|CASHEW|GANGSA|JELEBU\\b",
    "CHOA CHU KANG" = "\\bCHOA CHU KANG|CCK|TECK WHYE|KEAT HONG\\b",
    "CLEMENTI" = "\\bCLEMENTI|WEST COAST|DOVER|SUNSET WAY|NORTH BUONA VISTA\\b",
    "JURONG EAST" = "\\bJURONG EAST|TOH GUAN|INTERNATIONAL|PANDAN|JURONG GATEWAY\\b",
    "JURONG WEST" = "\\bJURONG WEST|KANG CHING ROAD|HO CHING ROAD|YUAN CHING ROAD|BOON LAY|UPPER JURONG|CHIN BEE|TAH CHING|YUNG|CORPORATION|TEBAN|YUNG HO|YUNG PING|YUNG AN|YUNG SHENG\\b",
    "PIONEER" = "\\bPIONEER|JALAN AHMAD IBRAHIM|BENOI\\b",
    "TENGAH" = "\\bTENGAH\\b",
    "TUAS" = "\\bTUAS\\b",
    "WESTERN ISLANDS" = "\\bWESTERN ISLANDS\\b",
    "WESTERN WATER CATCHMENT" = "\\bWESTERN WATER CATCHMENT\\b"
  )
  for (area in names(area_patterns)) {
    if (str_detect(address, area_patterns[[area]])) {
      return(area)
    }
  }
  return(NA)
}

# Classify each carpark address into a planning area
carpark_info <- carpark_info %>%
  mutate(planning_area = sapply(address, classify_planning_area))

# Combine Real-Time and Static Data
get_combined_carpark_data <- function() {
  api_data <- fetch_carpark_data()
  
  # Join API data with static carpark info based on carpark number
  combined_data <- unique_carparks %>%
    inner_join(api_data, by = c("car_park_no" = "carpark_number")) %>%
    left_join(carpark_info, by = "car_park_no")
  
  # Add vacancy percentage and arrange by it
  combined_data <- combined_data %>%
    mutate(vacancy_percentage = pmin((lots_available / total_lots) * 100, 100)) %>%
    arrange(desc(vacancy_percentage))
  
  return(combined_data)
}

# Shiny App UI and Server
ui <- fluidPage(
  titlePanel("Singapore HDB Carpark Availability by Area"),
  sidebarLayout(
    sidebarPanel(
      selectInput("area", "Select Planning Area:", choices = c("All", unique(carpark_info$planning_area)), selected = "All")
    ),
    mainPanel(
      DTOutput("carpark_table")
    )
  )
)

server <- function(input, output) {
  carpark_data <- reactive({
    data <- get_combined_carpark_data()
    
    if (input$area != "All") {
      data <- data %>% filter(planning_area == input$area)
    }
    
    # Select only relevant columns for display
    data <- data %>% select(car_park_no, planning_area, address.x,total_lots, lots_available, free_parking, vacancy_percentage)
    data
  })
  
  # Render interactive table
  output$carpark_table <- renderDT({
    datatable(
      carpark_data(),
      options = list(pageLength = 15, autoWidth = TRUE),
      rownames = FALSE
    ) %>%
      formatRound('vacancy_percentage', 2) %>%  # Round to 2 decimal places
      formatStyle(
        'vacancy_percentage',
        backgroundColor = styleInterval(c(25, 50, 75), c("red", "orange", "yellow", "green"))
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
