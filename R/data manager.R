# Library -----------------------------------------------------------------
library(tidyverse)
library(vroom)
library(rstudioapi)
library(data.table)

# Load Dataset ------------------------------------------------------------
load_datasets <- function() {
  #Load Carpark Information data
  carpark_info <- setDT(vroom("Data/carpark_info.csv", col_types = "_cccccccnncnn")) %>%
    .[, night_parking := factor(night_parking, levels = c("YES", "NO"))] %>%
    .[, car_park_basement := ifelse(car_park_basement == "Y", "YES", "NO")] %>%
    .[, car_park_basement := factor(car_park_basement)] %>%
    .[, c("car_park_type", "type_of_parking_system", "short_term_parking", "free_parking") :=
        lapply(.SD, factor), .SDcols = c("car_park_type", "type_of_parking_system", "short_term_parking", "free_parking")] %>%
    .[, setnames(.SD, old = c("longtitude"), new = c("longitude"))]
  
  # Load Carpark data
  merged_carpark <- setDT(vroom("Data/carpark_merged_data.csv", col_types = "_cTncnnnnn"))
  
  # Load Tourist data
  tourist <- setDT(vroom("Data/tourist_attractions.csv", col_types = "_cccccnn",
                         locale = vroom::locale(encoding = "UTF-8"))) %>%
    .[, PAGETITLE := gsub("â€™", "'", PAGETITLE)] %>%
    .[, PAGETITLE := gsub("â€“", "", PAGETITLE)] %>%
    .[, PAGETITLE := gsub("Â®", "", PAGETITLE)] %>%
    .[, PAGETITLE := gsub("â„¢", "", PAGETITLE)] %>%
    .[, OVERVIEW := gsub("\\^aEURTM", "'", OVERVIEW)] %>%
    .[, setnames(.SD, old = c("Longtitude", "Latitude"), new = c("longitude", "latitude"))]
    
  list(
    carpark_info = carpark_info,
    merged_carpark = merged_carpark,
    tourist = tourist
  )
}