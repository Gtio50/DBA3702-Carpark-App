# Library -----------------------------------------------------------------
library(vroom)
library(data.table)
library(dplyr)
library(sf)
library(geodist)

# Reading Data ------------------------------------------------------------
carpark_raw <- setDT(vroom("carpark_data_resultsfivepercent.csv", col_types = "_cTncn"))
carpark_info <- setDT(vroom("HDB Carpark Information.csv", col_types = "ccnncccccnnc"))
humidity <- setDT(vroom("humidity_data.csv", col_types = "_ccnnnT"))
rainfall <- setDT(vroom("rainfall_data.csv", col_types = "_cnnnT"))
temp <- setDT(vroom("temperature_data.csv", col_types = "_ccnnnT"))
wind <- setDT(vroom("windspeed_data.csv", col_types = "_ccnnnT"))

# Data Cleaning -----------------------------------------------------------
setnames(rainfall, c("Longtitude", "Latitude"), c("longtitude", "latitude"))
setnames(temp, "temperature", "value")
unique_cp <- unique(carpark_raw$carpark_number)
unique_cp_df <- setDT(data.frame(id = unique_cp))
cp_without_loc <- unique_cp[!unique_cp %in% carpark_info$car_park_no]
carpark <- carpark_raw[!carpark_number %in% cp_without_loc]
unique_cp_df <- unique_cp_df[!id %in% cp_without_loc]

# Merge Carpark and Weather data ------------------------------------------
#merge x and y coord to carpark id
cp_locations <- merge(unique_cp_df, carpark_info[, .(car_park_no, x_coord, y_coord)],
                      by.x = "id", by.y = "car_park_no", all.x = T)

#convert x and y coord to longtitude and latitude
cp_locations_sf <- st_as_sf(cp_locations, 
                            coords = c("x_coord", "y_coord"), 
                            crs = 3414)
cp_locations_transformed <- st_transform(cp_locations_sf, crs = 4326)
cp_locations[, c("longtitude", "latitude") := as.data.table(st_coordinates(cp_locations_transformed))]
cp_locations[, c("x_coord", "y_coord") := NULL]

#function for measuring dist
closest_st <- function(carpark, weather){
  nearest_indices <- mapply(
    function(src_lon, src_lat){
      which.min(geodist(
        data.frame(x = src_lon, y = src_lat),
        data.frame(
          x = weather$longtitude, 
          y = weather$latitude
        ),
        measure = "haversine"
      ))
    },
    carpark$longtitude, 
    carpark$latitude
  )

  return(weather$id[nearest_indices])
}

cp_locations$humidity_st <- closest_st(cp_locations, humidity %>% distinct(id, .keep_all = T))
cp_locations$rainfall_st <- closest_st(cp_locations, rainfall %>% distinct(id, .keep_all = T))
cp_locations$temp_st <- closest_st(cp_locations, temp %>% distinct(id, .keep_all = T))
cp_locations$wind_st <- closest_st(cp_locations, wind %>% distinct(id, .keep_all = T))

carpark_merge <- merge(carpark, cp_locations, by.x = "carpark_number", by.y = "id",
                       all.x = T)

carpark_merge_val <- carpark_merge %>% 
  left_join(humidity[, .(id, value, datetime)], 
            by = c("humidity_st" = "id", "update_datetime" = "datetime")) %>%
  rename('humidity' = 'value') %>%
  left_join(rainfall[, .(id, value, datetime)], 
            by = c("rainfall_st" = "id", "update_datetime" = "datetime")) %>%
  rename('rainfall' = 'value') %>%
  left_join(temp[, .(id, value, datetime)], 
            by = c("temp_st" = "id", "update_datetime" = "datetime")) %>%
  rename('temperature' = 'value') %>%
  left_join(wind[, .(id, value, datetime)], 
            by = c("wind_st" = "id", "update_datetime" = "datetime")) %>%
  rename('wind' = 'value')

carpark_merge_final <- carpark_merge_val[, c("longtitude", "latitude", "humidity_st",
                                             "rainfall_st", "temp_st", "wind_st") := NULL]
setnames(carpark_merge_final, "update_datetime", "datetime")
carpark_merge_final_cleaned <- na.omit(carpark_merge_final)

write.csv(carpark_merge_final_cleaned, "merged_data.csv")

