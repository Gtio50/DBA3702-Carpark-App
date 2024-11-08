# load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(lubridate)

# load datasets
df <- read.csv("Project/carpark_data_resultsfivepercent.csv")
rainfall <- read.csv("Project/rainfall_data.csv")
carpark_info <- read.csv("Project/HDB Carpark Information.csv")

# merge dataset to get carpark info based on the carpark number
carpark_data <- left_join(df, carpark_info, by=c("carpark_number"="car_park_no"))

# filter for only between jan and dec 2024
# Convert update_datetime to POSIXct format if it's not already
carpark_data <- carpark_data %>%
  mutate(update_datetime = ymd_hms(update_datetime))

# Define the start and end dates
start_date <- ymd("2023-01-01")
end_date <- ymd("2023-12-31")

# Filter for records within the specified date range
carpark_data_2023 <- carpark_data %>%
  filter(update_datetime >= start_date & update_datetime <= end_date)


###################################################################################3
# Function to plot carpark availability by hour, week, or month
plot_carpark_availability <- function(data, time_interval = c("hourly", "weekly", "monthly")) {
  
  # Ensure time_interval is one of the specified values
  time_interval <- match.arg(time_interval)
  
  # Convert update_datetime to POSIXct format using ymd_hms
  data <- data %>%
    mutate(update_datetime = ymd_hms(update_datetime))
  
  # Check if the conversion succeeded
  if (any(is.na(data$update_datetime))) {
    stop("Date-time conversion failed for some entries. Please check the format of update_datetime.")
  }
  
  # Aggregate data based on the specified time interval
  if (time_interval == "hourly") {
    data_aggregated <- data %>%
      mutate(hour = floor_date(update_datetime, "hour")) %>%
      group_by(hour) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("hour")
    
  } else if (time_interval == "weekly") {
    data_aggregated <- data %>%
      mutate(week = as.Date(floor_date(update_datetime, "week"))) %>%  # Convert to Date
      group_by(week) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("week")
    
  } else if (time_interval == "monthly") {
    data_aggregated <- data %>%
      mutate(month = as.Date(floor_date(update_datetime, "month"))) %>%  # Convert to Date
      group_by(month) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("month")
  }
  
  # Plot the data
  plot <- ggplot(data_aggregated, aes(x = !!x_axis, y = avg_lots_available)) +
    geom_line(color = "blue") +
    labs(title = paste("Average Carpark Availability -", time_interval),
         x = time_interval,
         y = "Average Available Lots") +
    theme_minimal()
  
  # Adjust x-axis for monthly interval to display every month
  if (time_interval == "monthly") {
    plot <- plot +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  plot
}

# Example usage:
plot_carpark_availability(carpark_data_2023, time_interval = "weekly")

#######################################################################
# Function to plot carpark availability by hour, week, or month
plot_carpark_availability <- function(data, time_interval = c("hourly", "weekly", "monthly")) {
  
  # Ensure time_interval is one of the specified values
  time_interval <- match.arg(time_interval)
  
  # Convert update_datetime to POSIXct format using ymd_hms
  data <- data %>%
    mutate(update_datetime = ymd_hms(update_datetime))
  
  # Check if the conversion succeeded
  if (any(is.na(data$update_datetime))) {
    stop("Date-time conversion failed for some entries. Please check the format of update_datetime.")
  }
  
  # Aggregate data based on the specified time interval
  if (time_interval == "hourly") {
    data_aggregated <- data %>%
      mutate(hour = hour(update_datetime)) %>%  # Extract hour of day
      group_by(hour) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    
    # Convert the hour column to a factor with AM/PM labels
    data_aggregated <- data_aggregated %>%
      mutate(hour_label = factor(hour, levels = 0:23, 
                                 labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
                                            "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
                                            "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", 
                                            "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")))
    x_axis <- sym("hour_label")
    
  } else if (time_interval == "weekly") {
    data_aggregated <- data %>%
      mutate(week = as.Date(floor_date(update_datetime, "week"))) %>%  # Convert to Date
      group_by(week) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("week")
    
  } else if (time_interval == "monthly") {
    data_aggregated <- data %>%
      mutate(month = as.Date(floor_date(update_datetime, "month"))) %>%  # Convert to Date
      group_by(month) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("month")
  }
  
  # Plot the data
  if (time_interval == "hourly") {
    # Use geom_col() for hourly to show distinct averages per hour
    plot <- ggplot(data_aggregated, aes(x = !!x_axis, y = avg_lots_available)) +
      geom_col(fill = "steelblue") +
      labs(title = "Average Carpark Availability by Hour of Day",
           x = "Hour of Day",
           y = "Average Available Lots") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    # Use geom_line() and geom_smooth() for weekly and monthly
    plot <- ggplot(data_aggregated, aes(x = !!x_axis, y = avg_lots_available)) +
      geom_line(color = "blue") +
      geom_smooth(method = "loess", color = "red", se = FALSE) +  # Add trendline
      labs(title = paste("Average Carpark Availability -", time_interval),
           x = time_interval,
           y = "Average Available Lots") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Monthly x-axis adjustments for better readability
    if (time_interval == "monthly") {
      plot <- plot +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
    }
  }
  
  plot
}

# Example usage:
plot_carpark_availability(carpark_data_2023, time_interval = "hourly")
plot_carpark_availability(carpark_data_2023, time_interval = "weekly")
plot_carpark_availability(carpark_data_2023, time_interval = "monthly")

##############################################################################################
# function which includes carpark number (users should be able to choose location from dropdown)
# Function to plot carpark availability by hour, week, or month for a specific carpark location
plot_carpark_availability <- function(data, time_interval = c("hourly", "weekly", "monthly"), carpark_number = NULL) {
  
  # Ensure time_interval is one of the specified values
  time_interval <- match.arg(time_interval)
  
  # Filter by carpark number if provided
  if (!is.null(carpark_number)) {
    data <- data %>% filter(carpark_number == carpark_number)
    # Check if any data is available after filtering
    if (nrow(data) == 0) {
      stop("No data available for the specified carpark number.")
    }
  }
  
  # Convert update_datetime to POSIXct format using ymd_hms
  data <- data %>%
    mutate(update_datetime = ymd_hms(update_datetime))
  
  # Check if the conversion succeeded
  if (any(is.na(data$update_datetime))) {
    stop("Date-time conversion failed for some entries. Please check the format of update_datetime.")
  }
  
  # Aggregate data based on the specified time interval
  if (time_interval == "hourly") {
    data_aggregated <- data %>%
      mutate(hour = hour(update_datetime)) %>%  # Extract hour of day
      group_by(hour) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    
    # Convert the hour column to a factor with AM/PM labels
    data_aggregated <- data_aggregated %>%
      mutate(hour_label = factor(hour, levels = 0:23, 
                                 labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", 
                                            "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
                                            "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", 
                                            "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")))
    x_axis <- sym("hour_label")
    
  } else if (time_interval == "weekly") {
    data_aggregated <- data %>%
      mutate(week = as.Date(floor_date(update_datetime, "week"))) %>%  # Convert to Date
      group_by(week) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("week")
    
  } else if (time_interval == "monthly") {
    data_aggregated <- data %>%
      mutate(month = as.Date(floor_date(update_datetime, "month"))) %>%  # Convert to Date
      group_by(month) %>%
      summarise(avg_lots_available = mean(lots_available, na.rm = TRUE))
    x_axis <- sym("month")
  }
  
  # Plot the data
  if (time_interval == "hourly") {
    # Use geom_col() for hourly to show distinct averages per hour
    plot <- ggplot(data_aggregated, aes(x = !!x_axis, y = avg_lots_available)) +
      geom_col(fill = "steelblue") +
      labs(title = paste("Average Carpark Availability by Hour of Day", if (!is.null(carpark_number)) paste("for Carpark", carpark_number)),
           x = "Hour of Day",
           y = "Average Available Lots") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    # Use geom_line() and geom_smooth() for weekly and monthly
    plot <- ggplot(data_aggregated, aes(x = !!x_axis, y = avg_lots_available)) +
      geom_line(color = "blue") +
      geom_smooth(method = "loess", color = "red", se = FALSE) +  # Add trendline
      labs(title = paste("Average Carpark Availability -", time_interval, if (!is.null(carpark_number)) paste("for Carpark", carpark_number)),
           x = time_interval,
           y = "Average Available Lots") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Monthly x-axis adjustments for better readability
    if (time_interval == "monthly") {
      plot <- plot +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
    }
  }
  
  plot
}

# Example usage:
plot_carpark_availability(carpark_data_2023, time_interval = "hourly", carpark_number = "BP1")
plot_carpark_availability(carpark_data_2023, time_interval = "weekly", carpark_number = "J55")
plot_carpark_availability(carpark_data_2023, time_interval = "monthly", carpark_number = "MN2")

#############################################################################################################

