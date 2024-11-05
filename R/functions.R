library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(jsonlite)
library(sf)
library(shiny)
library(shinyTime)
library(leaflet)
library(leaflet.extras)
library(shinycssloaders)
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

# Time Series Plot --------------------------------------------------------
create_time_series_plot <- function(data, 
                                    date_col = "date", 
                                    value_col = "value",
                                    title = "Time Series Analysis",
                                    color_scheme = c("#2C3E50", "#E74C3C", "#3498DB")) {
  
  # Calculate trend using moving average
  ma_data <- data %>%
    arrange(!!sym(date_col)) %>%
    mutate(
      MA30 = rollmean(!!sym(value_col), k = 30, fill = NA, align = "right"),
      MA90 = rollmean(!!sym(value_col), k = 90, fill = NA, align = "right")
    )
  
  p <- ggplot(ma_data, aes(x = !!sym(date_col))) +
    geom_line(aes(y = !!sym(value_col), color = "Raw Data"), alpha = 0.5) +
    geom_line(aes(y = MA30, color = "30-day MA"), size = 1) +
    geom_line(aes(y = MA90, color = "90-day MA"), size = 1) +
    scale_color_manual(values = color_scheme) +
    theme_minimal() +
    labs(
      title = title,
      x = "Date",
      y = "Value",
      color = "Series"
    ) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggplotly(p)
}

create_correlation_heatmap <- function(data, 
                                       exclude_cols = c(),
                                       title = "Correlation Heatmap") {
  
  # Select numeric columns and calculate correlations
  numeric_data <- data %>%
    select_if(is.numeric) %>%
    select(-any_of(exclude_cols))
  
  corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  
  # Create heatmap
  plot_ly(
    x = colnames(corr_matrix),
    y = colnames(corr_matrix),
    z = corr_matrix,
    type = "heatmap",
    colorscale = "RdBu",
    zmin = -1,
    zmax = 1
  ) %>%
    layout(
      title = title,
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )
}

create_distribution_plot <- function(data,
                                     value_col,
                                     group_col = NULL,
                                     title = "Distribution Analysis") {
  
  if (is.null(group_col)) {
    p <- ggplot(data, aes(x = !!sym(value_col))) +
      geom_histogram(aes(y = ..density..), fill = "#3498DB", alpha = 0.7) +
      geom_density(color = "#E74C3C", size = 1) +
      geom_rug(alpha = 0.1)
  } else {
    p <- ggplot(data, aes(x = !!sym(value_col), fill = !!sym(group_col))) +
      geom_density(alpha = 0.5) +
      scale_fill_viridis_d()
  }
  
  p <- p +
    theme_minimal() +
    labs(
      title = title,
      x = value_col,
      y = "Density"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
  
  ggplotly(p)
}


# Box Plot ----------------------------------------------------------------
create_box_plot <- function(data,
                            value_col,
                            group_col,
                            title = "Comparative Analysis") {
  
  p <- ggplot(data, aes(x = !!sym(group_col), y = !!sym(value_col))) +
    geom_boxplot(fill = "#3498DB", alpha = 0.7) +
    geom_jitter(alpha = 0.2, width = 0.2) +
    theme_minimal() +
    labs(
      title = title,
      x = group_col,
      y = value_col
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  ggplotly(p)
}
# Decomposition Plot ------------------------------------------------------
create_decomposition_plot <- function(data,
                                      date_col = "date",
                                      value_col = "value",
                                      title = "Time Series Decomposition") {
  
  # Convert to time series object
  ts_data <- ts(data[[value_col]], 
                frequency = 7)  # Adjust frequency as needed
  
  # Decompose time series
  decomp <- decompose(ts_data)
  
  # Create plot data
  plot_data <- data.frame(
    date = data[[date_col]],
    observed = as.numeric(decomp$x),
    trend = as.numeric(decomp$trend),
    seasonal = as.numeric(decomp$seasonal),
    random = as.numeric(decomp$random)
  )
  
  # Create subplots
  p1 <- plot_ly(plot_data, x = ~date, y = ~observed, 
                type = "scatter", mode = "lines", name = "Observed")
  p2 <- plot_ly(plot_data, x = ~date, y = ~trend, 
                type = "scatter", mode = "lines", name = "Trend")
  p3 <- plot_ly(plot_data, x = ~date, y = ~seasonal, 
                type = "scatter", mode = "lines", name = "Seasonal")
  p4 <- plot_ly(plot_data, x = ~date, y = ~random, 
                type = "scatter", mode = "lines", name = "Random")
  
  subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE) %>%
    layout(title = title)
}
# Filter For Single Location -----------------------------------------------------
filter_single_location <- function(df, input_loc, address_col = "address"){
  address_col = enquo(address_col)
  df %>% filter(.data[[address_col]] == input_loc) %>%
    transmute(
      address = .data[[address_col]],
      latitude,
      longitude
    )
}
# Nearby POI --------------------------------------------------------------
all_nearby_poi <- function(current_loc, pois, cols = 
                             c("PAGETITLE", "ADDRESS", "OVERVIEW",
                               "distance_km", "longitude", "latitude")){
  current_coords <- c(current_loc$longitude, current_loc$latitude)
  smth <- pois %>%
    mutate(
      distance = geosphere::distHaversine(
        cbind(longitude, latitude),  
        matrix(current_coords, ncol = 2, byrow = TRUE)
      ),
      distance_km = round(distance / 1000, 2)
    ) %>%
    select(all_of(cols)
    )
}

# Leaflet Script Dependencies ---------------------------------------------
addScriptDependencies <- function(map) {
  map$dependencies <- c(
    map$dependencies,
    htmlDependency(
      "routing",
      "1.0.0",
      src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet-routing-machine/3.2.12/"),
      script = "leaflet-routing-machine.min.js",
      stylesheet = "leaflet-routing-machine.css"
    )
  )
  return(map)
}


