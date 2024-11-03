# Library -----------------------------------------------------------------
library(tidyverse)
library(vroom)

# Load Dataset ------------------------------------------------------------
load_datasets <- function() {
  # Load sales data
  sales <- read_csv("data/sales.csv") %>%
    mutate(
      date = as.Date(date),
      revenue = quantity * price
    )
  
  # Load inventory data
  inventory <- read_excel("data/inventory.xlsx") %>%
    mutate(
      last_updated = as.Date(last_updated)
    )
  
  list(
    sales = sales,
    inventory = inventory
  )
}