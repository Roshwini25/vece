# R/cost_strategy.R

library(tidyverse)

# Function to calculate cost of vaccination programme
calculate_vaccine_cost <- function(population, uptake, cost_vaccine, cost_delivery) {
  return(sum((cost_vaccine + cost_delivery) * population * uptake, na.rm = TRUE))
}

