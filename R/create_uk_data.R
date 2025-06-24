# R/create_UK_data.R

library(tidyverse)
library(squire)

create_UK_data <- function(iso3c = "GBR", country_name = "United Kingdom") {
  # --- Load population and Squire parameters ---
  uk_pop_raw <- get_population(iso3c = iso3c)
  squire_params <- parameters_explicit_SEEIR(country = country_name)

  # --- Collapse population to 17 age groups ---
  uk_pop_df <- uk_pop_raw %>%
    group_by(age_group) %>%
    summarise(n = sum(n), .groups = "drop")

  # --- Create new lookup table for IFR, IHR, and wage ---
  risk_wage_lookup <- tibble(
    age_group = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+"),

    ifr = c(0.000000153, 0.000000153, 0.000000153, 0.00000633, 0.00000633, 0.00000633, 0.00000633,
            0.0000155, 0.0000155, 0.0000738, 0.0000738, 0.000236, 0.000236, 0.00107, 0.00107,
            0.00725, 0.00725),

    ihr = c(0.000226, 0.000226, 0.000226, 0.000317, 0.000317, 0.000317, 0.000317,
            0.0004, 0.0004, 0.000763, 0.000763, 0.00208, 0.00208, 0.00686, 0.00686,
            0.0328, 0.0328),

    daily_wage = c(0, 0, 0, 18, 44, 88, 88, 110, 118, 118, 112, 112, 87, 87, 87, 87, 87)
  )

  # --- Load and filter inputs ---
  vsly_df <- readRDS("analysis/data-derived/vsly.rds") %>% ungroup()
  life_expectancy <- read_csv("analysis/tables/life_expectancy.csv", show_col_types = FALSE)

  # --- Filter for selected country ---
  vsly_country <- vsly_df %>%
    filter(iso3c == iso3c) %>%
    distinct(age_group, .keep_all = TRUE)

  life_expectancy_country <- life_expectancy %>%
    filter(iso3c == iso3c) %>%
    rename(life_years_remaining = lg) %>%
    distinct(age_group, .keep_all = TRUE)

  # --- Harmonise age groups ---
  harmonise_age_groups <- function(df) {
    df %>%
      mutate(age_group = recode(age_group,
                                "0-5" = "0-4", "5-10" = "5-9", "10-15" = "10-14", "15-20" = "15-19",
                                "20-25" = "20-24", "25-30" = "25-29", "30-35" = "30-34", "35-40" = "35-39",
                                "40-45" = "40-44", "45-50" = "45-49", "50-55" = "50-54", "55-60" = "55-59",
                                "60-65" = "60-64", "65-70" = "65-69", "70-75" = "70-74", "75-80" = "75-79",
                                "5-Oct" = "5-9", "Oct-15" = "10-14"
      ))
  }

  vsly_country <- harmonise_age_groups(vsly_country)
  life_expectancy_country <- harmonise_age_groups(life_expectancy_country)

  # --- Merge into final df ---
  df <- uk_pop_df %>%
    left_join(vsly_country %>% select(age_group, vsly), by = "age_group") %>%
    left_join(life_expectancy_country %>% select(age_group, life_years_remaining), by = "age_group") %>%
    left_join(risk_wage_lookup, by = "age_group") %>%
    mutate(
      life_years_remaining = case_when(
        age_group == "5-9" ~ 78,
        age_group == "10-14" ~ 73,
        TRUE ~ life_years_remaining
      )
    )
  return(df)
}
