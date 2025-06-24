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

  # --- Manual IFR vector (17 age groups) ---
  # Infection Fatality Ratio (IFR) values adapted from Verity et al. (2020)
  # Source: Verity R et al. (2020), Lancet Infect Dis, doi:10.1016/S1473-3099(20)30243-7
  # Used in the Squire model: squire::parameters_explicit_SEEIR("GBR")$ifr
  ifr_vector <- c(0, 0.01, 0.01, 0.02, 0.03, 0.04, 0.06, 0.10,
                  0.16, 0.24, 0.38, 0.60, 0.94, 1.47, 2.31, 3.61, 8.82) / 100

  # --- Merge into final df ---
  df <- uk_pop_df %>%
    left_join(vsly_country %>% select(age_group, vsly), by = "age_group") %>%
    left_join(life_expectancy_country %>% select(age_group, life_years_remaining), by = "age_group") %>%
    mutate(
      life_years_remaining = case_when(
        age_group == "5-9" ~ 78,
        age_group == "10-14" ~ 73,
        TRUE ~ life_years_remaining
      ),
      prob_hosp = squire_params$prob_hosp[1:17],
      ifr = ifr_vector
    )

  return(df)
}
