# R/calculate_disease_burden.R

library(tidyverse)

calculate_disease_burden <- function(df, infection_rate, vaccine_uptake, efficacy_symptomatic, p_symptomatic = 0.67, long_covid_rate = 0.133) {
  df %>%
    mutate(
      total_infections = n * infection_rate,
      symptomatic_infections = total_infections * p_symptomatic * (1 - vaccine_uptake * efficacy_symptomatic),
      deaths = symptomatic_infections * ifr,
      hospitalisations = symptomatic_infections * ihr,
      pediatric_symptomatic = if_else(age_group %in% c("0-4", "5-9"), symptomatic_infections, 0)
    ) %>%
    # Explicitly retain all original columns
    select(
      age_group, n, ifr, ihr, vsly, life_years_remaining, daily_wage,
      total_infections, symptomatic_infections, deaths,
      hospitalisations, pediatric_symptomatic
    )
}
