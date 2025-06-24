# R/calculate_disease_burden.R
library(tidyverse)

calculate_disease_burden <- function(df, infection_rate, vaccine_uptake, efficacy_symptomatic, p_symptomatic = 0.67, long_covid_rate = 0.133) {
  df %>%
    mutate(
      total_infections = n * infection_rate,
      symptomatic_infections = total_infections * p_symptomatic * (1 - vaccine_uptake * efficacy_symptomatic),
      averted_symptomatic = total_infections * p_symptomatic * vaccine_uptake * efficacy_symptomatic,
      averted_hosp_cases = total_infections * prob_hosp * vaccine_uptake * efficacy_symptomatic,
      averted_long_covid_cases = total_infections * long_covid_rate * vaccine_uptake * efficacy_symptomatic,
      averted_deaths = total_infections * ifr * vaccine_uptake * efficacy_symptomatic,
      deaths = averted_deaths,
      hospitalisations = total_infections * prob_hosp,
      pediatric_infections = if_else(age_group %in% c("0-4", "5-9", "10-14"), total_infections, 0)
    )
}

