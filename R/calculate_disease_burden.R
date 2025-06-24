# R/calculate_disease_burden.R
library(tidyverse)

calculate_disease_burden <- function(df, infection_rate, vaccine_uptake, efficacy_symptomatic, p_symptomatic = 0.67, long_covid_rate = 0.133) {
  df %>%
    mutate(
      total_infections = n * infection_rate,
      symptomatic_infections = total_infections * p_symptomatic * (1 - vaccine_uptake * efficacy_symptomatic),
      deaths = symptomatic_infections * ifr,  # now vaccine-responsive
      hospitalisations = symptomatic_infections * prob_hosp,  # now vaccine-responsive
      pediatric_symptomatic = if_else(age_group %in% c("0-4", "5-9"), symptomatic_infections, 0)
    )
}
