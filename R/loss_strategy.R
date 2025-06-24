# R/loss_strategy.R

library(tidyverse)

# Mortality Loss: Deaths × Remaining Life Expectancy × VSLY
calculate_mortality_loss <- function(deaths, life_expectancy, vsly) {
  return(sum(deaths * life_expectancy * vsly, na.rm = TRUE))
}

# Productivity Loss from Hospitalisation = Hospitalisations × Days × Wage
calculate_productivity_loss_hosp <- function(hospitalisations, hosp_days_lost, daily_wage) {
  return(sum(hospitalisations * hosp_days_lost * daily_wage, na.rm = TRUE))
}

# Symptomatic Infection Loss = Infections × Days × Wage
calculate_symptomatic_loss <- function(symptomatic_infections, symptomatic_days_lost, daily_wage) {
  return(sum(symptomatic_infections * symptomatic_days_lost * daily_wage, na.rm = TRUE))
}

# Long COVID Loss = Infections × Long COVID Prob × Days × Wage
calculate_long_covid_loss <- function(infections, long_covid_prob, long_covid_days, daily_wage) {
  return(sum(infections * long_covid_prob * long_covid_days * daily_wage, na.rm = TRUE))
}

# Informal Caregiving Loss = Infections (age <16) × Avg Days × Daily Wage
calculate_informal_care_loss <- function(pediatric_infections, care_days, daily_value_care) {
  return(sum(pediatric_infections * care_days * daily_value_care, na.rm = TRUE))
}

# Wrapper to calculate all losses and return a named tibble
calculate_losses <- function(df, params) {
  with(params, {
    mortality_loss <- calculate_mortality_loss(df$deaths, df$life_years_remaining, df$vsly)
    hosp_prod_loss <- calculate_productivity_loss_hosp(df$hospitalisations, hosp_days_lost, daily_wage)
    symptomatic_loss <- calculate_symptomatic_loss(df$symptomatic_infections, symptomatic_days_lost, daily_wage)
    long_covid_loss <- calculate_long_covid_loss(df$symptomatic_infections, long_covid_prob, long_covid_days, daily_wage)
    informal_care_loss <- calculate_informal_care_loss(df$pediatric_infections, care_days, daily_value_care)

    return(tibble(
      mortality_loss = mortality_loss,
      hosp_prod_loss = hosp_prod_loss,
      symptomatic_loss = symptomatic_loss,
      long_covid_loss = long_covid_loss,
      informal_care_loss = informal_care_loss,
      total_loss = sum(mortality_loss, hosp_prod_loss, symptomatic_loss, long_covid_loss, informal_care_loss, na.rm = TRUE)
    ))
  })
}
