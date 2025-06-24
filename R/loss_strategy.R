# R/loss_strategy.R

library(tidyverse)

# Mortality Loss: Deaths × Remaining Life Expectancy × VSLY
calculate_mortality_loss <- function(deaths, life_expectancy, vsly) {
  return(sum(deaths * life_expectancy * vsly, na.rm = TRUE))
}

# Productivity Loss from Hospitalisation = Hospitalisations × Days × Wage
calculate_productivity_loss_hosp <- function(hospitalisations, los, post_hosp_days, daily_wage) {
  total_days_lost <- los + post_hosp_days
  wage_safe <- ifelse(is.na(daily_wage), 0, daily_wage)
  return(sum(hospitalisations * total_days_lost * wage_safe, na.rm = TRUE))
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
calculate_informal_care_loss <- function(pediatric_symptomatic, care_days, daily_wage) {
  return(sum(pediatric_symptomatic * care_days * daily_wage, na.rm = TRUE))
}

# Hospital Burden = Hospitalisations × Avg Length of Stay × Cost per Day
calculate_hospital_burden <- function(hospitalisations, los, cost_per_day) {
  return(sum(hospitalisations * los * cost_per_day, na.rm = TRUE))
}

# Wrapper to calculate all losses and return a named tibble
calculate_losses <- function(df, params) {
  with(params, {
    mortality_loss <- calculate_mortality_loss(df$deaths, df$life_years_remaining, df$vsly)
    hosp_prod_loss <- calculate_productivity_loss_hosp(df$hospitalisations, los, post_hosp_days, df$daily_wage)
    symptomatic_loss <- calculate_symptomatic_loss(df$symptomatic_infections, symptomatic_days_lost, df$daily_wage)
    long_covid_loss <- calculate_long_covid_loss(df$symptomatic_infections, long_covid_prob, long_covid_days, df$daily_wage)
    informal_care_loss <- calculate_informal_care_loss(df$pediatric_symptomatic, care_days, df$daily_wage)
    hospital_medical_cost <- calculate_hospital_burden(df$hospitalisations, los, cost_per_day)

    return(tibble(
      mortality_loss = mortality_loss,
      hosp_prod_loss = hosp_prod_loss,
      hospital_medical_cost = hospital_medical_cost,
      symptomatic_loss = symptomatic_loss,
      long_covid_loss = long_covid_loss,
      informal_care_loss = informal_care_loss,
      total_loss = sum(mortality_loss, hosp_prod_loss, hospital_medical_cost, symptomatic_loss, long_covid_loss, informal_care_loss, na.rm = TRUE)
    ))
  })
}
