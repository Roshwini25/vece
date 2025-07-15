# Main analysis script: runs all scenarios using parameter grid
# R/run_scenarios.R

library(tidyverse)
library(purrr)

# --- Load modular functions ---
source("R/create_UK_data.R")            # Static population and risk inputs
source("R/calculate_disease_burden.R")  # Scenario-specific infections & outcomes
source("R/loss_strategy.R")              # Productivity and mortality losses
source("R/cost_strategy.R")              # Vaccine costs

# --- Step 1: Create base UK population data ---
uk_base <- create_UK_data()

# --- Step 2: Define scenario grid with best/central/worst cases ---
scenario_grid <- expand.grid(
  vaccine_uptake       = c(0.80, 0.90, 0.95),   # worst, central, best
  infection_rate       = c(0.0313, 0.0361, 0.0413), # lower, central, upper
  efficacy_symptomatic = c(0.25,  0.60,  0.75)   # worst, central, best
)

# --- Step 3: Define shared fixed parameters ---
params <- list(
  los = 8,
  cost_per_day = 933,
  post_hosp_days = 8,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.033,
  long_covid_days = 112,
  care_days = 3,
  cost_vaccine_total = 25
)

# --- Step 4: JCVI Spring 2025 Policy (75+ only, excluding care home and immunosuppressed) ---
scenario_results_jcvi <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, efficacy_symptomatic) {

    uk_pop <- uk_base %>%
      filter(age_group %in% c("75-79", "80+"))  # JCVI proxy: 75+ only

    df_burden <- calculate_disease_burden(
      df = uk_pop,
      infection_rate = infection_rate,
      vaccine_uptake = vaccine_uptake,
      efficacy_symptomatic = efficacy_symptomatic
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, vaccine_uptake, params$cost_vaccine_total)
    hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "JCVI Spring 2025 (75+ only)",
      vaccine_uptake,
      infection_rate,
      efficacy_symptomatic,
      hospital_cost = hospital_burden,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 5: Universal Policy (All Ages) ---
scenario_results_universal <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, efficacy_symptomatic) {

    df_burden <- calculate_disease_burden(
      df = uk_base,  # no filter, all age groups
      infection_rate = infection_rate,
      vaccine_uptake = vaccine_uptake,
      efficacy_symptomatic = efficacy_symptomatic
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, vaccine_uptake, params$cost_vaccine_total)
    hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "Universal (All Ages)",
      vaccine_uptake,
      infection_rate,
      efficacy_symptomatic,
      hospital_cost = hospital_burden,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 6: All Adults Policy (18+) ---
scenario_results_adults <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate, efficacy_symptomatic) {

    uk_pop <- uk_base %>%
      filter(age_group %in% c("18-29", "30-39", "40-49", "50-59", "60-64", "65-74", "75-79", "80+"))

    df_burden <- calculate_disease_burden(
      df = uk_pop,
      infection_rate = infection_rate,
      vaccine_uptake = vaccine_uptake,
      efficacy_symptomatic = efficacy_symptomatic
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, vaccine_uptake, params$cost_vaccine_total)
    hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      policy_name = "All Adults (18+)",
      vaccine_uptake,
      infection_rate,
      efficacy_symptomatic,
      hospital_cost = hospital_burden,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss
    )
  }
)

# --- Step 7: Combine All Policies ---
scenario_results_all <- bind_rows(
  scenario_results_jcvi,
  scenario_results_universal,
  scenario_results_adults
)

# --- Step 8: Save Combined Results ---
write_csv(scenario_results_all, "analysis/tables/scenario_results_policies.csv")
saveRDS(scenario_results_all, "analysis/data-derived/scenario_results_policies.rds")

# --- Optional: QA Preview ---
print(scenario_results_all)
