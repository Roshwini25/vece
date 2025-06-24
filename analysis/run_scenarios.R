# Main analysis script: runs all scenarios using parameter grid
# R/run_scenarios.R

library(tidyverse)
library(purrr)

# --- Load modular functions ---
source("R/create_UK_data.R")            # Static population and risk inputs
source("R/calculate_disease_burden.R")  # Scenario-specific infections & outcomes
source("R/loss_strategy.R")              # Productivity and mortality losses
source("R/cost_strategy.R")              # Vaccine and hospitalisation costs

# --- Step 1: Create base UK population data ---
uk_base <- create_UK_data()

# --- Step 2: Define scenario grid ---
scenario_grid <- expand.grid(
  vaccine_uptake = c(0.5, 0.8, 1.0),
  infection_rate = c(0.1, 0.2, 0.3)
)

# --- Step 3: Set fixed vaccine efficacy ---
efficacy_fixed <- 0.6

# --- Step 4: Shared parameters for all scenarios ---
params <- list(
  los = 7,
  cost_per_day = 933,
  hosp_days_lost = 7,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.133,
  long_covid_days = 28,
  daily_wage = 128,
  care_days = 3,
  daily_value_care = 128,
  cost_vaccine = 10,
  cost_delivery = 5
)

# --- Step 5: Run scenario loop and compute outcomes ---
scenario_results <- pmap_dfr(
  scenario_grid,
  function(vaccine_uptake, infection_rate) {

    df_burden <- calculate_disease_burden(
      df = uk_base,
      infection_rate = infection_rate,
      vaccine_uptake = vaccine_uptake,
      efficacy_symptomatic = efficacy_fixed
    )

    loss <- calculate_losses(df_burden, params)
    vax_cost <- calculate_vaccine_cost(df_burden$n, vaccine_uptake, params$cost_vaccine, params$cost_delivery)
    hospital_burden <- calculate_hospital_burden(df_burden$hospitalisations, params$los, params$cost_per_day)

    tibble(
      vaccine_uptake = vaccine_uptake,
      infection_rate = infection_rate,
      efficacy_symptomatic = efficacy_fixed,
      hospital_cost = hospital_burden,
      vaccine_cost = vax_cost,
      symptomatic_loss = loss$symptomatic_loss,
      long_covid_loss = loss$long_covid_loss,
      mortality_loss = loss$mortality_loss,
      informal_care_loss = loss$informal_care_loss,
      hosp_prod_loss = loss$hosp_prod_loss,
      total_productivity_loss = loss$total_loss,#removed net_loss = hospital_burden + vax_cost + loss$total_loss

    )
  }
)

# --- Step 6: Save scenario outputs ---
write_csv(scenario_results, "analysis/tables/scenario_results.csv")
saveRDS(scenario_results, "analysis/data-derived/scenario_results.rds")

# --- Optional: Print output for QA ---
print(scenario_results)

# --- Manual Check (Optional for debugging) ---
df_manual <- calculate_disease_burden(uk_base, 0.1, 0.5, efficacy_fixed)
loss_check <- calculate_losses(df_manual, params)
vax_check <- calculate_vaccine_cost(df_manual$n, 0.5, params$cost_vaccine, params$cost_delivery)
hosp_check <- calculate_hospital_burden(df_manual$hospitalisations, params$los, params$cost_per_day)

loop_result <- scenario_results %>%
  filter(vaccine_uptake == 0.5, infection_rate == 0.1)

comparison <- tibble(
  metric = c("Total Productivity Loss", "Vaccine Cost", "Hospital Cost"),
  manual_value = c(loss_check$total_loss, vax_check, hosp_check),
  loop_value = c(loop_result$total_productivity_loss, loop_result$vaccine_cost, loop_result$hospital_cost)
)

print(comparison)
