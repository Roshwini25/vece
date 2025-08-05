# 08_run_vsly_cost_sweep.R
# Runs scenarios spanning both VSLY scaling and vaccine cost scaling
# Generates Figure 5: Heatmap of NMB vs VSLY Scaling and Vaccine Cost Scaling

library(tidyverse)
library(purrr)
devtools::load_all()

# --- Load input data ---
ifr_tbl <- read.csv("analysis/data-derived/ifhr_table.csv")
employment_tbl <- read.csv("analysis/data-derived/uk_employment_rate_2024.csv")

# --- Define scaling factors ---
vsly_factors <- seq(0.1, 2, by = 0.1)       # from 0.1x to 2x VSLY
vax_cost_factors <- seq(0.1, 2, by = 0.1)   # from 0.1x to 2x vaccine cost

# --- Define scenario grid ---
scenario_grid_vsly_cost <- expand.grid(
  vaccine_uptake = c(0.80, 0.90, 0.95),
  infection_rate = c(0.0361),        # central infection rate
  vaccine_efficacy = c("central"),   # central efficacy
  vsly_factor = vsly_factors,
  cost_factor = vax_cost_factors
)

# --- Shared parameters ---
params <- list(
  los = 8,
  cost_per_day = 933,
  post_hosp_days = 8,
  symptomatic_days_lost = 1.5,
  long_covid_prob = 0.033,
  long_covid_short_days = 12 * 7,
  long_covid_medium_days = 18 * 7,
  long_covid_long_days = 24 * 7,
  care_days = 3,
  cost_vaccine_total = 25,    # baseline cost
  ifr_ihrs = ifr_tbl
)

# --- Policy definitions ---
policy_list <- list(
  list(name = "JCVI Spring 2025 (75+ only)",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = replace(uptake, !(age_group %in% c("75-80","80+")), 0))),
  list(name = "Universal (All Ages)",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = uptake)),
  list(name = "All Adults (18+)",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = replace(uptake, age_group %in% c("0-5","5-10","10-15","15-20"), 0))),
  list(name = "No Vaccination",
       filter = function(df, uptake) df %>% mutate(vaccine_uptake = 0))
)

# --- Run scenarios ---
results_vsly_cost <- pmap_dfr(
  scenario_grid_vsly_cost,
  function(vaccine_uptake, infection_rate, vaccine_efficacy, vsly_factor, cost_factor) {

    ve_hosp <- params$ifr_ihrs$ve_h
    ve_death <- params$ifr_ihrs$ve_d

    map_dfr(policy_list, function(pol) {

      uk_base <- create_country_data(iso3c = "GBR",
                                     ifr = params$ifr_ihrs$ifr_naive,
                                     ihr = params$ifr_ihrs$ihr_naive) %>%
        left_join(employment_tbl, by = "age_group") %>%
        mutate(employment_rate = employment_rate_percent / 100)

      uk_base$ve_hosp <- ve_hosp
      uk_base$ve_death <- ve_death

      uk_pop <- pol$filter(uk_base, vaccine_uptake) %>% ungroup()

      df_burden <- calculate_disease_burden(
        df = uk_pop,
        infection_rate = infection_rate,
        vaccine_uptake = uk_pop$vaccine_uptake,
        ve_hosp = uk_pop$ve_hosp,
        ve_death = uk_pop$ve_death
      ) %>%
        mutate(vsly = vsly * vsly_factor)

      loss <- calculate_losses(df_burden, params)
      vax_cost <- calculate_vaccine_cost(df_burden$n, df_burden$vaccine_uptake,
                                         params$cost_vaccine_total * cost_factor)

      nmb <- loss$total_loss - vax_cost

      tibble(
        policy_name = pol$name,
        vaccine_uptake,
        infection_rate,
        vaccine_efficacy,
        vsly_factor,
        cost_factor,
        nmb = nmb
      )
    })
  }
)

# --- Save results ---
saveRDS(results_vsly_cost, "analysis/data-derived/scenario_results_vsly_cost.rds")
write_csv(results_vsly_cost, "analysis/tables/scenario_results_vsly_cost.csv")
cat("✅ VSLY + Vaccine Cost sweep complete. Results saved to analysis/data-derived/scenario_results_vsly_cost.rds\n")

# --- Step 4: Calculate relative NMB (baseline = No Vaccination) ---
baseline_vsly_cost <- results_vsly_cost %>%
  filter(policy_name == "No Vaccination") %>%
  mutate(vaccine_uptake = as.numeric(vaccine_uptake),
         cost_factor = as.numeric(cost_factor),
         vsly_factor = as.numeric(vsly_factor)) %>%
  select(vaccine_uptake, vsly_factor, cost_factor, baseline_nmb = nmb)

df_vsly_cost <- results_vsly_cost %>%
  mutate(vaccine_uptake = as.numeric(vaccine_uptake),
         cost_factor = as.numeric(cost_factor),
         vsly_factor = as.numeric(vsly_factor)) %>%
  left_join(baseline_vsly_cost, by = c("vaccine_uptake","vsly_factor","cost_factor")) %>%
  mutate(relative_nmb = nmb - baseline_nmb)

# Check if we have NA values
print(sum(is.na(df_vsly_cost$relative_nmb)))

# --- Step 5: Plot Figure 5 (Heatmap of NMB) ---
fig5 <- df_vsly_cost %>%
  filter(policy_name != "No Vaccination") %>%
  ggplot(aes(x = vsly_factor, y = cost_factor, fill = relative_nmb)) +
  geom_tile() +
  scale_fill_viridis_c(option = "C", name = "Relative NMB (£ billions)") +  # removed log
  facet_grid(policy_name ~ vaccine_uptake, labeller = label_both) +
  labs(
    title = "Figure 5: Heatmap of Net Monetary Benefit (NMB) vs VSLY and Vaccine Cost Scaling",
    subtitle = "NMB shown relative to No Vaccination (baseline = 0)",
    x = "\nVSLY Scaling Factor",
    y = "Vaccine Cost Scaling Factor\n"
  ) +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(size = 11))

# --- Save Figure ---
ggsave("analysis/plots/figure5_vsly_cost_heatmap.png", fig5, width = 12, height = 8, dpi = 300)
ggsave("analysis/plots/figure5_vsly_cost_heatmap.pdf", fig5, width = 12, height = 8)

cat("✅ Figure 5 (VSLY + Vaccine Cost sensitivity) saved to analysis/plots/figure5_vsly_cost_heatmap.[png/pdf]\n")
