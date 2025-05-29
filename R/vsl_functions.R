#from Squire
vsl_fn <- function(iso3c = "GBR", vsl_path = "analysis/data-derived/vsly.rds") {
  uk_pop_df <- squire::get_population(iso3c = "GBR")   # Get UK population by age group
  squire_params <- squire::parameters_explicit_SEEIR(country = "United Kingdom")  # Get parameters like IFR and hospitalisation rates
  uk_pop_df$ifr <- squire_params$ifr  #  Add IFR and hospitalisation rate
  uk_pop_df$prob_hosp <- squire_params$prob_hosp  #  Add IFR and hospitalisation rate

  vsly_df <- readRDS(vsl_path)

  # Filter for UK only (iso3c == "GBR")
  vsl_uk_value <- vsly_df[vsly_df$iso3c == iso3c, "vsl"]
  le_uk_value <- vsly_df[vsly_df$iso3c == iso3c, "lg"]
  age_groups <- vsly_df[vsly_df$iso3c == iso3c, "age_group"]

  df <- data.frame("age_group" = age_groups,
                   "le" = le_uk_value,
                   "vsl" = vsl_uk_value,
                   "iso3c" = iso3c)

  return(df)

}
