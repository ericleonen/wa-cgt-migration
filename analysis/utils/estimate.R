# ==============================================================================
# Function: estimate.R
# Purpose: Estimate difference-in-difference estimates alpha, gamma, lambda, and
#          beta
#
# Inputs:
# - state_migration_total: must be ./data/clean/state_migration_total.csv
# - control_migration: must be from synthesize_control
# - treated: treated state, default WA
# - treatment_year: year marking treatment, this is the "pre" year, default 2021
# - agi_class_: AGI class to estimate for, default 7
# - age_group_ : age group to estimate for, default 0
#
# Outputs:
# - coefs: df of coefficients, columns are "term", "outcome", and "estimate"
# ==============================================================================

library(dplyr)

estimate <- function(state_migration_total,
                     control_migration,
                     treated = "WA",
                     treatment_year = 2021,
                     agi_class_ = 7,
                     age_group_ = 0) {
  year.pre <- treatment_year
  year.post <- treatment_year + 1
  
  panel <- bind_rows(state_migration_total |> filter(state == treated),
                    control_migration) |>
    filter(year %in% year.pre:year.post,
           agi_class == agi_class_,
           age_group == age_group_) |>
    mutate(treated = state == treated, post = year == year.post)
  
  mod <- lm(cbind(pct_outflow_n1, net_outflow_n1, net_outflow_agi) ~ treated * post, 
            data = panel)
  coefs <- coef(mod)
  rownames(coefs) <- c("alpha", "gamma", "lambda", "beta")
  
  coefs <- as.data.frame(as.table(coefs))
  names(coefs) <- c("term", "outcome", "estimate")
  
  coefs
}