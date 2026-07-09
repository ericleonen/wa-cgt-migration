# ==============================================================================
# Function: synthesize_control.R
# Purpose: Create a synthetic control unit, a weighted sum of donor states that
#          most closely track Washington's low-income pre-trends
#
# Inputs:
# - state_migration_total: must be ./data/clean/state_migration_total.csv
# - treated: treated state, default WA
# - excluded: excluded states, default ./artifacts/excluded.txt
# - treatment_year: year marking treatment (before is pre-trends), default 2021
# - agi_classes: AGI classes to match on, default 1-4
# - age_group_ : age group to match on, default 0
#
# Outputs (list):
# - weights: weights (0-1 that sum to 1) of all states in the donor pool
# - control_migration: panel df for control state only
# ==============================================================================

library(tidyr)
library(dplyr)
library(quadprog)

synthesize_control <- function(state_migration_total,
                               treated = "WA",
                               excluded = readLines("./artifacts/excluded.txt"),
                               treatment_year = 2021,
                               agi_classes = 1:4,
                               age_group_ = 0) {
  R.df <- state_migration_total |>
    filter(!state %in% excluded,
           year < treatment_year, # pre-trends
           agi_class %in% agi_classes, # low-income households
           age_group == age_group_) |>
    group_by(state, year) |>
    summarise(pct_outflow_n1 = sum(outflow_n1) / sum(total_n1), 
              .groups = "drop")
  
  R.treated <- R.df$pct_outflow_n1[R.df$state == treated]
  R <- as.matrix(R.df |>
                   filter(state != treated) |>
                   pivot_wider(id_cols = "year",
                               names_from = "state",
                               values_from = "pct_outflow_n1") |>
                   select(-year))
  
  N <- ncol(R)
  D <- t(R) %*% R + diag(1e-8, N) # R' R + 1e-8 * I
  d <- t(R) %*% R.treated # R' R.treated
  A <- cbind(rep(1, N), diag(N))
  b <- c(1, rep(0, N))
  
  w <- solve.QP(D, d, A, b, meq = 1)$solution
  w <- pmax(w, 0)
  w <- w / sum(w)
  
  names(w) <- colnames(R)
  w.df <- tibble(state = colnames(R), w = w)
  
  control_migration <- state_migration_total |>
    filter(!state %in% excluded, state != treated) |>
    left_join(w.df, by = "state") |>
    group_by(year, agi_class, age_group) |>
    summarise(across(c(pct_outflow_n1, net_outflow_n1, net_outflow_agi),
                     ~ sum(.x * w)),
              .groups = "drop") |>
    mutate(state = "control", .before = "year")
  
  list(weights = w.df, control_migration = control_migration)
}