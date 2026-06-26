# ==============================================================================
# Script: synthesize_control.R
# Purpose: Create a synthetic control unit, a weighted sum of non-spillover
#          states that most closely track Washington's low-income pre-trends
#
# Inputs:
# - data/clean/state_migration_total.csv
# - spillover_states
#
# Outputs:
# - ./data/clean/synthetic_control.csv
# - ./figures/
# ==============================================================================

library(tidyr)
library(dplyr)
library(quadprog)

LOW_INCOME_AGI_CLASSES <- 1:3

spillover_states <- c("CA", "FL", "NY", "TX")
state_migration_total <- read.csv("./data/clean/state_migration_total.csv")

all_P <- state_migration_total |>
  filter(year < 2021, 
         agi_class %in% LOW_INCOME_AGI_CLASSES,
         !state %in% spillover_states) |>
  group_by(state, year) |>
  summarise(pct_outflow_n1 = sum(outflow_n1) / sum(total_n1), .groups = "drop")


pWA <- all_P$pct_outflow_n1[all_P$state == "WA"]
P <- as.matrix(
  all_P |>
    filter(state != "WA") |>
    pivot_wider(id_cols = "year", 
                names_from = "state", 
                values_from = "pct_outflow_n1") |>
    select(-year)
)

N <- ncol(P)
D <- t(P) %*% P + diag(1e-8, N)
d <- t(P) %*% pWA
A <- cbind(rep(1, N), diag(N))
b <- c(1, rep(0, N))

result <- solve.QP(D, d, A, b, meq = 1)
weights <- result$solution
names(weights) <- colnames(P)
weights <- pmax(weights, 0)
weights <- weights / sum(weights)

