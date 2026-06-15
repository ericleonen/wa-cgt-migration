# ==============================================================================
# Script: verify_parallel_trends.R
# Purpose: Make plots and DiD computations providing evidence that trends for
#          placebo group (low-income) and pre-trends are small.
#
# Inputs:
# - data/clean/panel_migration_total.csv
# ==============================================================================

library(tidyr)
library(dplyr)
library(ggplot2)
source("./analysis/utils.R")

TREATMENT_YEAR <- 2021

panel_migration_total <- read.csv("./data/clean/panel_migration_total.csv")

plot_trends <- function(control,
                        agi_groups,
                        age_group = 0,
                        treatment_year = TREATMENT_YEAR) {
  agi_label <- if (length(agi_groups) > 1) {
    paste("AGI Groups:", paste(agi_groups, collapse = ", "))
  } else {
    paste("AGI Group:", agi_groups)
  }
  
  panel_migration_total |>
    filter(age_group == age_group, 
           agi_group %in% agi_groups,
           state %in% c("WA", paste0("CONTROL_", control))) |>
    group_by(state, year) |>
    summarise(across(c(net_n1, outflow_rate, net_y2_agi),
                     mean, na.rm = TRUE), .groups = "drop") |>
    pivot_longer(cols = c(net_n1, outflow_rate, net_y2_agi),
                 names_to = "var",
                 values_to = "value") |>
    mutate(var = factor(var,
                        levels = c("net_n1", "outflow_rate", "net_y2_agi"),
                        labels = c("Net N1", "Outflow Rate", "Net Y2 AGI"))) |>
    ggplot(aes(x = year, y = value, color = state)) +
    geom_line() +
    geom_vline(xintercept = treatment_year, linetype = "dashed", color = "gray50") +
    facet_wrap(~ var, scales = "free_y", ncol = 1) +
    # scale_color_manual(values = c("WA" = "steelblue", "DONOR" = "gray50")) +
    labs(title = paste0("WA vs Donor (", agi_label, ")"),
         x = NULL, y = NULL, color = NULL) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "top"
    )
}

# CHECK 1: low-income placebo
LOW_INCOME_GROUPS <- 1:3

print(plot_trends(control = 4, agi_groups = LOW_INCOME_GROUPS))
cat("\nPlacebo DiD Estimates:\n")
print(compute_did(panel_migration_total, 
                  control = 4,
                  agi_groups = LOW_INCOME_GROUPS,
                  treatment_years = TREATMENT_YEAR))

# CHECK 2: target pre-trends
print(plot_trends(control = 4, agi_groups = 7))
cat("\nPre-treatment DiD Estimates by Year:\n")
print(compute_did(panel_migration_total, 
                  control = 4,
                  agi_groups = LOW_INCOME_GROUPS,
                  treatment_years = min(panel_migration_total$year):TREATMENT_YEAR - 1))
