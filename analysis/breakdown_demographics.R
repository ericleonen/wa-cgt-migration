# ==============================================================================
# Script: breakdown_demographics.R
# Purpose: Describe the distributions of age and wealth of taxpayers in our
#          date in 2021 and 2022
#
# Inputs:
# - data/clean/state_migration_total.csv
#
# Outputs:
# - ./artifacts/age_breakdown.csv: df of age distributions (counts and %'s)
# - ./artifacts/agi_breakdown.csv: df of AGI distributions (counts and %'s)
# ==============================================================================

library(tidyr)
library(dplyr)

wa_migration_total <- read.csv("./data/clean/state_migration_total.csv") |>
  filter(state == "WA", year == 2021:2022, age_group != 0, agi_class != 0)

age_breakdown <- wa_migration_total |>
  group_by(age_group, year) |>
  summarise(total_n1 = sum(total_n1), .groups = "drop") |>
  pivot_wider(id_cols = age_group, names_from = year, values_from = total_n1)
age_breakdown$`2021 (%)` <- age_breakdown$`2021` / sum(age_breakdown$`2021`)
age_breakdown$`2022 (%)` <- age_breakdown$`2022` / sum(age_breakdown$`2022`)

write.csv(age_breakdown, "./artifacts/age_breakdown.csv", row.names = FALSE)

agi_breakdown <- wa_migration_total |>
  group_by(agi_class, year) |>
  summarise(total_n1 = sum(total_n1), .groups = "drop") |>
  pivot_wider(id_cols = agi_class, names_from = year, values_from = total_n1)
agi_breakdown$`2021 (%)` <- agi_breakdown$`2021` / sum(agi_breakdown$`2021`)
agi_breakdown$`2022 (%)` <- agi_breakdown$`2022` / sum(agi_breakdown$`2022`)

write.csv(agi_breakdown, "./artifacts/agi_breakdown.csv", row.names = FALSE)