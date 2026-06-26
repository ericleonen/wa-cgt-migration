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

age_breakdown

agi_breakdown <- wa_migration_total |>
  group_by(agi_class, year) |>
  summarise(total_n1 = sum(total_n1), .groups = "drop") |>
  pivot_wider(id_cols = agi_class, names_from = year, values_from = total_n1)
agi_breakdown$`2021 (%)` <- agi_breakdown$`2021` / sum(agi_breakdown$`2021`)
agi_breakdown$`2022 (%)` <- agi_breakdown$`2022` / sum(agi_breakdown$`2022`)

agi_breakdown
