# ==============================================================================
# Script: select_donors.R
# Purpose: Select candidate groups of donor states to form a control state.
#          First, filter out outlier states with high inter-migration with
#          Washington. Then, create small, medium, and large groups of donor
#          states matching on migration pre-trends for placebo (low-income)
#          groups.
#
# Inputs:
# - data/clean/wa_migration_breakdown.csv
# - data/clean/state_migration_total.csv
#
# Outputs:
# - data/clean/donors.csv
# - data/clean/panel_migration_total.csv
# ==============================================================================

library(tidyr)
library(dplyr)
library(ggplot2)

DONORS_FILE <- "./data/clean/donors.csv"
PANEL_MIGRATION_TOTAL_FILE <- "./data/clean/panel_migration_total.csv"

state_migration_total <- read.csv("./data/clean/state_migration_total.csv")
wa_migration_breakdown <- read.csv("./data/clean/wa_migration_breakdown.csv")

donors <- wa_migration_breakdown |>
  distinct(state) |>
  mutate(group = 0)

# STEP 1: filter high inter-migration states

# compute outlier thresholds and states
outlier_thresholds <- wa_migration_breakdown |>
  pivot_longer(cols = c(n1, n2, agi), names_to = "var") |>
  mutate(var = toupper(var),
         year = factor(year)) |>
  group_by(var, year) |>
  summarise(upper = quantile(value, 0.75) + 1.5 * IQR(value), .groups = "drop")

outlier_states <- wa_migration_breakdown |>
  pivot_longer(cols = c(n1, n2, agi), names_to = "var") |>
  mutate(var = toupper(var), year = factor(year)) |>
  left_join(outlier_thresholds, by = c("var", "year")) |>
  filter(value > upper) |>
  pull(state) |>
  unique()

# plot: distributions of total migration in 2021 and 2022
wa_migration_breakdown |>
  pivot_longer(cols = c(n1, n2, agi), names_to = "var") |>
  mutate(var = toupper(var),
         year = factor(year)) |>
  left_join(outlier_thresholds, by = c("var", "year")) |>
  ggplot(aes(x = value, fill = year)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(aes(xintercept = upper),
             linetype = "dashed", color = "red", linewidth = 0.5) +
  facet_grid(year ~ var, scales = "free") +
  labs(title = "WA Total (Inflow + Outflow) Migration, 2021-2022") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"),
        axis.title = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
        panel.spacing = unit(0.5, "lines"))

# STEP 2: sort on pre-treatment placebo migration trends

LOW_AGI_GROUPS <- 1:3

pre_treatment <- state_migration_total |>
  filter(year < 2021, age_group == 0, agi_group %in% LOW_AGI_GROUPS) |>
  group_by(state, year) |>
  summarise(
    outflow_rate = mean(outflow_rate, na.rm = TRUE),
    net_n1 = mean(net_n1, na.rm = TRUE),
    net_y2_agi = mean(net_y2_agi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(c(outflow_rate, net_n1, net_y2_agi), scale))

wa_pre <- pre_treatment |> filter(state == "WA")

donor_sims <- pre_treatment |>
  filter(!state %in% outlier_states, state != "WA") |>
  left_join(wa_pre, by = "year", suffix = c("", "_wa")) |>
  group_by(state) |>
  summarise(
    sim = as.numeric(
      cor(outflow_rate, outflow_rate_wa) +
        cor(net_n1, net_n1_wa) +
        cor(net_y2_agi, net_y2_agi_wa)
    ),
    .groups = "drop"
  ) |>
  arrange(-sim)

# STEP 3: create panel data for each donor group

min_sims <- c(-3, 0, 1, 2, max(donor_sims$sim))

panel_migration_total <- bind_rows(
  state_migration_total |>
    filter(state == "WA"),
  Map(function (min_sim, group) {
    states_group <- donor_sims |> filter(sim >= min_sim) |> pull(state)
    
    donors[donors$state %in% states_group, "group"] <<- group 
    
    state_migration_total |>
      filter(state %in% states_group) |>
      group_by(year, agi_group, age_group) |>
      summarise(net_n1 = mean(net_n1),
                outflow_rate = mean(outflow_rate),
                net_y2_agi = mean(net_y2_agi),
                .groups = "drop") |>
      mutate(state = paste0("CONTROL_", group))
  }, min_sims, seq_along(min_sims))
)

panel_migration_total |> filter(agi_group == 0, age_group == 0)

print(paste("Successfully wrote to", 
            DONORS_FILE, 
            "and", 
            PANEL_MIGRATION_TOTAL_FILE))
write.csv(donors,
          file = DONORS_FILE,
          row.names = FALSE)
write.csv(panel_migration_total,
          file = PANEL_MIGRATION_TOTAL_FILE,
          row.names = FALSE)