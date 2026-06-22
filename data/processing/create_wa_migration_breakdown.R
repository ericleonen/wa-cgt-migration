# ==============================================================================
# Script: create_wa_migration_breakdown.R
# Purpose: Ingest raw IRS SOI Tax Stats Migration data .zip files and produce
#          a dataset of inflow, outflow, and bilateral N1 migration with
#          Washington.
#
# Inputs:
# - data/raw/[Y1][Y2]migrationdata.zip: raw migration data from Y1 to Y2
#
# Outputs:
# - data/clean/wa_migration_breakdown.csv
# ==============================================================================

library(dplyr)
library(tidyr)
source("./data/processing/utils.R")

RELEVANT_YEARS <- 2021:2022 # pre- and post-years
OUTPUT_FILE = "./data/clean/wa_migration_breakdown.csv"

wa_migration_breakdown <- create_irs_dataset(function (path, year_code, year) {
  bind_rows(lapply(c("in", "out"), function(direction) {
    csv_name <- paste0("state", direction, "flow", year_code, ".csv")
    
    read.csv(unz(path, csv_name)) |>
      rename_with(~ "state", matches("^y[12]_state$")) |>
      filter(!state %in% c("WA", "FR")) |>
      select(state, n1) |>
      mutate(year = year, direction = direction, .after = "state")
  }))
}) |>
  filter(year %in% RELEVANT_YEARS) |>
  group_by(state, year, direction) |>
  summarise(n1 = sum(n1), .groups = "drop") |>
  pivot_wider(names_from = direction, values_from = n1,
              names_glue = "{ifelse(direction == 'in', 'to_WA_n1', 'from_WA_n1')}") |>
  mutate(bilateral_n1 = to_WA_n1 + from_WA_n1)

print(paste("Successfully wrote to", OUTPUT_FILE))
write.csv(wa_migration_breakdown,
          file = OUTPUT_FILE,
          row.names = FALSE)
