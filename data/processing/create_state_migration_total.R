# ==============================================================================
# Script: create_state_migration_total.R
# Purpose: Ingest raw IRS SOI Tax Stats Migration data .zip files and produce
#          a dataset of net outflow, total, (raw) outflow, and percent outflow
#          of N1, N2, and AGI.
#
# Inputs:
# - data/raw/[Y1][Y2]migrationdata.zip: raw migration data from Y1 to Y2
#
# Outputs:
# - data/clean/state_migration_total.csv
# ==============================================================================

library(dplyr)
library(tidyr)
source("./data/processing/utils.R")

COL_PATTERN <- paste0("^(in|out)flow_(n1|n2|y2_agi)_[0-6]$|",
                      "^(total|nonmig|samest)_(n1|n2|y1_agi)_[0-6]$")
OUTPUT_FILE = "./data/clean/state_migration_total.csv"

state_migration_total <- create_irs_dataset(function (path, year_code, year) {
  csv_name <- paste0(year_code, "inmigall.csv")
  
  read.csv(unz(path, csv_name)) |>
    select(state,
           agi_class = agi_stub,
           matches(COL_PATTERN)) |>
    pivot_longer(cols = matches(COL_PATTERN),
                 names_to = c(".value", "age_group"),
                 names_pattern = "^(.+)_([0-6])") |>
    mutate(outflow_n1 = outflow_n1,
           net_outflow_n1 = outflow_n1 - inflow_n1,
           total_n1 = nonmig_n1 + samest_n1 + outflow_n1, # data gives y2 total
           outflow_n2 = outflow_n2,
           pct_outflow_n1 = outflow_n2 / total_n1,
           net_outflow_n2 = outflow_n2 - inflow_n2,
           total_n2 = nonmig_n2 + samest_n2 + outflow_n2, # data gives y2 total
           pct_outflow_n2 = outflow_n2 / total_n2,
           outflow_agi = outflow_y2_agi,
           net_outflow_agi = outflow_y2_agi - inflow_y2_agi,
           total_agi = total_y1_agi,
           pct_outflow_agi = outflow_agi / total_agi,
           .keep = "unused") |>
    mutate(year = year, .after = "state")
})

print(paste("Successfully wrote to", OUTPUT_FILE))
write.csv(state_migration_total, 
          file = OUTPUT_FILE, 
          row.names = FALSE)
