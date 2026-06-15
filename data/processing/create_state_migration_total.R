# ==============================================================================
# Script: create_state_migration_total.R
# Purpose: Ingest raw IRS SOI Tax Stats Migration data .zip files and produce
#          a dataset of net inflow of returns (N1), N1 outflow rate, and net
#          inflow of adjusted gross income (AGI) per state/year/AGI group/age
#          group.
#
# Inputs:
# - data/raw/[Y1][Y2]migrationdata.zip: raw migration data from Y1 to Y2
#
# Outputs:
# - data/clean/state_migration_total.csv
# ==============================================================================

library(dplyr)
library(tidyr)

IRS_ZIPS <- list.files("./data/raw", pattern = "\\.zip$", full.names = TRUE)
COL_PATTERN = "^(in|out)flow_(n1|y2_agi)_[0-6]$|^total_n1_[0-6]$"

OUTPUT_FILE = "./data/clean/state_migration_total.csv"

state_migration_data <- bind_rows(lapply(IRS_ZIPS, function(path) {
  year_code <- regmatches(path,
                          regexpr("\\d{4}(?=migrationdata)", path, perl = TRUE))
  
  # year refers to the end year of each interval
  year <- as.integer(paste0("20", substr(year_code, 3, 4)))
  
  csv_name <- paste0(year_code, "inmigall.csv")
  
  read.csv(unz(path, csv_name)) |>
    select(state,
           agi_group = agi_stub, # agi_stub renamed to the clearer agi_group
           matches(COL_PATTERN)) |>
    pivot_longer(cols = matches(COL_PATTERN),
                 names_to = c(".value", "age_group"),
                 names_pattern = "^(.+)_([0-6])") |>
    mutate(net_n1 = inflow_n1 - outflow_n1,
           outflow_rate = outflow_n1 / total_n1,
           net_y2_agi = inflow_y2_agi - outflow_y2_agi,
           .keep = "unused") |>
    mutate(year = year, .after = "state")
}))

print(paste("Successfully wrote to", OUTPUT_FILE))
write.csv(state_migration_data, 
          file = OUTPUT_FILE, 
          row.names = FALSE)
