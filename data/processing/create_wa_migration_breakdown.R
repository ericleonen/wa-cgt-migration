# ==============================================================================
# Script: create_wa_migration_breakdown.R
# Purpose: Ingest raw IRS SOI Tax Stats Migration data .zip files and produce
#          a dataset of migrations to and from (inflow + outflow) Washington
#          state: migrated returns (N1), migrated individuals (N2), and migrated
#          adjusted gross income (AGI).
#
# Inputs:
# - data/raw/[Y1][Y2]migrationdata.zip: raw migration data from Y1 to Y2
#
# Outputs:
# - data/clean/wa_migration_breakdown.csv
# ==============================================================================

library(dplyr)
library(tidyr)

IRS_ZIPS <- list.files("./data/raw", pattern = "\\.zip$", full.names = TRUE)
RELEVANT_YEARS <- 2021:2022 # pre- and post-years
OUTPUT_FILE = "./data/clean/wa_migration_breakdown.csv"

wa_migration_breakdown <- bind_rows(lapply(IRS_ZIPS, function (path) {
  year_code <- regmatches(path,
                          regexpr("\\d{4}(?=migrationdata)", path, perl = TRUE))
  
  # year refers to the end year of each interval
  year <- as.integer(paste0("20", substr(year_code, 3, 4)))
  
  bind_rows(lapply(c("in", "out"), function(direction) {
    csv_name <- paste0("state", direction, "flow", year_code, ".csv")
    
    read.csv(unz(path, csv_name)) |>
      rename_with(~ "state", matches("^y[12]_state$")) |>
      filter(!state %in% c("WA", "FR"), # origin is not WA or foreign
             year %in% RELEVANT_YEARS,
             y1_statefips == 53 | y2_statefips == 53) |> # destination is WA
      select(state, n1, n2, agi = AGI) |>
      mutate(year = year, direction = direction, .after = "state")
  }))
})) |>
  group_by(state, year) |>
  summarise(n1 = sum(n1), n2 = sum(n2), agi = sum(agi), .groups = "drop")

print(paste("Successfully wrote to", OUTPUT_FILE))
write.csv(wa_migration_breakdown,
          file = OUTPUT_FILE,
          row.names = FALSE)
