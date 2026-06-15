# ==============================================================================
# Script: compute_estimates.R
# Purpose: Compute target DiD estimates
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

print("Group 1")
print(compute_did(panel_migration_total, 
                  control = 1,
                  agi_groups = 7,
                  treatment_years = TREATMENT_YEAR))

print("Group 2")
print(compute_did(panel_migration_total, 
                  control = 2,
                  agi_groups = 7,
                  treatment_years = TREATMENT_YEAR))

print("Group 3")
print(compute_did(panel_migration_total, 
                  control = 3,
                  agi_groups = 7,
                  treatment_years = TREATMENT_YEAR))

print("Group 4")
print(compute_did(panel_migration_total, 
                  control = 4,
                  agi_groups = 7,
                  treatment_years = TREATMENT_YEAR))

print("Group 4") # CO only
print(compute_did(panel_migration_total, 
                  control = 5,
                  agi_groups = 7,
                  treatment_years = TREATMENT_YEAR))