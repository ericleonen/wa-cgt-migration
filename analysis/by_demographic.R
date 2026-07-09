# ==============================================================================
# Script: by_demographic.R
# Purpose: Further analysis, computations for all demographics (age x AGI)
#
# Inputs:
# - state_migration_total: data/clean/state_migration_total.csv
#
# Outputs:
# - ./artifacts/demographic_betas.csv
# ==============================================================================

source("./analysis/utils/synthesize_control.R")
source("./analysis/utils/estimate.R")
source("./analysis/utils/compute_significance.R")
source("./analysis/utils/coefs.get.R")

state_migration_total <- read.csv("./data/clean/state_migration_total.csv")

betas.demo <- bind_rows(lapply(1:6, function (age_group) {
  bind_rows(lapply(1:7, function (agi_class) {
    print(paste("age_group:", age_group, "+", "agi_class:", agi_class))
    
    C.demo <- synthesize_control(state_migration_total, age_group_ = age_group)
    coefs.demo <- estimate(state_migration_total, 
                           C.demo$control_migration, 
                           agi_class_ = agi_class, 
                           age_group_ = age_group)
    sig.demo <- compute_significance(state_migration_total,
                                     coefs.demo,
                                     agi_class.target = agi_class,
                                     age_group.match = age_group,
                                     age_group.target = age_group)
    inner_join(coefs.demo, sig.demo$pvals, by = c("term", "outcome")) |>
      filter(term == "beta") |>
      mutate(age_group = age_group, 
             agi_class = agi_class, 
             .before = "outcome") |>
      select(-term)
  }))
}))

write.csv(betas.demo, "./artifacts/demographic_betas.csv", row.names = FALSE)
