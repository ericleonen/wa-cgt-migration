# ==============================================================================
# Function: compute_significance.R
# Purpose: Compute p-values for Washington coefficient estimates
#
# Inputs:
# - state_migration_total: must be ./data/clean/state_migration_total.csv
# - coefs.wa: coefs from estimate
# - excluded: excluded states, default ./artifacts/excluded.txt
# - treatment_year: year marking treatment, default 2021
# - agi_classes.match: AGI classes to match on, default 1-4
# - agi_class.target: AGI class to estimate for, default 1-4
# - age_group.match : age group to match on, default 0
# - age_group.target : age group to estimate for, default 0
#
# Outputs (list):
# - coefs.donors: coefs from applying estimate to each donor
# - pvals: raw (histogram) and smooth (KDE) p-values for each term/outcome pair
# ==============================================================================

library(dplyr)
source("analysis/utils/synthesize_control.R")
source("analysis/utils/estimate.R")
source("analysis/utils/coefs.get.R")

compute_significance <- function(state_migration_total,
                                 coefs.wa,
                                 excluded = readLines("./artifacts/excluded.txt"),
                                 treatment_year = 2021,
                                 agi_classes.match = 1:4,
                                 agi_class.target = 7,
                                 age_group.match = 0,
                                 age_group.target = 0) {
  
  donors <- setdiff(unique(state_migration_total$state), excluded)
  donors <- donors[donors != "WA"]
  
  coefs.donors <- bind_rows(lapply(donors, function (donor) {
    control_migration <- synthesize_control(state_migration_total,
                                            donor,
                                            c(excluded, "WA"),
                                            treatment_year,
                                            agi_classes.match,
                                            age_group.match)$control_migration
    
    estimate(state_migration_total,
             control_migration,
             donor,
             treatment_year,
             agi_class.target,
             age_group.target) |>
      mutate(donor = donor, .before = "term")
  }))
  
  pvals <- coefs.donors |>
    group_by(term, outcome) |>
    summarise(est = coefs.get(coefs.wa, term[1], outcome[1]),
              est_finite = is.finite(est),
              n_finite = sum(is.finite(estimate)),
              h = tryCatch({
                vals <- estimate[is.finite(estimate)]
                if (length(unique(vals)) < 2) {
                  NA_real_
                } else {
                  bw.SJ(vals)
                }
              },
              error = function(e) NA_real_),
              p.raw = if (isTRUE(est_finite)) mean(estimate > est, na.rm = TRUE) else NA_real_,
              p.smooth = if (isTRUE(est_finite) && !is.na(h)) {
                mean(pnorm((est - estimate[is.finite(estimate)]) / h, lower.tail = FALSE))
              } else {
                NA_real_
              },
              .groups = "drop") |>
    select(-est, -est_finite, -n_finite, -h)
  
  list(coefs.donors = coefs.donors, pvals = pvals)
}