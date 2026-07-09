# ==============================================================================
# Function: coefs.get.R
# Purpose: Easily filter coefs by term and outcome
#
# Inputs:
# - coefs: output from estimate
# - term_: desired term
# - outcome_: desired outcome
#
# Outputs:
# - estimates
# ==============================================================================

library(dplyr)

coefs.get <- function(coefs, term_, outcome_) {
  coefs |>
    filter(term == term_, outcome == outcome_) |>
    pull(estimate)
}