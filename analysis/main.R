# ==============================================================================
# Script: main.R
# Purpose: Main analysis; target computations, visuals, and pre-trends and
#          placebo verifications
#
# Inputs:
# - state_migration_total: data/clean/state_migration_total.csv
#
# Outputs:
# - ./artifacts/control_weights.csv
# - ./figures/target-beta-dists.png
# - ./artifacts/target_wa_coefs.csv
# - ./artifacts/pre_wa_coefs.csv
# - ./artifacts/placebo_wa_coefs.csv
# ==============================================================================

library(latex2exp)

source("./analysis/utils/synthesize_control.R")
source("./analysis/utils/estimate.R")
source("./analysis/utils/compute_significance.R")
source("./analysis/utils/coefs.get.R")

state_migration_total <- read.csv("./data/clean/state_migration_total.csv")

C <- synthesize_control(state_migration_total)

write.csv(C$weights, "./artifacts/control_weights.csv", row.names = FALSE)

wa <- state_migration_total |>
  filter(state == "WA", agi_class == 7, age_group == 0)

control <- C$control_migration |>
  filter(agi_class == 7, age_group == 0)

png("./figures/parallel-trends.png", width = 1600, height = 500, res = 120)
par(mfrow = c(1, 3))

plot_parallel_trends <- function (years, 
                                  wa.outcomes,
                                  control.outcomes, 
                                  ylab, 
                                  ylim) {
  plot(years, 
       wa.outcomes, 
       type = "b",
       pch = 16,
       ylim = ylim,
       xlab = "Year",
       ylab = ylab,
       cex.lab = 1.4)
  lines(years,
        control.outcomes,
        type = "b", 
        lty = 2, 
        pch = 1) 
}

plot_parallel_trends(wa$year, 
                     wa$pct_outflow_n1 * 100,
                     control$pct_outflow_n1 * 100, 
                     "N1 outflow rate (%)", 
                     c(4, 10))

plot_parallel_trends(wa$year, 
                     wa$net_outflow_n1 / 1000,
                     control$net_outflow_n1 / 1000, 
                     "Net N1 outflow (thousands of households)",
                     c(-2.5, 2.3))

plot_parallel_trends(wa$year, 
                     wa$net_outflow_agi / 1000,
                     control$net_outflow_agi / 1000, 
                     "Net AGI outflow (thousands of USD)",
                     c(-1000, 2000))

legend("topleft",
       legend = c("Washington", "Synthetic control"),
       lty = c(1, 2),
       cex = 1.4)

dev.off()

# ---TARGET ESTIMATES---
coefs.wa <- estimate(state_migration_total, C$control_migration)
sig <- compute_significance(state_migration_total, coefs.wa)

OUTCOMES <- c("pct_outflow_n1", "net_outflow_n1", "net_outflow_agi")
OUTCOME_LABELS <- c(TeX(r"(\beta (% outflow in N1))"),
                    TeX(r"(\beta (net outflow in N1))"),
                    TeX(r"(\beta (net outflow in AGI))"))

png("./figures/target-beta-dists.png", width = 1800, height = 500, res = 120)
par(mfrow = c(1, 3))

for (i in 1:3) {
  hist(coefs.get(sig$coefs.donors, "beta", OUTCOMES[i]),
       main = "",
       xlab = OUTCOME_LABELS[i],
       ylab = ifelse(i == 1, "frequency", ""),
       cex.lab = 1.4,
       col = "gray90",
       border = "gray50")
  abline(v = coefs.get(coefs.wa, "beta", OUTCOMES[i]),
         lty = 2,
         lwd = 2,
         col = "dodgerblue")
  box()
  
  if (i == 1) {
    legend("topleft",
           legend = "Washington",
           cex = 1.2,
           lty = 2,
           lwd = 2,
           col = "dodgerblue") 
  }
}

dev.off()

coefs.wa.target <- inner_join(coefs.wa, sig$pvals, by = c("term", "outcome"))
write.csv(coefs.wa.target, "./artifacts/target_wa_coefs.csv", row.names = FALSE)

# ---PARALLEL PRE-TRENDS VERIFICATION---
year.min <- min(state_migration_total$year) + 1

coefs.wa.pre <- bind_rows(lapply(year.min:2020, function (year) {
  coefs.wa.year <- estimate(state_migration_total, 
                           C$control_migration,
                           treatment_year = year)
  sig.year <- compute_significance(state_migration_total,
                                   coefs.wa.year,
                                   treatment_year = year)
  
  inner_join(coefs.wa.year, sig.year$pvals, by = c("term", "outcome")) |>
    filter(term == "beta") |>
    mutate(year = year, .before = "outcome")
}))

write.csv(coefs.wa.pre, "./artifacts/pre_wa_coefs.csv", row.names = FALSE)

# ---PLACEBO TEST VERIFICATION---
coefs.wa.placebo <- bind_rows(lapply(1:6, function(agi_class) {
  coefs.wa.agi_class <- estimate(state_migration_total,
                                 C$control_migration,
                                 agi_class_ = agi_class)
  sig.agi_class <- compute_significance(state_migration_total,
                                        coefs.wa.agi_class,
                                        agi_class.target = agi_class)
  
  inner_join(coefs.wa.agi_class, sig.agi_class$pvals, by = c("term", "outcome")) |>
    filter(term == "beta") |>
    mutate(agi_class = agi_class, .before = "outcome")
}))

write.csv(coefs.wa.placebo, "./artifacts/placebo_wa_coefs.csv", row.names = FALSE)
