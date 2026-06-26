# ==============================================================================
# Script: determine_spillover_states.R
# Purpose: Determine states with high levels of in- and out-migration and create
#          plots to visualize them
#
# Inputs:
# - data/clean/wa_migration_breakdown.csv
#
# Outputs:
# - ./figures/wa-migration-dists.png: plot of distributions of in- and
#                                     out-migration with upper Tukey fences
# - spillover_states: list of states with high levels of migration
# ==============================================================================

library(tidyr)
library(dplyr)

wa_migration_breakdown <- read.csv("./data/clean/wa_migration_breakdown.csv")

# computing Tukey fences
tukey_fences <- wa_migration_breakdown |>
  pivot_longer(cols = c(to_WA_n1, from_WA_n1), 
               names_to = "migration_var") |>
  group_by(migration_var) |>
  summarise(upper = quantile(value, 0.75) + 1.5 * IQR(value), .groups = "drop")

spillover_states <- wa_migration_breakdown |>
  pivot_longer(cols = c(to_WA_n1, from_WA_n1), 
               names_to = "migration_var") |>
  left_join(tukey_fences, by = "migration_var") |>
  filter(value > upper) |>
  pull(state) |>
  unique()

# plotting 2021 Washington migration distributions
png("./figures/wa-migration-dists.png", width = 900, height = 500, res = 120)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0))

plot_migration_distribution <- function(data_n1, main, upper, show_ylab = FALSE) {
  data_k  <- data_n1 / 1000
  upper_k <- upper / 1000
  
  hist(data_k,
       main = main,
       cex.main = 0.9,
       font.main = 1,
       xlab = "primary taxpayers (thousands)",
       ylab = ifelse(show_ylab, "number of states", ""),
       col = "steelblue",
       border = "white")
  abline(v = upper_k, lty = 2, lwd = 2, col = "red")
}

par(mar = c(4, 4, 2, 1.5))
plot_migration_distribution(wa_migration_breakdown$to_WA_n1,
                            main = "In-Migration",
                            upper = tukey_fences$upper[tukey_fences$migration_var == "to_WA_n1"],
                            show_ylab = TRUE)

par(mar = c(4, 1.5, 2, 2.5))
plot_migration_distribution(wa_migration_breakdown$from_WA_n1,
                            main = "Out-Migration",
                            upper = tukey_fences$upper[tukey_fences$migration_var == "from_WA_n1"])

legend("topright", legend = "upper Tukey fence", lty = 2, col = "red", cex = 0.8)

mtext("Histogram of Washington Migration, 2021", outer = TRUE, cex = 1.3, font = 2)

dev.off()
