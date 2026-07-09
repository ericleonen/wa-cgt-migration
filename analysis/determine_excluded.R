# ==============================================================================
# Script: determine_excluded.R
# Purpose: Determine states with to exclude from the donor pool; find states
#          with high levels of bilateral migration, visualize them
#
# Inputs:
# - data/clean/wa_migration_breakdown.csv
#
# Outputs:
# - ./figures/wa-bi-migration-hist.png: histogram of bilateral migration with
#                                       and upper Tukey fence
# - ./artifacts/spillovers.csv: df of states with high levels of migration with
#                               migration levels
# - ./artifacts/excluded.txt: set of states excluded from the donor pool 
# ==============================================================================

library(tidyr)
library(dplyr)

wa_migration_breakdown <- read.csv("./data/clean/wa_migration_breakdown.csv")

# computing Tukey fences
tukey_fence <- quantile(wa_migration_breakdown$bilateral_n1, 0.75) +
  1.5 * IQR(wa_migration_breakdown$bilateral_n1)

# New York and New Jersey had major tax policy changes affecting high-income
# residents in 2021
excluded.set <- c("NY", "NJ")

spillovers.set <- wa_migration_breakdown |>
  filter(bilateral_n1 > tukey_fence) |>
  pull(state) |>
  unique()

spillovers <- wa_migration_breakdown[wa_migration_breakdown$state %in% spillovers.set,]

write.csv(spillovers, "./artifacts/spillovers.csv", row.names = FALSE)

excluded <- union(excluded.set, spillovers.set)

write(excluded, "./artifacts/excluded.txt")

# plotting 2021 Washington migration distributions
png("./figures/wa-bi-migration-hist.png", width = 900, height = 500, res = 120)

hist(wa_migration_breakdown$bilateral_n1 / 1000,
     main = "",
     cex.main = 0.1,
     xlab = "N1 (thousands of households)",
     ylab = "Number of states",
     cex.lab = 1.2,
     breaks = 12)
box()
abline(v = tukey_fence / 1000,
       col = "azure4",
       lty = 2,
       lwd = 2)

legend("topright",
       legend = "upper Tukey fence (k=1.5)",
       col = "azure4",
       lty = 2, 
       lwd = 2)

dev.off()
