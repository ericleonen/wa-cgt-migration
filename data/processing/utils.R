# ==============================================================================
# File: utils.R
# Purpose: Provide helpers used to read IRS zip files
# ==============================================================================

library(tidyr)
library(dplyr)

# Creates a dataset using the IRS SOI data zips in "./data/raw". Equips a
# zip handler with the zip's path, the two-digit year code, and the full year
# (i.e. handler(path, year_code, year)).
create_irs_dataset <- function (handler) {
  irs_zips <- list.files("./data/raw", pattern = "\\.zip$", full.names = TRUE)
  
  bind_rows(lapply(irs_zips, function (path) {
    year_code <- regmatches(path,
                            regexpr("\\d{4}(?=migrationdata)",
                                    path, 
                                    perl = TRUE))
    
    year <- as.integer(paste0("20", substr(year_code, 3, 4)))
    
    handler(path, year_code, year)
  }))
}