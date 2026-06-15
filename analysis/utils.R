compute_did <- function(panel_migration_total, 
                        control,
                        agi_groups,
                        age_group = 0, 
                        treatment_years) {
  ageg <- age_group   # avoid shadowing the column name in filter()
  
  lapply(as.list(treatment_years), function(yr) {
    panel_migration_total |>
      filter(agi_group %in% agi_groups, 
             age_group == ageg,
             year %in% c(yr, yr + 1),
             state %in% c("WA", paste0("CONTROL_", control))) |>
      group_by(state, year) |>
      summarise(across(c(net_n1, outflow_rate, net_y2_agi),
                       mean, na.rm = TRUE), .groups = "drop") |>
      arrange(state, year) |>
      select(-state, -year) |>
      (\(df) (df[4, ] - df[3, ]) - (df[2, ] - df[1, ]))() |>
      mutate(treatment_year = yr)
  }) |>
    bind_rows()
}