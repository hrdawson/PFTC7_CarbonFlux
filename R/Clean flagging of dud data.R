# Detect duplicates
licor_nee_redos = read_csv2("clean_data/segmented_fluxes_comments.csv") |>
  # Automate the comments on the flux file
  mutate(flag2 = case_when(
    extra_info == "Needs timing changed" ~ "manual_flux_time_selection",
    extra_info %in% c("Keep the redo", "increasing, keep the redo", "redo is better") ~ "discard_this_keep_redo",
    extra_info == "Remove" & comment == "Should not be negative" ~ "decreasing_NEE",
    extra_info == "Remove" & comment == "should not be positive" ~ "increasing_ER",
    extra_info == "Has not been picked up in manual go-through" ~ "suspicious",
    extra_info == "its increasing, changed it to 5-15, but should be removed" ~ "increasing_respiration",
    TRUE ~ "okay"
  )) |>
  filter(flag2 %in% c("okay")) |>
  # Add pivot ID
  mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", time, "_", measurement)) |>
  dplyr::group_by(pairID) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) |>
  # Join data back in
  left_join(licor_nee |> mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", time, "_", measurement))) |>
  select(pairID, aic_lm)

licor_nee_redos_keep = licor_nee_redos |>
  group_by(pairID) |>
  slice_min(order_by = aic_lm) |>
  mutate(flag = "okay") 

licor_nee_redos_discard = licor_nee_redos |>
  anti_join(licor_nee_redos_keep) |>
  mutate(flag = replace_na("high_aic_discard_this_keep_redo"))

licor_nee_redos_all = licor_nee_redos_keep |>
  bind_rows(licor_nee_redos_discard)

# Bring back to the rest of the data
licor_nee = read_csv2("clean_data/segmented_fluxes_comments.csv") |>
  mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", time, "_", measurement)) |>
  # Add in flags
  left_join(licor_nee_redos_all) |>
  # Automate the comments on the flux file
  mutate(flag2 = case_when(
    extra_info == "Needs timing changed" ~ "manual_flux_time_selection",
    extra_info %in% c("Keep the redo", "increasing, keep the redo", "redo is better") ~ "discard_this_keep_redo",
    extra_info == "Remove" & comment == "Should not be negative" ~ "decreasing_NEE",
    extra_info == "Remove" & comment == "should not be positive" ~ "increasing_ER",
    extra_info == "Has not been picked up in manual go-through" ~ "suspicious",
    extra_info == "its increasing, changed it to 5-15, but should be removed" ~ "increasing_respiration",
    TRUE ~ "okay"
  ),
  flag = coalesce(flag, flag2)
  ) |>
  #Remove extra column
  select(-c(flag2,'...1'))
