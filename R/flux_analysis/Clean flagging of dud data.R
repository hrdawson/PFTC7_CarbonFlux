# CO2 Fluxes ----
# Detect duplicates
licor_nee_duplicates = read_csv2("clean_data/segmented_fluxes_comments.csv") |>
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
  # Filter to just 'okay' fluxes
  filter(flag2 %in% c("okay")) |>
  # Add pivot ID
  mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", time, "_", measurement)) |>
  # Use pivot warning code to detect duplicates
  dplyr::group_by(pairID) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) |>
  # Join data back in
  left_join(licor_nee |> mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", time, "_", measurement))) |>
  select(pairID, aic_lm)

# Make an object with the duplicates to keep flagged as okay
licor_nee_duplicates_keep = licor_nee_duplicates |>
  group_by(pairID) |>
  # Choose lowest AIC value
  slice_min(order_by = aic_lm) |>
  # Flag
  mutate(flag = "okay") 

# Make an object with the duplicates to discard
licor_nee_duplicates_discard = licor_nee_duplicates |>
  # Filter out the ones kept in the object above
  anti_join(licor_nee_duplicates_keep) |>
  # Flag
  mutate(flag = replace_na("high_aic_discard_this_keep_redo"))

# Combine the duplicates
licor_nee_duplicates_all = licor_nee_duplicates_keep |>
  bind_rows(licor_nee_duplicates_discard)

# Bring back to the rest of the data
licor_nee = read_csv2("clean_data/segmented_fluxes_comments.csv") |>
  mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", time, "_", measurement)) |>
  # Add in flags
  left_join(licor_nee_duplicates_all) |>
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
  # Combine duplicate flags and automated comment flags
  flag = coalesce(flag, flag2)
  ) |>
  #Remove extra column
  select(-c(flag2,'...1'))

# H2O fluxes ----
library(readxl)

licor_et_comments = read_excel("raw_data/licor_et_cleaning_comments.xlsx") |>
  select(filename, comments) |>
  mutate(filename = paste(filename, "txt", sep = "."))

licor_et = read_csv("raw_data/licor_et_raw.csv") |>
  select(-c('...1')) |>
  # Add in comments
  left_join(licor_et_comments) |>
  # Write machine usable comments
  mutate(flag = case_when(
    is.na(comments) ~ "okay",
    comments == "Looks good" ~ "okay",
    comments == "Redo2 is better" ~ "discard_this_keep_redo",
    comments == "Redo is better" & flagged == TRUE ~ "discard_this_keep_redo",
    comments == "Redo is better" & flagged == FALSE ~ "okay",
    comments == "There is a better redo" ~ "discard_this_keep_redo",
    comments == "This is better but needed a cut" ~ "manual_flux_time_selection",
    comments == "This is better" ~ "manual_flux_time_selection",
    comments == "Needed to be cut" ~ "manual_flux_time_selection",
    comments == "The original is better" ~ "discard_this_keep_redo",
    comments == "signalstrength low for the whole measurement" ~ "suspicious",
    comments == "Signalstrength low for the whole measurement" ~ "suspicious",
    comments == "low signalstrength in the beginning" ~ "manual_flux_time_selection",
    comments == "Saturated and goes down around 70 sec" ~ "manual_flux_time_selection",
    comments == "Signalstrength below 95 at 50 sec" ~ "suspicious",
    comments == "Saturated after 55 sec" ~ "manual_flux_time_selection"
  ))
