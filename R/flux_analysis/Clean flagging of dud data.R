# CO2 Fluxes ----
# Detect duplicates
licor_nee_start = read_csv2("clean_data/segmented_fluxes_comments.csv") |>
  # Set flux type
  mutate(flux = case_when(
    str_detect(filename, "photo") ~ "NEE",
    str_detect(filename, "resp") ~ "ER",
    str_detect(filename, "a") ~ "Ambient"
  )) |>
  # Add pivot ID
  rename(day.night = time) |>
  mutate(pairID = paste0(site, "_", aspect, "_", plot, "_", day.night, "_", flux)) |>
  # Automate the comments on the flux file
  mutate(flag2 = case_when(
    extra_info == "Needs timing changed" ~ "manual_flux_time_selection",
    extra_info %in% c("Keep the redo", "increasing, keep the redo", "redo is better") ~ "discard_this_keep_other_reading",
    extra_info == "Remove" & comment == "Should not be negative" ~ "decreasing_NEE",
    extra_info == "Remove" & comment == "should not be positive" ~ "increasing_ER",
    extra_info == "Has not been picked up in manual go-through" ~ "suspicious",
    extra_info == "its increasing, changed it to 5-15, but should be removed" ~ "increasing_respiration",
    TRUE ~ "okay"
  )) 

licor_nee_duplicates = licor_nee_start |>
  # Filter to just 'okay' fluxes
  filter(flag2 %in% c("okay")) |>
  # Use pivot warning code to detect duplicates
  dplyr::group_by(pairID) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) |>
  # Join data back in
  left_join(licor_nee_start) |>
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
  mutate(flag = replace_na("high_aic_discard_this_keep_other_reading"))

# Combine the duplicates
licor_nee_duplicates_all = licor_nee_duplicates_keep |>
  bind_rows(licor_nee_duplicates_discard)

# Bring back to the rest of the data
licor_nee = licor_nee_start |>
  # Add in flags
  left_join(licor_nee_duplicates_all) |>
  # Combine duplicate flags and automated comment flags
  mutate(flag = coalesce(flag, flag2)) |>
  #Remove extra column
  select(-c(flag2,'...1'))

# write.csv(licor_nee, "clean_data/licor_nee_flagged.csv")

# H2O fluxes ----
library(readxl)

## Read in comments ----
licor_et_comments = read_excel("raw_data/licor_et_cleaning_comments.xlsx") |>
  select(filename, comments) 

## Construct initial ET object ----
licor_et_start = read_csv("raw_data/licor_et_raw.csv") |>
  select(-c('...1')) |>
  # Drop unnecessary parts of the file name
  mutate(filename = basename(filename),
         filename = str_remove(filename, ".txt")
  ) |>
  # Separate into relevant info
  separate(filename, into = c("site", "elevation", "aspect", "plot", "day.night", "measurement"), remove = FALSE) |>
  # Set flux type
  mutate(flux = case_when(
    str_detect(filename, "photo") ~ "Tlight",
    str_detect(filename, "resp") ~ "Tdark",
    str_detect(filename, "a") ~ "Tamb")
  ) |>
  # Add in comments
  left_join(licor_et_comments) |>
  # Write machine usable comments
  mutate(flag2 = case_when(
    is.na(comments) ~ "okay",
    comments == "Looks good" ~ "okay",
    comments == "Redo2 is better" ~ "discard_this_keep_other_reading",
    comments == "Redo is better" & flagged == TRUE ~ "discard_this_keep_other_reading",
    comments == "Redo is better" & flagged == FALSE ~ "okay",
    comments == "There is a better redo" ~ "discard_this_keep_other_reading",
    comments == "This is better but needed a cut" ~ "manual_flux_time_selection",
    comments == "This is better" ~ "manual_flux_time_selection",
    comments == "Needed to be cut" ~ "manual_flux_time_selection",
    comments == "The original is better" ~ "discard_this_keep_other_reading",
    comments == "signalstrength low for the whole measurement" ~ "suspicious",
    comments == "Signalstrength low for the whole measurement" ~ "suspicious",
    comments == "low signalstrength in the beginning" ~ "manual_flux_time_selection",
    comments == "Saturated and goes down around 70 sec" ~ "manual_flux_time_selection",
    comments == "Signalstrength below 95 at 50 sec" ~ "suspicious",
    comments == "Saturated after 55 sec" ~ "manual_flux_time_selection"
  )) |>
  rename(suggested.discard = flagged) |>
  # Add pivot ID
  mutate(pairID = paste(site, aspect, plot, day.night, flux, sep = "_")) 

## Detect duplicates
licor_et_duplicates = licor_et_start |>
  dplyr::filter(flag2 %in% c("okay", "manual_flux_time_selection")) |>
  rename(file = filename) |>
  # Pivot
  dplyr::group_by(pairID) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) |>
  # Join data back in
  left_join(licor_et_start) |>
  select(pairID, aic_lm)

# Make an object with the duplicates to keep flagged as okay
licor_et_duplicates_keep = licor_et_duplicates |>
  group_by(pairID) |>
  # Choose lowest AIC value
  slice_min(order_by = aic_lm) |>
  # Flag
  mutate(flag = "okay") 

# Make an object with the duplicates to discard
licor_et_duplicates_discard = licor_et_duplicates |>
  # Filter out the ones kept in the object above
  anti_join(licor_nee_duplicates_keep) |>
  # Flag
  mutate(flag = replace_na("high_aic_discard_this_keep_other_reading"))

# Combine the duplicates
licor_et_duplicates_all = licor_et_duplicates_keep |>
  bind_rows(licor_et_duplicates_discard)

## Big ET object ----
licor_et = licor_et_start |>
  left_join(licor_et_duplicates_all) |>
  # Combine duplicate flags and automated comment flags
  mutate(flag = coalesce(flag, flag2)) |>
  #Remove extra column
  select(-c(flag2))

# write.csv(licor_et, "clean_data/licor_et_flagged.csv")
