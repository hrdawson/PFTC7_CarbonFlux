library(tidyverse)
library(tidylog)
library(co2fluxtent)

## Read in files ----
# Make file list
filesPFTC7 <- dir(path = "raw_data",
                  pattern = ".txt", full.names = TRUE, recursive = TRUE)

licor_files <- Map(c, co2fluxtent::read_files("raw_data/Site 1/"),
                   co2fluxtent::read_files("raw_data/Site 2/"))

# Test files are good
licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 50) |>
  unlist()

# Read in data
tempPFTC7 <- map_df(set_names(licor_files), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_delim(file, skip = 3, delim = "\t")) #important! reading in American format
}, .id = "File")

str(tempPFTC7)

# Calculate data
fluxes = tempPFTC7 |>
  # Drop unnecessary parts of the file name
  mutate(File = basename(File),
         File = str_remove(File, ".txt"),
         # Set flux type
         flux = case_when(
           str_detect(File, "photo") ~ "NEE",
           str_detect(File, "resp") ~ "ER",
           str_detect(File, "a") ~ "Ambient"
         )) |>
  # Separate into relevant info
  separate(File, into = c("siteID", "elevation", "aspect", "plotID", "day.night"), remove = FALSE) |>
  # Rename site and plot so they behave
  mutate(siteID = paste0("Site ", siteID),
         plotID = paste0("Plot ", plotID),
         # Add column for redos
         redo = case_when(
           str_detect(File, "redo") ~ "second",
           str_detect(File, "redo2") ~ "third",
           TRUE ~ "first")
  )

# Calculate new variables
# Calculate means
fluxes.means = fluxes |>
  group_by(File) |>
  summarize(tav = mean(`Temperature (C)`),
            pav <- mean(`Pressure (kPa)`),
            cav <- mean(`CO2 (umol/mol)`),
            wav <- mean(`H2O (mmol/mol)`))

fluxes.ambient = fluxes |>
  filter(flux == "Ambient") |>
  group_by(File, siteID, elevation, aspect, plotID, day.night) |>
  summarize(amb.CO2 = mean(`CO2 (umol/mol)`),
            amb.H2O = mean(`H2O (mmol/mol)`)) |>
  ungroup() |>
  select(-File)

fluxes.calc = fluxes |>
  # Join ambient
  left_join(fluxes.ambient) |>
  rename(co2 = `CO2 (umol/mol)`, h2o = `H2O (mmol/mol)`, temp = `Temperature (C)`,
         pressure = `Pressure (kPa)`, co2.signal = `CO2 Signal Strength`, dew = `Dew Point (C)`) |>
  group_by(File) |>
  mutate(time = row_number(),
         cprime = co2/(1 - (h2o/1000)),
         wprime = h2o/(1 - (h2o/1000)),
         wav_dil = mean(h2o/(1 - (h2o/1000))),
         camb = amb.CO2/(1 - amb.H2O/1000),
         wamb = amb.H2O/(1 - amb.H2O/1000)
  )

# Format data for faceting
fluxes.pivot = fluxes.calc |>
  # Remove ambient
  filter(flux != "Ambient") |>
  # Pivot the useful columns
  select(siteID:day.night, redo, flux, time, temp, pressure, dew, co2.signal, cprime, wprime) |>
  pivot_longer(cols = temp:wprime, names_to = "variable", values_to = "value") |>
  # Create uniqueID
  mutate(uniqueID = paste0(siteID, " ", aspect, " ", plotID, " ", day.night)) |>
  ungroup() |>
  select(uniqueID, redo, flux, time, variable, value)

# Write the ggplot by aspect for now
library(ggh4x)

# Code from https://community.rstudio.com/t/using-a-for-loop-to-make-ggplot2-plots-and-save-them/84715/4

fluxes.pivot %>%
  group_nest(uniqueID) %>%
  mutate(plot = map(data,
                    ~ggplot(.x, aes(x = time, y = value, color = redo)) +
                      geom_point() +
                      scale_color_manual(values = c("black", "dodgerblue4", "chocolate4")) +
                      # scale_x_continuous(breaks = seq(0, 120, by = 10)) +
                      facet_grid2(variable ~ flux, scales = "free", independent = "y") +
                      theme_bw() +
                      theme(legend.position = "none")
  )) %>%
  walk2(.x = .$uniqueID,
        .y = .$plot,
        .f = ~ ggsave(filename = paste0("visualizations/", .x, ".pdf"),
                      plot = .y, width = 10, height = 8, units = "in")
  )
