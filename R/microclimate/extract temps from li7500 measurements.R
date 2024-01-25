# Extract temperatures from the LI7500 data
# devtools::install_github("PaulESantos/co2fluxtent")
library(co2fluxtent)
library(tidyverse)
library(data.table)

# Read in all the ambient files ----
# Remember to fix the file names if you've just downloaded from OSF
source("R/functions/fix_file_names.R")
fix_file_names(path = "raw_data/LI7500/")

list.files("raw_data/LI7500/", recursive = TRUE)
# Look for flux files in a folder
licor_files <- Map(c, co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 1"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 2"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 3"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 4"),
                   co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 5"))


## clean file names
# Check if the files are ok
licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 50) ##removed three files

#Only the ambients
licor_files_ambient <- licor_files[["ambient_names"]]
filesAmbient <- licor_files_ambient |>
  # Fixed missing file names thanks to this post https://stackoverflow.com/questions/69357657/purrrmap-dfr-gives-number-of-list-element-as-id-argument-not-value-of-list-e
  purrr::set_names()

#Read them in
tempAmbient <- map_df(set_names(licor_files_ambient), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_delim(file = file, skip = 3, delim = "\t")) #important! reading in American format
}, .id = "File")

# Gather site, plot etc. information from the filenames
t.ambient = tempAmbient |>
  # extract metadata
  mutate(File = basename(File),
         File = str_remove(File, ".txt")
  ) |>
  # Separate into relevant info
  separate(File, into = c("flux", "siteID", "elevation", "aspect", "day.night"), remove = FALSE) |>
  # Get just the relevant data
  select(flux:day.night, Time:`Sequence Number`, `Temperature (C)`, `CO2 Signal Strength`) |>
  # Filter to just data with a decent signal strength
  filter(`CO2 Signal Strength` >= 90)

# Summarise by the ten minute interval so that we can compare to Tomst ----
library(lubridate)
library(timetk)

t.ten = t.ambient |>
  mutate(datetime = paste0(Date, " ", Time),
         datetime = str_sub(datetime, 1, 19),
         datetime = ymd_hms(datetime)) |>
  summarise_by_time(
    .date_var = datetime,
    .by = "minute",
    temp.C = mean(`Temperature (C)`),
    siteID = first(siteID),
    elevation = first(elevation),
    aspect = first(aspect),
    day.night = first(day.night)
  )

# What if we skip all that and just work with the compiled data file ----
# This will need to be redone when Michael finishes the function
# It doesn't have all the data, but on the other hand, it has the data we've decided are "good" from a carbon standard
# No, this doesn't work because it doesn't have the ambient.