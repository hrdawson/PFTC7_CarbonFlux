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

print(licor_files)

# Gather site, plot etc. information from the filenames

meta <- tibble(file_path = unlist(licor_files),
               file = basename(file_path)) %>%
  mutate(site = unlist(lapply(file, function(x) str_split(x, "_")[[1]][1])),
         elevation = unlist(lapply(file, function(x) str_split(x, "_")[[1]][2])),
         aspect = unlist(lapply(file, function(x) str_split(x, "_")[[1]][3])),
         plot = unlist(lapply(file, function(x) str_split(x, "_")[[1]][4])),
         day_night = unlist(lapply(file, function(x) str_split(x, "_")[[1]][5])),
         measurement = unlist(lapply(file, function(x) gsub(".txt","",tail(str_split(x, "_")[[1]],1)))),
         redo = grepl("redo", file, ignore.case = T))

# Make list of just the ambients to read in
filesAmbient = meta |>
  filter(measurement == "a") |>
  select(file_path)
