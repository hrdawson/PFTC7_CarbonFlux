# install.packages("devtools")
devtools::install_github("PaulESantos/co2fluxtent")

library(co2fluxtent)
library(tidyverse)

source("scripts/FUNCTIONS.R")

# Look for flux files in a folder
licor_files <- co2fluxtent::read_files("raw_data/test/")
licor_files <- Map(c, co2fluxtent::read_files("raw_data/Site 1/"),
                   co2fluxtent::read_files("raw_data/Site 2/"))

# Check if the files are ok
licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 50)

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

meta

licor_nee <- licor_files %>%
  flux_calc_own(param = "nee",
                skip = 3,
                vol = 1.2^3,
                area = 1.2^2) %>%
  mutate(filename = basename(filename))


licor_et <- licor_files %>%
  flux_calc_own(param = "et",
                skip = 3,
                vol = 1.2^3,
                area = 1.2^2) %>%
  mutate(filename = basename(filename))
