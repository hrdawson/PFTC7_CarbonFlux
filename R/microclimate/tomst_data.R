library(tidyverse)
library(data.table)
library(googledrive)
library(googlesheets4)

source("R/functions/read_tomst_data.R")
source("R/functions/moist_calibration.R")

# Set time limits for the data read in
mind <- as.Date("2023-12-06", tz = "Etc/GMT-2")
maxd <- as.Date(Sys.Date(), tz = "Etc/GMT-2")
# THESE STARTING/FINISHING TIMES MAY NEED TRIMMING TO MATCH THE ACTUAL TIME ON FIELD

# Read tomst and site metadata
ids <- read_sheet("https://docs.google.com/spreadsheets/d/1wDnYs1BskBZ_F1GMkVqgg1RYGcbvc2k7bzRrfnpddrY/edit#gid=0") %>% 
  mutate(tomst_id = as.numeric(gsub("TMS","",tomst_id))) %>% 
  mutate(site = as.numeric(unlist(lapply(plot_id, function(x){substr(x, 1, 1)}))),
         aspect = unlist(lapply(plot_id, function(x){substr(x, 2, 2)})),
         plot = as.numeric(unlist(lapply(plot_id, function(x){substr(x, 3, 3)}))))

# List Tomst data files in a google drive folder
f <- drive_ls("https://drive.google.com/drive/folders/1Vnw1l6jl__G43F3GOTEG6BZjo2EBnfYK") %>% 
  filter(grepl("^data_.*.csv$", name))

# Read and combine Tomst data 
d <- read_tomst_data(f, tzone = "Etc/GMT-2") %>% 
  distinct(tomst_id, datetime, .keep_all = T) %>% # Remove dublicates
  arrange(tomst_id, datetime) %>%
  filter(datetime >= mind, #filter with setted time
         datetime <= maxd)

# Join Tomst data and metadata 
d <- left_join(ids, d) %>% 
  mutate(moist_vol = cal_funNA(moist)) # transform raw moisture counts to volumetric moisture content

# Plot temperature timeseries
d %>% 
  ggplot(aes(x=datetime, group = tomst_id)) +
  geom_line(aes(y = T3), col = "cornflowerblue") +
  geom_line(aes(y = T2), col = "brown1") +
  geom_line(aes(y = T1), col = "darkgoldenrod") +
  theme_minimal() +
  ylab("Temperature") + xlab("Date") +
  facet_grid(rows = vars(site), cols = vars(aspect))

# Plot moisture timeseries
d %>% 
  ggplot(aes(x=datetime, color = aspect, group = tomst_id)) +
  geom_line(aes(y = moist_vol)) +
  theme_minimal() +
  ylab("Volumetric moisture content") + xlab("Date") +
  facet_grid(rows = vars(site))

# means for each transect
d %>% 
  group_by(site, aspect) %>% 
  summarise(across(c(T1,T2,T3,moist_vol), mean))
