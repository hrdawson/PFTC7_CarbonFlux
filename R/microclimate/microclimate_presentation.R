# Ridgeline plot
library(tidyverse)
library(ggridges)
library(tidylog)
library(lubridate)
library(rstatix)
# For color gradient
library(hrbrthemes)
library(viridis)

# Read in Tomst data
tomst = read.csv("clean_data/PFTC7_Tomst_Data.csv") |>
  # tidy
  select(-moist) |>
  pivot_longer(cols = T1:moist_vol, names_to = "metric", values_to = "value") |>
  # alter so that all temps have the same name
  mutate(metric = case_when(
    metric == "T1" ~ "Aboveground Temperature (C)",
    metric == "T2" ~ "Surface Temperature (C)",
    metric == "T3" ~ "Soil Temperature (C)",
    metric == "moist_vol" ~ "Soil moisture"
  ),
  metric = factor(metric, levels = c("Aboveground Temperature (C)", "Surface Temperature (C)",
                                         "Soil Temperature (C)", "Soil moisture"))) |>
  # work with date time for day/night
  mutate(datetime = ymd_hms(datetime),
         hour = hour(datetime),
         day.night = case_when(
           hour %in% 5:18 ~ "Day",
           hour %in% 0:4 ~ "Night",
           hour >= 19 ~ "Night"
         ))

tomst.means = tomst |>
  group_by(metric, site, aspect, day.night) |>
  get_summary_stats(value, type = "mean")

# Make trial plot with basic Tomst data
ggplot(tomst, aes(x = value, y = as.factor(site), fill = aspect)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges() +
  labs(y = "Site", x = "") +
  facet_grid(day.night ~ metric, scales = "free_x") +
  theme(legend.position = "none")

# make trial plot with basic FLIR
ggplot(FLIRflat |> filter(temp_C > 0) |> drop_na(siteID),
       aes(x = temp_C, y = as.factor(siteID), fill = aspect)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges() +
  facet_grid(~day.night, scales = "free_x") +
  theme(legend.position = "none")

# Import IR temp gun data ----
IRtemp = read.csv("raw_data/PFTC7_SA_raw_fluxes_2023.csv") |>
  # flag erroneous reads
  mutate(flag = case_when(
    str_detect(Remarks, "IRt on soil") ~ "discard",
    TRUE ~ "okay"
  )) |>
  select(day.night:plotID, flag, IR_temp_1:IR_temp_5) |>
  pivot_longer(cols = IR_temp_1:IR_temp_5, names_to = "delete", values_to = "temp_C") |>
  drop_na(siteID) |>
  filter(flag == "okay")

# Plot IR temp gun data
ggplot(IRtemp,
       aes(x = temp_C, y = as.factor(siteID), fill = aspect)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges() +
  facet_grid(~day.night, scales = "free_x") +
  theme(legend.position = "none")
