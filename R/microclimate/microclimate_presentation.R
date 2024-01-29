# Ridgeline plot
library(tidyverse)
library(ggridges)
library(tidylog)
library(lubridate)
library(rstatix)
# For color gradient
library(viridis)

# For standardised colors
library(MetBrewer)

scale_color_met_d(name = "Cassatt2")

# Read in Tomst data ----
tomst = read.csv("clean_data/PFTC7_Tomst_Data.csv") |>
  # tidy
  select(-moist) |>
  pivot_longer(cols = T1:moist_vol, names_to = "metric", values_to = "value") |>
  rename(siteID = site) |>
  # alter so that all temps have the same name
  mutate(metric = factor(case_when(
    ## NOTE THAT THESE ARE INCORRECT ##
    metric == "T1" ~ "Aboveground T (ºC)",
    metric == "T2" ~ "Surface T (ºC)",
    metric == "T3" ~ "Soil T (ºC)",
    metric == "moist_vol" ~ "Soil moisture (%)"
  )),
  elevation = case_when(
    siteID == 1 ~ 2000,
    siteID == 2 ~ 2200,
    siteID == 3 ~ 2400,
    siteID == 4 ~ 2600,
    siteID == 5 ~ 2800
  )) |>
  # work with date time for day/night
  mutate(datetime = ymd_hms(datetime),
         hour = hour(datetime),
         day.night = case_when(
           hour %in% 5:18 ~ "Day",
           hour %in% 0:4 ~ "Night",
           hour >= 19 ~ "Night"
         )) |>
  mutate(dataset = "Tomst")

tomst.means = tomst |>
  group_by(metric, siteID, day.night) |>
  get_summary_stats(value, type = "mean_sd")


tomst.means.moist = tomst |>
  group_by(metric, siteID, aspect) |>
  get_summary_stats(value, type = "mean") |>
  filter(metric == "Soil moisture") |>
  select(-n) |>
  pivot_wider(names_from = aspect, values_from = mean) |>
  mutate(diff = east - west)

# Make trial plot with basic Tomst data ----
ggplot(tomst |> filter(metric != "Soil moisture (%)"),
       aes(x = value, y = as.factor(elevation), fill = aspect)) +
  geom_density_ridges(alpha = 0.7, color = "grey50") +
  theme_ridges() +
  scale_color_met_d(name = "Cassatt2", direction = - 1) +
  scale_fill_met_d(name = "Cassatt2", direction = - 1) +
  labs(y = "Site", x = "") +
  facet_grid(metric ~ day.night, scales = "free_x")

ggsave("visualizations/2023.12.17_tempTomst.png", width = 10, height = 8, units = "in")

ggplot(tomst |> filter(metric == "Soil moisture (%)"),
       aes(x = value, y = as.factor(elevation), fill = aspect)) +
  geom_density_ridges(alpha = 0.7, color = "grey50") +
  theme_ridges() +
  scale_color_met_d(name = "Cassatt2", direction = - 1) +
  scale_fill_met_d(name = "Cassatt2", direction = - 1) +
  labs(y = "Site", x = "")
  # facet_grid(day.night ~ ., scales = "free_x")

ggsave("visualizations/2023.12.17_moistTomst.png", width = 6, height = 8, units = "in")

# make trial plot with basic FLIR ----
FLIRflat = read.csv("clean_data/flir_values.csv") |>
  mutate(aspect = case_when(
    siteID == 6 ~ "none",
    TRUE ~ aspect
  ),
  aspect = factor(aspect, levels = c("east", "west", "none"))) |>
  mutate(flag = case_when(
    temp_C < 0 ~ "too cold",
    day.night == "night" & temp_C > 18 ~ "too warm",
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay") |>
  drop_na(siteID) |>
  mutate(dataset = "FLIR",
         elevation = case_when(
           siteID == 1 ~ 2000,
           siteID == 2 ~ 2200,
           siteID == 3 ~ 2400,
           siteID == 4 ~ 2600,
           siteID == 5 ~ 2800,
           siteID == 6 ~ 3000
         ))

FLIR.means = FLIRflat |>
  group_by(siteID, day.night) |>
  get_summary_stats(temp_C, type = "mean_sd")

# For standardised colors
library(MetBrewer)
scale_color_met_d(name = "Cassatt2")

ggplot(FLIRflat, aes(x = temp_C, y = as.factor(elevation), fill = aspect, color = aspect)) +
  geom_density_ridges(alpha = 0.7, color = "grey50") +
  theme_ridges() +
  scale_color_met_d(name = "Cassatt2", direction = - 1) +
  scale_fill_met_d(name = "Cassatt2", direction = - 1) +
  facet_grid(~day.night, scales = "free_x")

ggsave("visualizations/2023.12.17_tempFLIR.png", width = 10, height = 8, units = "in")

# Import IR temp gun data ----
IRtemp = read.csv("raw_data/LI7500/PFTC7_SA_raw_fluxes_2023.csv") |>
  drop_na(siteID) |>
  # flag erroneous reads
  mutate(flag = case_when(
    str_detect(Remarks, "IRt on soil") ~ "discard",
    TRUE ~ "okay"
  )) |>
  filter(flag == "okay") |>
  # take most recent attempt
  arrange(Attempt) |>
  group_by(day.night, aspect, siteID, plotID) |>
  slice_tail(n = 1) |>
  ungroup() |>
  select(day.night:plotID, flag, IR_temp_1:IR_temp_5) |>
  pivot_longer(cols = IR_temp_1:IR_temp_5, names_to = "delete", values_to = "temp_C") |>
  drop_na(siteID) |>
  mutate(dataset = "IR Temp",
         elevation = case_when(
           siteID == 1 ~ 2000,
           siteID == 2 ~ 2200,
           siteID == 3 ~ 2400,
           siteID == 4 ~ 2600,
           siteID == 5 ~ 2800
         ))

IRtemp.means = IRtemp |>
  group_by(siteID, day.night) |>
  get_summary_stats(temp_C, type = "mean_sd")

# Plot IR temp gun data ----
ggplot(IRtemp,
       aes(x = temp_C, y = as.factor(elevation), fill = aspect, color = aspect)) +
  geom_density_ridges(alpha = 0.7, color = "grey50") +
  theme_ridges() +
  scale_color_met_d(name = "Cassatt2", direction = - 1) +
  scale_fill_met_d(name = "Cassatt2", direction = - 1) +
  facet_grid(~day.night, scales = "free_x")

ggsave("visualizations/2023.12.17_IRtemps.png", width = 10, height = 8, units = "in")

# Compare temps across methods ----

temps = tomst |>
  filter(metric == "Aboveground Temperature (C)") |>
  rename(temp_C = value) |>
  bind_rows(FLIRflat) |>
  bind_rows(IRtemp) |>
  select(day.night, siteID, aspect, plotID, dataset, temp_C) |>
  mutate(day.night = str_to_title(day.night)) |>
  mutate(aspect = factor(aspect, levels = c("east", "west", "none")))

# plot
ggplot(temps,
       aes(x = temp_C, y = as.factor(siteID), fill = dataset, color = dataset)) +
  geom_density_ridges(alpha = 0.7, color = "grey50") +
  theme_ridges() +
  facet_grid(~day.night, scales = "free_x")
ggsave("visualizations/2023.12.17_tempMethods.png", width = 10, height = 8, units = "in")

tomst |>
  pivot_wider(names_from = metric, values_from = value) |>
  select(siteID, aspect, elevation, day.night, 'Aboveground T (ºC)',
         'Soil moisture (%)', 'Soil T (ºC)', 'Surface T (ºC)') |>
  ggpairs()
