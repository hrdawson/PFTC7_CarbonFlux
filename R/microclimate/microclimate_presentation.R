# Ridgeline plot
library(tidyverse)
library(ggridges)
library(tidylog)
# For color gradient
library(hrbrthemes)
library(viridis)

# Read in Tomst data
tomst = read.csv("clean_data/PFTC7_Tomst_Data.csv") |>
  # tidy
  pivot_longer(cols = T1:moist, names_to = "variable", values_to = "value") |>
  # alter so that all temps have the same name
  mutate(variable = case_when(
    variable %in% c("T1", "T2", "T3") ~ "Temperature (C)",
    variable == "moist" ~ "Soil moisture"
  ))

# Make trial plot with basic Tomst data
ggplot(tomst, aes(x = value, y = as.factor(site), fill = aspect)) +
  geom_density_ridges(alpha = 0.5) +
  theme_ridges() +
  facet_grid(~variable, scales = "free_x") +
  theme(legend.position = "none")
