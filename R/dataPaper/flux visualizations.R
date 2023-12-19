# Visualize fluxes
# Note that these are messy data that haven't been cleaned yet

# Read in flux datasets
LI7500 = read.csv("outputs/2023.12.19_Cflux_originalCalc.csv") |>
  mutate(dataset = "Ecosystem fluxes")

LI8100 = read.csv("clean_data/LI8100_combined.csv") |>
  mutate(dataset = "Soil fluxes")

LI7500H2O = read.csv("outputs/2023.12.19_H2Oflux_originalCalc.csv") |>
  mutate(dataset = "Water fluxes")
