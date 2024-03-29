# Download data using this script
# Use RStudio's handy script menu to jump to the data you need
# You should only need to download data once, unless you know they have been updated
# If you've been away from the project for awhile, you should download them again
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# Fluxes ----
## Clean LI7500 data ----
### All carbon fluxes ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "licor_nee_flagged.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

### Carbon fluxes made pretty ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "licor7500_carbon_fluxes.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

## Water fluxes ----
### All water fluxes ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "licor_et_flagged.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

### Water fluxes made pretty ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "licor7500_ET_fluxes.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

## Clean LI8100 data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI8100_combined.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

### Soil fluxes made pretty ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "licor8100_soil_fluxes.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

# Microclimate ----
## Clean tomst data ----
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "PFTC7_Tomst_Data.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "climate_data")

## Clean FLIR data ----
# Note that this file is 383MB and may cause R to crash. You can also download it from OSF at https://osf.io/gp8u9
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "flir_values.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "climate_data")
