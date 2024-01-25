# Download data using this script
# Use RStudio's handy script menu to jump to the data you need
# You should only need to download data once, unless you know they have been updated
# If you've been away from the project for awhile, you should download them again
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# Fluxes ----
## Clean LI7500 data ----
# WARNING: These data aren't actually clean
# They're just the best that we have while Michael works on the new way of cleaning fluxes
get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_Cflux_originalCalc.csv",
  # Where do you want the file to go to?
  path = "clean_data",
  # Where is the file stored within the OSF repository?
  remote_path = "flux_data")

get_file(
  # Which repository is it in?
  node = "hk2cy",
  # Which file do you want?
  file = "LI7500_H2Oflux_originalCalc.csv",
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