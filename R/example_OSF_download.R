# Example code for how to download data from OSF
# You'll need the dataDownloader package
# install.packages("remotes")
# remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

# Example of how to download the data
# The node will stay the same for our whole project,
# but the file and remote_path will change for each file.
# Check OSF to know where to find your data

# Download Site 1 tent flux data
get_file(
         # Which repository is it in?
         node = "hk2cy",
         # Which file do you want?
         file = "LI7500_Site 1.zip",
         # Where do you want the file to go to?
         path = "raw_data/LI7500",
         # Where is the file stored within the OSF repository?
         remote_path = "raw_data/raw_flux_data/LI7500")

# Groups of files on OSF are stored as compressed (zipped) folders to reduce chaos
# After downloading the zip file, use this code to unzip it
# If you downloaded a .txt, .csv, or similar file type, you can skip this


