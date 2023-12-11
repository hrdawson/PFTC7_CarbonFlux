
## Code by Jonas Trepel, Hilary Rose Dawson & Kristine Birkeli

#required packages
library(RespChamberProc)
library(tidyverse)
#library(tidylog)

#assumptions: 
## naming: SR_site_elevation_aspect
### e.g., SR_1_2000_west
## model: LI-8100
## setup/settings: 240 seconds purge time, 180 measurement
## file-type: .81x
## start at plot 1, end at 5

#arguments/required input: 
## filepath
## area
## volume
## time of interest 

readSR <- function(files, toi = 120:179){ ##default time is between 120 and 179 seconds
  
  #load required functions
  library(RespChamberProc)
  library(tidyverse)

# Read in data
tempSR <- map_df(set_names(files), function(file) {
  file %>%
    set_names() %>%
    map_df(~ RespChamberProc::read81xVar(fName = file)) #important! reading in American format
}, .id = "File")


SR <- tempSR %>% 
mutate(File = basename(File),
       File = str_remove(File, ".81x")
      ) |>
  # Separate into relevant info
  separate(File, into = c("flux", "siteID", "elevation", "aspect"), remove = FALSE) |>
  # Rename site and plot so they behave
  mutate(siteID = paste0("Site_", siteID),
         plotID = paste0("Plot_", iChunk),
         aspect = str_to_lower(aspect), 
         uniqueID = paste0(siteID, "_", elevation, "_", aspect, "_", plotID)
  ) %>% 
  #select only measurements from 120 seconds onwards (which is the most represenative window)
  filter(Etime %in% c(toi)) 


#str(SR)
return(SR)
}


