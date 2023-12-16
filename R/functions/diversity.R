# Load necessary libraries
library(dplyr)
library(knitr)
#install.packages("kableExtra")
library(kableExtra)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
library(co2fluxtent)
library(tidyverse)
library(data.table)
#install.packages("RespChamberProc")
library(RespChamberProc)
library(ggdist)
library(ggbeeswarm)
#install.packages("ggbeeswarm")
#install.packages("ggdist")
##HOW TO DO IT NICE####
## calculating NEE, ER, GPP, NPP, C use efficiency , Water use efficiency, 
## GPP = NEE - ER
## NPP = GPP - (ER-SR)
## CUE = GPP/NPP
## ET 
## T = H20 light - H20 dark (tarp)
## WUE = T / GPP 

# load functions
source("./read_SR.R")
source("./calc_SR.R")
source("./flux_calc_own.R")

#packages
library(co2fluxtent)
library(tidyverse)
library(data.table)


## TENT CO2 --------------
# Look for flux files in a folder
licor_files <- Map(c, co2fluxtent::read_files("raw_data/Site 1/"),
                   
                   co2fluxtent::read_files("raw_data/Site 2/"),
                   co2fluxtent::read_files("raw_data/Site 3/"),
                   co2fluxtent::read_files("raw_data/Site 4/"),
                   co2fluxtent::read_files("raw_data/Site 5/"))

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


#look at measurements and calculate fluxes 
licor_nee <- licor_files %>% 
  flux_calc_own(param = "nee", 
                skip = 3,
                vol = 1.2^3,
                area = 1.2^2, 
                tstart = 20, 
                tfinish = 80,
                signal_threshold = 95) %>%  
  mutate(filename = basename(filename)) 

#modify and restructure the data
dt.nee <- licor_nee |>
  rename(file = filename) |>
  left_join(meta) |>
  # Drop unnecessary parts of the file name
  mutate(file = basename(file),
         file = str_remove(file, ".txt")
  ) |>
  # Separate into relevant info
  separate(file, into = c("siteID", "elevation", "aspect", "plotID", "day.night"), remove = FALSE) |>
  # Rename site and plot so they behave
  mutate(siteID = paste0("Site ", siteID),
         plotID = paste0("Plot ", plotID),
         # Add column for redos
         redo = case_when(
           str_detect(file, "redo") ~ "second",
           str_detect(file, "redo2") ~ "third",
           TRUE ~ "first"),
         # Set flux type
         flux = case_when(
           str_detect(file, "photo") ~ "NEE",
           str_detect(file, "resp") ~ "ER",
           str_detect(file, "a") ~ "Ambient")
  ) |>
  # Create uniqueID
  mutate(uniqueID = paste0(siteID, " ", aspect, " ", plotID, " ", day.night, ' ', flux)) |>
  arrange(redo) |> 
  group_by(uniqueID) %>% 
  slice_tail(n = 1) |> 
  as.data.table()

#exclude extremely badly fitting models
#dt.nee <- dt.nee[lm_rsqd > 0.75, ]
names(dt.nee)

dt.nee[, nee_best := ifelse(aic_lm < aic_nlm, nee_lm, nee_exp)]

#exclude outliers 
dt.nee <- dt.nee[!abs(nee_best) > 20, ]

#split up in resp and photo and join again later to calculate GPP 
dt.resp <- dt.nee[flux == "ER" & day_night == "day" , .(nee_best, day_night, plot, elevation, aspect, redo, file)]
dt.photo <- dt.nee[flux == "NEE" & day_night == "day", .(nee_best, day_night, plot, elevation, aspect, redo, file)]



setnames(dt.resp, c("nee_best", "file", "redo"), c("resp_best", "resp_file", "resp_redo"))
setnames(dt.photo, c("file", "redo"), c("nee_file", "nee_redo"))

dt.carb <- left_join(dt.photo, dt.resp, by = c("day_night", "plot", "elevation", "aspect"))

#calculate GPP
dt.carb[, GPP := nee_best - resp_best,]
dt.carb[, NEE := nee_best,]
dt.carb[, ER := resp_best,]



# WATER FLUXES -----
#inspect measurements and calculate fluxes (actually, the function does the calculation for you)
licor_et <- licor_files %>% 
  flux_calc_own(param = "et", 
                skip = 3,
                vol = 1.2^3,
                area = 1.2^2, 
                tstart = 20, 
                tfinish = 80,
                signal_threshold = 95) %>%  
  mutate(filename = basename(filename)) 

#modify the data frame
dt.et <- licor_et |>
  rename(file = filename) |>
  left_join(meta) |>
  # Drop unnecessary parts of the file name
  mutate(file = basename(file),
         file = str_remove(file, ".txt")
  ) |>
  # Separate into relevant info
  separate(file, into = c("siteID", "elevation", "aspect", "plotID", "day.night"), remove = FALSE) |>
  # Rename site and plot so they behave
  mutate(siteID = paste0("Site ", siteID),
         plotID = paste0("Plot ", plotID),
         # Add column for redos
         redo = case_when(
           str_detect(file, "redo") ~ "second",
           str_detect(file, "redo2") ~ "third",
           TRUE ~ "first"),
         # Set flux type
         flux = case_when(
           str_detect(file, "photo") ~ "NEE",
           str_detect(file, "resp") ~ "ER",
           str_detect(file, "a") ~ "Ambient")
  ) |>
  # Create uniqueID
  mutate(uniqueID = paste0(siteID, " ", aspect, " ", plotID, " ", day.night, ' ', flux)) |>
  arrange(redo) |> 
  group_by(uniqueID) %>% 
  slice_tail(n = 1) |> 
  as.data.table()

#select best model 
dt.et <- dt.et  %>% 
  mutate(
    et_best = ifelse(aic_lm < aic_nlm, flux_lm, flux_nlm)) %>% 
  # filter(lm_rsqd > 0.75) %>% 
  as.data.table()


#split up in resp and photo and join again later to calculate GPP 
dt.tarp <- dt.et[flux == "ER" & day.night == "day" , .(et_best, day.night, plot, elevation, aspect, redo, file)]
dt.light <- dt.et[flux == "NEE" & day.night == "day", .(et_best, day.night, plot, elevation, aspect, redo, file)]



setnames(dt.tarp, c("et_best", "file", "redo"), c("eva_best", "eva_file", "eva_redo"))
setnames(dt.light, c("file", "redo"), c("nee_file", "nee_redo"))

dt.water <- left_join(dt.tarp, dt.light, by = c("day.night", "plot", "elevation", "aspect"))

#calculate GPP
dt.water[, TRANS := et_best - eva_best,]
dt.water[, ET := et_best,]


## SOIL RESPIRATION ----------------------

#get file locations
filesSR <- dir(path = "../data/raw_data/", pattern = ".81x", full.names = TRUE, recursive = TRUE)

toi <- 120:179 #time of interest (not strictly necessary as this time is also the default)

#read SR
SR <- readSR(files = filesSR, toi = toi)
names(SR)

#calc SR
dt.sr <- calcSR(data = SR)
dt.sr <- as.data.table(dt.sr)

##exclude site 4 for now as it's not yet measured

dt.sr <- dt.sr[!elevation == 2600,]

## COMBINE ----------------
# combine the relevant information from the different sources 
dt.carb[, plotID := paste0("Plot_", plot)]
dt.carb.sub <- dt.carb[,.(plotID, elevation, aspect, GPP, NEE, ER)] 
dt.water[, plotID := paste0("Plot_", plot)]
dt.water.sub <- dt.water[,.(plotID, elevation, aspect, TRANS, ET)] 
dt.sr.sub <- dt.sr[,.(plotID, elevation, aspect, co2_flux_sr, h2o_flux_sr, transectID)]


dt.com <- left_join(dt.water.sub, dt.carb.sub, by = c("plotID", "aspect", "elevation"))
dt.com <- left_join(dt.com, dt.sr.sub, by = c("plotID", "aspect", "elevation"))



## CALCULATE ------------------

## NPP = GPP - (ER-SR)
## CUE = GPP/NPP
## WUE = T / GPP 

#careful here, there is some flipping going on to make everything that got uptaken negative and everything that gets into the atmosphere positive.
# gives e.g., a negative NPP and GPP which is weird but somewhat right
dt.res <- dt.com %>% mutate(
  ER = ER*-1,
  GPP = GPP*-1,
  NEE = NEE*-1,
  TRANS = TRANS*-1,
  ET = ET*-1,
  NPP = GPP + (ER - co2_flux_sr), 
  CUE = NPP/GPP,
  WUE = NPP / TRANS,
  elevation = as.numeric(elevation)
)

#calculate transect means
dt.res[, `:=` (NPP_mean = mean(NPP, na.rm = TRUE),
               GPP_mean = mean(GPP, na.rm = TRUE),
               CUE_mean = mean(CUE, na.rm = TRUE),
               ER_mean = mean(ER, na.rm = TRUE),
               NEE_mean = mean(NEE, na.rm = TRUE),
               SR_mean = mean(co2_flux_sr, na.rm = TRUE), 
               TRANS_mean = mean(TRANS, na.rm = TRUE),
               ET_mean = mean(ET, na.rm = TRUE),
               WUE_mean = mean(WUE, na.rm = TRUE)
), by = transectID]

str(dt.res)

#HOW TO DO IT NOT SO NICE#####

#Read the CSV file
data <- read.csv("veg_cover.csv")

# Convert 'Cover' column to numeric
data$Cover <- as.numeric(data$Cover)

# Function to calculate Shannon Diversity Index
shannon_diversity <- function(cover_values) {
  proportions <- cover_values / sum(cover_values, na.rm = TRUE)
  -sum(proportions * log(proportions), na.rm = TRUE)
}

# Calculate Shannon Diversity Index for each plot
diversity_results <- data %>%
  group_by(plotID) %>%
  summarise(Shannon_Index = shannon_diversity(Cover))

# Print the results
print(diversity_results)

# Create a table with styling
table <- kable(diversity_results, format = "html", caption = "Shannon Diversity Index for Each Plot") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Print the styled table
print(table)

# Plot the results
# ggplot(diversity_results, aes(x = plotID, y = Shannon_Index, fill = plotID)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   labs(title = "Shannon Diversity Index for Each Plot",
#        x = "Plot ID", y = "Shannon Diversity Index") +
#   theme_minimal()

# Extract site information from plotID
diversity_results$Site <- gsub("_E_.*|_W_.*", "", diversity_results$plotID)
# Extract slope information from plotID
diversity_results$Slope <- gsub("s_.*_(E|W)_.*", "\\1", diversity_results$plotID)

# Plotting
# ggplot(diversity_results, aes(x = factor(Site), y = Shannon_Index, fill = factor(Site))) +
#   stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black") +
#   stat_summary(
#     fun.data = mean_se,
#     geom = "errorbar",
#     position = position_dodge(0.9),
#     color = "black",
#     width = 0.2
#   ) +
#   facet_wrap(~Slope) +
#   labs(title = "Mean Shannon Diversity Index by Site and Slope",
#        x = "Site", y = "Mean Shannon Diversity Index") +
#   theme_minimal()

# Plotting with letters indicating different confidence levels
ggplot(diversity_results, aes(x = factor(Site), y = Shannon_Index, fill = factor(Site))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge", color = "black") +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(0.9),
    color = "black",
    width = 0.2
  ) +
  facet_wrap(~Slope) +
  labs(title = "Mean Shannon Diversity Index by Site and Slope",
       x = "Site", y = "Mean Shannon Diversity Index") +
  theme_minimal() +
  stat_compare_means(comparisons = list(c("s_1", "s_2"), c("s_2", "s_3"), c("s_3", "s_4"), c("s_4", "s_5")),
                     method = "t.test", label = "p.signif")


#Now I need to relate this information with the licor nee data

# Read the CSV file
licor_data <- read.csv("licor_nee.csv")

#I extract only the columns I want to use
licor_data <- select(licor_data, filename, nee_lm)

# Create a mapping between original and desired values
site_mapping <- c("2000" = "s_1", "2200" = "s_2", "2400" = "s_3", "2600" = "s_4", "2800" = "s_5")

# Extract Site and Slope from filename
licor_data <- licor_data %>%
  mutate(
    Site = sub("\\d+_(\\d+)_.*", "\\1", filename),
    Site = ifelse(Site %in% names(site_mapping), site_mapping[Site], as.character(Site)),
    Slope = ifelse(grepl("_east_", filename), "E", "W")
  )

# Extract Site and Slope from filename
licor_data <- licor_data %>%
  mutate(
    Site = sub("\\d+_(\\d+)_.*", "\\1", filename),
    Site = ifelse(Site %in% names(site_mapping), site_mapping[Site], as.character(Site)),
    Slope = ifelse(grepl("_east_", filename), "E", "W"),
    Time = ifelse(grepl("_day_", filename), "day", "night"),  # Add Time column
    Measurement = ifelse(grepl("_photo", filename), "photo", "resp")  # Add Measurement column
  )

# Create the plotID column
licor_data <- licor_data %>%
  group_by(Site, Slope) %>%
  mutate(
    plotID = paste0("", Site, "_", Slope, "_", row_number())
  ) %>%
  ungroup()

#Now we need to join the df:

# Merge the two dataframes
data_full <- left_join(licor_data, diversity_results, by = "plotID")

# Print the merged dataframe
print(data_full)

# Select columns 1, 2, 4, 6, 7, 8
data_full <- data_full[, c(2:8)]

#Now we can plot nee vs shannon
# Plotting
ggplot(data_full, aes(x = Shannon_Index, y = nee_lm, color = Site.x)) +
  stat_halfeye(aes(fill = Site.x), width = 0.2, alpha = 0.2) +
  facet_grid(Measurement ~ Slope.x, scales = "free", switch = "y") +
  labs(title = "Scatter Plots of nee_lm vs Shannon Index ",
       x = "Shannon Index", y = "nee_lm") +
  theme_minimal() +
  theme(legend.position = "top")

