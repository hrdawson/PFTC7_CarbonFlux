
## Code by Jonas Trepel, Hilary Rose Dawson & Kristine Birkeli

#required packages
library(tidyverse)
#library(tidylog)

#assumptions: 
## naming: SR_site_elevation_aspect
### e.g., SR_1_2000_west
## model: LI-8100
## setup/settings: 240 seconds purge time, 180 measurement
## file-type: .81x
## start at plot 1, end at 5

# load functions
source("R/functions/read_SR.R")
source("R/functions/calc_SR.R")

#get file locations
filesSR <- dir(path = "../data/raw_data/", pattern = ".81x", full.names = TRUE, recursive = TRUE)

toi <- 120:179 #time of interest

#read SR
SR <- readSR(files = filesSR)
names(SR)

#check out the file
ggplot(data = SR[!SR$siteID == "Site_4", ], aes(x = Etime, y = CO2, color = plotID)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(aspect~siteID) +
  theme_bw()

#calc SR
df.res <- calcSR(data = SR)


#Visualize fluxes
ggplot(data = df.res, aes(x = aspect, y = co2_flux_m2, fill = aspect)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(size = 4, alpha = 0.5) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Soil respiration", x = "Aspect", y = "Respiration (ppm/s/m2)") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))

ggplot(data = df.res, aes(x = aspect, y = h2o_flux_m2, fill = aspect)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(size = 4, alpha = 0.5) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Soil respiration", x = "Aspect", y = "Respiration (ppm/s/m2)") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
