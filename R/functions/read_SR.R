

# library(devtools)
# devtools::install_github("bgctw/RespChamberProc")

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

filesSR <- dir(path = "../data/raw_data/", pattern = ".81x", full.names = TRUE, recursive = TRUE)

toi <- 120:179 #time of interest

getSR <- function(files, toi = 120:179){ ##default time is between 120 and 179 seconds
  
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

SR <- getSR(files = filesSR)
names(SR)
#check out the file
ggplot(data = SR[!SR$siteID == "Site_4", ], aes(x = Etime, y = CO2, color = plotID)) +
  geom_point() +
  scale_color_viridis_d() +
  facet_grid(aspect~siteID) +
  theme_bw()

## Calculate fluxes 

calcSR <- function(data, area = 317.8, volume = 1807.6){
  
  #load dependencies
  library(tidyverse)
  
#build empty dataframes
tmp <- data.frame(co2_flux = NA, co2_rsq = NA, 
                    h2o_flux = NA, h20_rsq = NA, 
                    plotID = NA, siteID = NA,
                    elevation = NA, aspect = NA, 
                    uniqueID = NA, transectID = NA,
                    co2_flux_m2 = NA, h2o_flux_m2 = NA)

df.res <- data.frame(co2_flux = NA, co2_rsq = NA, 
                  h2o_flux = NA, h20_rsq = NA, 
                  plotID = NA, siteID = NA,
                  elevation = NA, aspect = NA, 
                  uniqueID = NA, transectID = NA, 
                  co2_flux_m2 = NA, h2o_flux_m2 = NA)


area <- 317.8 #most likely in cm2
volume <- 1807.6

for(unique in unique(data$uniqueID)){
  
  
  #model
  lm.co2 <- lm(CO2 ~ Etime, data = data[data$uniqueID == unique, ])
  lm.h2o <- lm(H2O ~ Etime, data = data[data$uniqueID == unique, ])
  
  #CO2
tmp <- tmp %>% mutate(
    co2_flux = lm.co2$coefficients[2], #slope in ppm/second
    co2_rsq = summary(lm.co2)$r.sq,
  #H2O
    h2o_flux = lm.h2o$coefficients[2], #slope in ppm/second
    h20_rsq = summary(lm.h2o)$r.sq,
  #General information
      plotID = unique(data[data$uniqueID == unique, ]$plotID),
      siteID = unique(data[data$uniqueID == unique, ]$siteID),
      uniqueID = unique(data[data$uniqueID == unique, ]$uniqueID),
      elevation = unique(data[data$uniqueID == unique, ]$elevation),
      aspect = unique(data[data$uniqueID == unique, ]$aspect),
      transectID = unique(paste0(data[data$uniqueID == unique, ]$elevation, "_", unique(data[data$uniqueID == unique, ]$aspect)))
    ) %>% mutate(
      co2_flux_m2 = co2_flux*(10000/area), 
      h2o_flux_m2 = h2o_flux*(10000/area)
      
    )
  
df.res <- rbind(df.res, tmp)

df.res <- df.res[!is.na(df.res$uniqueID), ]
  
}

return(df.res)

}

res <- calcSR(data = SR)
df.res <- res

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
