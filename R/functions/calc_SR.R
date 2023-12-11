## Code by Jonas Trepel, Hilary Rose Dawson & Kristine Birkeli

#required packages
library(tidyverse)
#library(tidylog)

#assumptions: read SR files with the readSR function

#arguments/required input: 
## data 
## area
## volume

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
