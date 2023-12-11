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
  tmp <- data.frame(co2_dry = NA, co2_rsq = NA, 
                    h2o_raw = NA, h2o_rsq = NA, 
                    plotID = NA, siteID = NA,
                    elevation = NA, aspect = NA, 
                    uniqueID = NA, transectID = NA,
                    co2_flux = NA, h2o_flux = NA)
  
  df.res <- data.frame(co2_dry = NA, co2_rsq = NA, 
                       h2o_raw = NA, h2o_rsq = NA, 
                       plotID = NA, siteID = NA,
                       elevation = NA, aspect = NA, 
                       uniqueID = NA, transectID = NA,
                       co2_flux = NA, h2o_flux = NA)
  
  
  area <- 317.8/10000 #most likely in m2
  volume <- 1807.6/1000000
  R <- 8.314472
  
  
  for(unique in unique(data$uniqueID)){
    
    temp <- mean(as.numeric(data[data$uniqueID == unique, ]$Tcham), na.rm = TRUE)
    pressure <- mean(as.numeric(data[data$uniqueID == unique, ]$Pressure), na.rm = TRUE)
    
    #model
    lm.co2 <- lm(Cdry ~ Etime, data = data[data$uniqueID == unique, ])
    lm.h2o <- lm(H2O ~ Etime, data = data[data$uniqueID == unique, ])
    
    
    
    #CO2
    tmp <- tmp %>% mutate(
      co2_dry = lm.co2$coefficients[2], #slope in ppm/second
      co2_rsq = summary(lm.co2)$r.sq,
      #H2O
      h2o_raw = lm.h2o$coefficients[2], #slope in ppm/second
      h2o_rsq = summary(lm.h2o)$r.sq,
      #General information
      plotID = unique(data[data$uniqueID == unique, ]$plotID),
      siteID = unique(data[data$uniqueID == unique, ]$siteID),
      uniqueID = unique(data[data$uniqueID == unique, ]$uniqueID),
      elevation = unique(data[data$uniqueID == unique, ]$elevation),
      aspect = unique(data[data$uniqueID == unique, ]$aspect),
      transectID = unique(paste0(data[data$uniqueID == unique, ]$elevation, "_", unique(data[data$uniqueID == unique, ]$aspect)))
    ) %>% mutate(
      co2_flux_sr = (volume * pressure * (1000) * co2_dry)/(R * area * (temp + 273.15)), 
      h2o_flux_sr = (volume * pressure * (1000) * h2o_raw)/(R * area * (temp + 273.15))
      
      
    )
    
    df.res <- rbind(df.res, tmp)
    
    df.res <- df.res[!is.na(df.res$uniqueID), ]
    
  }
  
  return(df.res)
  
}
