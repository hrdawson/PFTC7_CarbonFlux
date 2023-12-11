
require(dplyr)
require(lubridate)

join_PAR_LICOR <- function(PAR, LICOR, PAR_tz = NULL, LICOR_tz = NULL){
  if(!is.null(PAR_tz) & !is.null(LICOR_tz)){
    
    PAR$POSIXct_uc <- as.POSIXct(paste(PAR$Date, PAR$Time),
                              format="%Y-%m-%d %H:%M:%S", 
                              tz = PAR_tz)
    
    LICOR$POSIXct <- as.POSIXct(paste(LICOR$Date, LICOR$Time), 
                                    format="%Y-%m-%d %H:%M:%S", 
                                    tz = LICOR_tz)
    
    common_time_zone = LICOR_tz
    
    PAR$POSIXct <- with_tz(PAR$POSIXct_uc, tzone = common_time_zone)
  }
  else if(!is.null(PAR_tz) & is.null(LICOR_tz)) {
    print("ERROR: Must specify tz for both datasets. Function returned NA")
    return(NA)
  }
  else if(is.null(PAR_tz) & !is.null(LICOR_tz)){
    print("ERROR: Must specify tz for both datasets. Function returned NA")
    return(NA)
  }
 else{
    common_time_zone = "UTC"
    PAR$POSIXct <- as.POSIXct(paste(PAR$Date, PAR$Time),
                                 format="%Y-%m-%d %H:%M:%S", 
                                 tz = common_time_zone)
    
    LICOR$POSIXct <- as.POSIXct(paste(LICOR$Date, LICOR$Time), 
                                format="%Y-%m-%d %H:%M:%S", 
                                tz = common_time_zone)
  }
  
  PAR_LICOR <- inner_join(PAR, LICOR, by="POSIXct")
  
  return(PAR_LICOR)
}
