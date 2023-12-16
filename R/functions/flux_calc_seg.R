require(cpop)
require(ggplot2)

flux_calc_seg <- function(flux_files, PAR_files,
                          dat_sel="weighted avg",
                          param = "nee",
                          licor_skip=3,
                          par_skip=7,
                          vol = 2.197,
                          area = 1.69){
  
  PAR_dat <- read_delim(PAR_files[1], delim="\t", skip = par_skip)
  if(length(PAR_files) > 1){
    for(f in c(2:length(PAR_files)){
      PAR_dat <- cbind(PAR_dat, 
                       read_delim(PAR_files[f], delim="\t", skip = par_skip))
    }
    
  }
  
  LI_dat <- read.csv(licor_files[j], sep="\t", skip = licor_skip) 
  LI_PAR_dat[[j]] <- join_PAR_LICOR(PAR, LI_dat, 
                                      PAR_tz = "America/Mexico_City", 
                                      LICOR_tz = "Africa/Johannesburg")
  
  
  flux_fit <- function(filename, PAR){
    R <- 8.314472
    
    time <- as.numeric(d$`Sequence Number`)
    co2 <- as.numeric(d$`CO2 (umol/mol)`)
    h2o <- as.numeric(d$`H2O (mmol/mol)`)
    par <- as.numeric(d$INPUT2)
    press <- as.numeric(d$`Pressure (kPa)`)
    temp <- as.numeric(d$`Temperature (C)`)
    signal_strength <- as.numeric(d$`CO2 Signal Strength`)
    
    tav <- mean(temp)
    pav <- mean(press)
    cav <- mean(co2)
    wav <- mean(h2o)
    cprime <- co2/(1 - (h2o/1000))
    wprime <- h2o/(1 - (h2o/1000))
    wav_dil <- mean(h2o/(1 - (h2o/1000)))
    camb <- mean(as.numeric(as.character(ambient$`CO2 (umol/mol)`))/(1 - (as.numeric(as.character(ambient$`H2O (mmol/mol)`))/1000)))
    wamb <- mean(as.numeric(as.character(ambient$`H2O (mmol/mol)`))/(1 - (as.numeric(as.character(ambient$`H2O (mmol/mol)`))/1000)))
    
    res <- cpop(cprime, minseglen = 30, beta = 2*log(length(cprime)))
    changepoints(res)
    cprime_seg <- fitted(res)
    
    if(data_sel == "manual"){
      print(cprime_seg)
      
      p2 <- ggplot(aes(y=`CO2 (umol/mol)`, x=`Sequence Number`-min(`Sequence Number`)),
                       data=d) + 
        geom_point(color="red")  + 
        geom_line(color="red") + 
        ylim(min(d$`CO2 (umol/mol)`), max(d$`CO2 (umol/mol)`)) + 
        ylab("CO2") + xlab("Time") + geom_vline(xintercept = cprime_seg$x0)
      
      p1 <- ggplot(aes(y=INPUT1, x=`Sequence Number`-min(`Sequence Number`)), 
                   data=d) + 
        geom_point(color="blue") + geom_line(color="blue") + 
        ylim(min(d$INPUT1), max(d$INPUT1)) +
        ylab("PAR") + xlab("Time") + geom_vline(xintercept = cprime_seg$x0 )
      p_c <- plot_grid(p1, p2, ncol=1, align="v", axis=1)

      plot(p_c)

      segs <- readline("Choose segments for response curve (segment numbers separated by comma)")

      segs <- lapply(str_split(segs, pattern = ","), as.numeric)
      
      
    }
    else if(dat_sel == "weighted avg"){
      segs <- c(1:nrow(cprime_seg))
      aic.lm <- c()
      inter <- ac()
      dcw_dt <- c()
      rsqd <- c()
      param_lm <- c()
      i = 1
      
      for(s in segs){
        end <- length(par)
        
        s1 <- cprime_seg$x0[s]
        if(s == nrow(cprime_seg)){
          s2 <- end
        }
        else{
          s2 <- cprime_seg$x0[s+1]
        }
        
        MSS <- mean(signal_strength[s1:s2])
        Mpar <- mean(par[s1:s2])
        
        if ("nee" == param) {
          cw_prime <- cprime
        } else if ("et" == param) {
          cw_prime <- wprime
        }
        if ("nee" == param) {
          tag <- "c_prime"
        } else if ("et" == param) {
          tag <- "w_prime"
        }
        
        if(MSS > SS_thresh && Mpar > par_thresh){
          linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time[s1:s2]))
          aic.lm[i] <- stats::AIC(linear.fit)
          inter[i] <- as.numeric(linear.fit$coeff[1])
          dcw_dt[i] <- as.numeric(linear.fit$coeff[2])
          rsqd[i] <- summary(linear.fit)$r.sq
          if ("nee" == param) {
            param_lm[i] <- -(vol * pav * (1000) * dcw_dt)/(R * area *
                                                          (tav + 273.15))
          } 
          else if ("et" == param) {
            param_lm[i] <- (vol * pav * (1000) * dcw_dt)/(R * area *
                                                         (tav + 273.15))
          }
          i <- i+1
        }
      }
      
      aic_avg <- mean(aic.lm)
      inter_avg <- mean(inter)
      dcw_dt_avg <- mean(dcw_dt)
      rsqd_avg <- mean(rsqd)
      
      if ("nee" == param) {
        print(tibble::tibble(filename = filename, tstart = tstart,
                             tfinish = tfinish, nee_lm = param_lm, nee_exp = nee_exp,
                             rsqd = rsqd, sigma = sigma, aic.lm = aic.lm,
                             aic.nlm = aic.nlm,
                             c_prime_min = min(cw_prime[tstart:tfinish], na.rm = T),
                             c_prime_max = max(cw_prime[tstart:tfinish], na.rm = T)))
        result <- tibble::tibble(filename = filename, tstart = tstart,
                                 tfinish = tfinish, nee_lm = param_lm, nee_exp = nee_exp,
                                 rsqd = rsqd, sigma = sigma, aic.lm = aic.lm,
                                 aic.nlm = aic.nlm,
                                 c_prime_min = min(cw_prime[tstart:tfinish], na.rm = T),
                                 c_prime_max = max(cw_prime[tstart:tfinish], na.rm = T))
      
    }
    else if(dat_sel == "best fit"){
        segs <- c(1:nrow(cprime_seg))
        
        for(s in segs){
          end <- length(par)
          rsqd_max <- 0
          
          s1 <- cprime_seg$x0[s]
          if(s == nrow(cprime_seg)){
            s2 <- end
          }
          else{
            s2 <- cprime_seg$x0[s+1]
          }
          
          MSS <- mean(signal_strength[s1:s2])
          Mpar <- mean(par[s1:s2])
          
          if ("nee" == param) {
            cw_prime <- cprime
          } else if ("et" == param) {
            cw_prime <- wprime
          }
          if ("nee" == param) {
            tag <- "c_prime"
          } else if ("et" == param) {
            tag <- "w_prime"
          }
          
          if(MSS > SS_thresh && Mpar > par_thresh){
            linear.fit <- stats::lm(cprime[s1:s2] ~ (time[s1:s2]))
            rsqd <- summary(linear.fit)$r.sq
            
            if(rsqd > rsqd_max){
              rsqd_max <- rsqd
              linear.fit <- stats::lm(cw_prime[s1:s2] ~ (time[s1:s2]))
              aic.lm <- stats::AIC(linear.fit)
              inter <- as.numeric(linear.fit$coeff[1])
              dcw_dt <- as.numeric(linear.fit$coeff[2])
              rsqd <- summary(linear.fit)$r.sq
              
              if ("nee" == param) {
                param_lm <- -(vol * pav * (1000) * dcw_dt)/(R * area *
                                                              (tav + 273.15))
              } 
              else if ("et" == param) {
                param_lm <- (vol * pav * (1000) * dcw_dt)/(R * area *
                                                             (tav + 273.15))
              }
            }
          }
        }
      }
    }
  
  if(param == "nee")
  
  return(result)
  
  }
}
  