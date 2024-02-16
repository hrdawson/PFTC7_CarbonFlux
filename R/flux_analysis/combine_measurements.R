
## calculating NEE, ER, GPP, NPP, C use efficiency , Water use efficiency,
## GPP = NEE - ER
## NPP = GPP - (ER-SR)
## CUE = GPP/NPP
## ET
## T = H20 light - H20 dark (tarp)
## WUE = T / GPP

# load functions
source("R/functions/read_SR.R")
source("R/functions/calc_SR.R")
source("R/functions/flux_calc_own.R")
source("R/functions/fix_file_names.R")

#packages
library(co2fluxtent)
library(tidyverse)
library(data.table)

# TENT CO2 --------------
## Code for processing segmented flux data ----
# Import segmented flux data
# licor_nee = read_csv2("clean_data/segmented_fluxes_comments.csv") |>
  # filter(is.na(comment))

#modify and restructure the data
# Right now this version of `licor_nee` is made with the `Clean flagging of dud data.R`
dt.nee <- read.csv("clean_data/licor_nee_flagged.csv") |>
  filter(flag %in% c("okay", "manual_flux_time_selection")) |>
  select(-X) |>
  rename(file = filename) |>
  # Pivot
  select(site, plot, aspect, elevation, day.night, flux, nee_lm) |>
  pivot_wider(names_from = flux, values_from = nee_lm) |>
  # Calculate GPP
  mutate(GPP = case_when(
    !is.na(NEE) ~ as.numeric(NEE)-as.numeric(ER),
    TRUE ~ NA
  ))

dt.missing.nee = dt.nee |>
  filter(day.night == "day") |>
  filter(is.na(GPP))

write.csv(dt.nee, "clean_data/licor7500_carbon_fluxes.csv")

# Tent H2O ----
# Import segmented flux data

#modify and restructure the data
dt.et <- read.csv("clean_data/licor_et_flagged.csv") |>
  dplyr::filter(flag %in% c("okay", "manual_flux_time_selection")) |>
  # Remove unique columns
  select(site:day.night, flux, flux_lm) |>
  pivot_wider(names_from = flux, values_from = flux_lm) |>
  # Calculate transpiration
  mutate(TRANS = case_when(
    !is.na(EVAP) ~ as.numeric(ET)-as.numeric(EVAP),
    TRUE ~ NA
  ))

dt.missing.et = dt.et |>
  filter(day.night == "day") |>
  filter(is.na(TRANS))

# write.csv(dt.et, "clean_data/licor7500_ET_fluxes.csv")

# SOIL RESPIRATION ----------------------

#get file locations
filesSR <- dir(path = "raw_data/LI8100/", pattern = ".81x", full.names = TRUE, recursive = TRUE)

toi <- 120:179 #time of interest (not strictly necessary as this time is also the default)

#read SR
SR = read.csv("clean_data/LI8100_combined.csv") |>
  add_column(toi = toi)

SR <- readSR(files = filesSR, toi = toi)
names(SR)

ggplot(data = SR, aes(x = Etime, y = Cdry, color = iChunk)) +
  geom_point() +
  scale_color_viridis_c() +
  facet_wrap(~ elevation) +
  theme_minimal()

#calc SR
dt.sr <- calcSR(data = SR)
dt.sr <- as.data.table(dt.sr)
dt.sr <- dt.sr[!co2_flux_sr < 0]

dt.soilResp = dt.sr |>
  rename(CO2 = co2_flux_sr, H2O = h2o_flux_sr) |>
  # Extract site and plot names and numbers for consistency
  mutate(site = str_sub(siteID, -1, -1),
         plot = str_sub(plotID, -1, -1)) |>
  relocate(site, plot, .after = plotID) |>
  select(site:aspect, CO2, H2O)

# fwrite(dt.sr, "outputs/2023.12.19_SR_originalCalc.csv")
# write.csv(dt.soilResp, "clean_data/licor8100_soil_fluxes.csv")

# Archived way of doing this### ----

# fix_file_names(path = "raw_data/LI7500/")
# 
# list.files("raw_data/LI7500/", recursive = TRUE)
# # Look for flux files in a folder
# licor_files <- Map(c, co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 1/"),
#                    co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 2/"),
#                    co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 3/"),
#                    co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 4/"),
#                    co2fluxtent::read_files("raw_data/LI7500/LI7500_Site 5/"))
# 
# 
# ## clean file names
# 
# 
# # Check if the files are ok
# licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 50) ##removed three files
# 
# print(licor_files)
# 
# # Gather site, plot etc. information from the filenames
# 
# meta <- tibble(file_path = unlist(licor_files),
#                file = basename(file_path)) %>%
#   mutate(site = unlist(lapply(file, function(x) str_split(x, "_")[[1]][1])),
#          elevation = unlist(lapply(file, function(x) str_split(x, "_")[[1]][2])),
#          aspect = unlist(lapply(file, function(x) str_split(x, "_")[[1]][3])),
#          plot = unlist(lapply(file, function(x) str_split(x, "_")[[1]][4])),
#          day_night = unlist(lapply(file, function(x) str_split(x, "_")[[1]][5])),
#          measurement = unlist(lapply(file, function(x) gsub(".txt","",tail(str_split(x, "_")[[1]],1)))),
#          redo = grepl("redo", file, ignore.case = T))
# 
# meta
# 
# 
# #look at measurements and calculate fluxes
# licor_nee <- licor_files %>%
#   flux_calc_own(param = "nee",
#                 skip = 3,
#                 vol = 1.2^3,
#                 area = 1.2^2,
#                 tstart = 20,
#                 tfinish = 80,
#                 signal_threshold = 95) %>%
#   mutate(filename = basename(filename))
# 
# # fwrite(licor_nee, "outputs/2023_12_19_licor_nee_for_HRD.csv")
# 
# #modify and restructure the data
# dt.nee <- licor_nee |>
#   rename(file = filename) |>
#   left_join(meta) |>
#   # Drop unnecessary parts of the file name
#   mutate(file = basename(file),
#          file = str_remove(file, ".txt")
#   ) |>
#   # Separate into relevant info
#   separate(file, into = c("siteID", "elevation", "aspect", "plotID", "day.night"), remove = FALSE) |>
#   # Rename site and plot so they behave
#   mutate(siteID = paste0("Site ", siteID),
#          plotID = paste0("Plot ", plotID),
#          # Add column for redos
#          redo = case_when(
#            str_detect(file, "redo") ~ "second",
#            str_detect(file, "redo2") ~ "third",
#            TRUE ~ "first"),
#          # Set flux type
#          flux = case_when(
#            str_detect(file, "photo") ~ "NEE",
#            str_detect(file, "resp") ~ "ER",
#            str_detect(file, "a") ~ "Ambient")
#   ) |>
#   # Create uniqueID
#   mutate(uniqueID = paste0(siteID, " ", aspect, " ", plotID, " ", day.night, ' ', flux)) |>
#   arrange(redo) |>
#   group_by(uniqueID) %>%
#   slice_tail(n = 1) |>
#   as.data.table()
# 
# #exclude extremely badly fitting models
# dt.nee <- dt.nee[lm_rsqd > 0.75, ]
# names(dt.nee)
# 
# dt.nee[, nee_best := ifelse(aic_lm < aic_nlm, nee_lm, nee_exp)]
# 
# #exclude outliers
# dt.nee <- dt.nee[!abs(nee_best) > 20, ]
# 
# #split up in resp and photo and join again later to calculate GPP
# dt.resp <- dt.nee[flux == "ER" & day_night == "day" , .(nee_best, day_night, plot, elevation, aspect, redo, file)]
# dt.photo <- dt.nee[flux == "NEE" & day_night == "day", .(nee_best, day_night, plot, elevation, aspect, redo, file, tav)]
# 
# ## COMBINE ----------------
# combine the relevant information from the different sources
dt.carb[, plotID := paste0("Plot_", plot)]
dt.carb[, temperature := tav]
dt.carb.sub <- dt.carb[,.(plotID, elevation, aspect, GPP, NEE, ER, temperature)]
summary(dt.carb.sub) # NA's in ER and thus in GPP
dt.water[, plotID := paste0("Plot_", plot)]
dt.water.sub <- dt.water[,.(plotID, elevation, aspect, TRANS, ET, EVAP)]
summary(dt.water.sub) #all good, all there
dt.sr.sub <- dt.sr[,.(plotID, elevation, aspect, co2_flux_sr, h2o_flux_sr, transectID)]
summary(dt.sr.sub) #all good, all there

dt.com <- left_join(dt.water.sub, dt.carb.sub, by = c("plotID", "aspect", "elevation"))
dt.com <- left_join(dt.com, dt.sr.sub, by = c("plotID", "aspect", "elevation"))
# 
# setnames(dt.resp, c("nee_best", "file", "redo"), c("resp_best", "resp_file", "resp_redo"))
# setnames(dt.photo, c("file", "redo"), c("nee_file", "nee_redo"))
# 
# dt.carb <- left_join(dt.photo, dt.resp, by = c("day_night", "plot", "elevation", "aspect"))
# 
# #calculate GPP
# dt.carb[, GPP := nee_best - resp_best,]
# dt.carb[, NEE := nee_best,]
# dt.carb[, ER := resp_best,]
# 
# # fwrite(dt.carb, "outputs/2023.12.19_Cflux_originalCalc.csv")
# 
# # WATER FLUXES -----
# #inspect measurements and calculate fluxes (actually, the function does the calculation for you)
# licor_et <- licor_files %>%
#   flux_calc_own(param = "et",
#                 skip = 3,
#                 vol = 1.2^3,
#                 area = 1.2^2,
#                 tstart = 20,
#                 tfinish = 80,
#                 signal_threshold = 95) %>%
#   mutate(filename = basename(filename))
# 
# # fwrite(licor_et, "outputs/2023_12_19_licor_et_for_HRD.csv")
# 
# #modify the data frame
# dt.et <- licor_et |>
#   rename(file = filename) |>
#   left_join(meta) |>
#   # Drop unnecessary parts of the file name
#   mutate(file = basename(file),
#          file = str_remove(file, ".txt")
#   ) |>
#   # Separate into relevant info
#   separate(file, into = c("siteID", "elevation", "aspect", "plotID", "day.night"), remove = FALSE) |>
#   # Rename site and plot so they behave
#   mutate(siteID = paste0("Site ", siteID),
#          plotID = paste0("Plot ", plotID),
#          # Add column for redos
#          redo = case_when(
#            str_detect(file, "redo") ~ "second",
#            str_detect(file, "redo2") ~ "third",
#            TRUE ~ "first"),
#          # Set flux type
#          flux = case_when(
#            str_detect(file, "photo") ~ "NEE",
#            str_detect(file, "resp") ~ "ER",
#            str_detect(file, "a") ~ "Ambient")
#   ) |>
#   # Create uniqueID
#   mutate(uniqueID = paste0(siteID, " ", aspect, " ", plotID, " ", day.night, ' ', flux)) |>
#   arrange(redo) |>
#   group_by(uniqueID) %>%
#   slice_tail(n = 1) |>
#   as.data.table()
# 
# #select best model
# dt.et <- dt.et  %>%
#   mutate(
#     et_best = ifelse(aic_lm < aic_nlm, flux_lm, flux_nlm)) %>%
#   # filter(lm_rsqd > 0.75) %>%
#       as.data.table()
# 
# 
# #split up in resp and photo and join again later to calculate GPP
# dt.tarp <- dt.et[flux == "ER" & day.night == "day" , .(et_best, day.night, plot, elevation, aspect, redo, file)]
# dt.light <- dt.et[flux == "NEE" & day.night == "day", .(et_best, day.night, plot, elevation, aspect, redo, file)]
# 
# setnames(dt.tarp, c("et_best", "file", "redo"), c("eva_best", "eva_file", "eva_redo"))
# setnames(dt.light, c("file", "redo"), c("nee_file", "nee_redo"))
# 
# dt.water <- left_join(dt.tarp, dt.light, by = c("day.night", "plot", "elevation", "aspect"))
# 
# #calculate GPP
# dt.water[, TRANS := et_best - eva_best,]
# dt.water[, ET := et_best,]
# dt.water[, EVAP := eva_best]
# 
# # fwrite(dt.water, "outputs/2023.12.19_H2Oflux_originalCalc.csv")
# 
# 
# 
# 
# ## CALCULATE ------------------
# 
# ## NPP = GPP - (ER-SR)
# ## CUE = GPP/NPP
# ## WUE = T / GPP
# 
# #careful here, there is some flipping going on to make everything that got uptaken negative and everything that gets into the atmosphere positive.
# # gives e.g., a negative NPP and GPP which is weird but somewhat right
# dt.res <- dt.com %>% mutate(
#   ER = ER*-1,
#   GPP = GPP,
#   NEE = NEE*-1,
#   TRANS = TRANS,
#   ET = ET,
#   EVAP = EVAP,
#   NPP = GPP - (ER - co2_flux_sr),
#   CUE = NPP/GPP,
#   WUE = NPP / TRANS,
#   elevation = as.numeric(elevation)
# )
# 
# 
# quantile(dt.com$ER*-1, na.rm = TRUE)
# quantile(dt.com$co2_flux_sr, na.rm = TRUE)
# quantile(dt.com$PR, na.rm = TRUE)
# 
# #calculate transect means
# dt.res[, `:=` (NPP_mean = mean(NPP, na.rm = TRUE),
#                GPP_mean = mean(GPP, na.rm = TRUE),
#                 CUE_mean = mean(CUE, na.rm = TRUE),
#                ER_mean = mean(ER, na.rm = TRUE),
#                NEE_mean = mean(NEE, na.rm = TRUE),
#                SR_mean = mean(co2_flux_sr, na.rm = TRUE),
#                TRANS_mean = mean(TRANS, na.rm = TRUE),
#                ET_mean = mean(ET, na.rm = TRUE),
#                WUE_mean = mean(WUE, na.rm = TRUE)
#                ), by = transectID]
# 
# summary(dt.res) #interestingly, we seem do drop one GPP value here.
# 
# fwrite(dt.res, "outputs/prelim_flux_results.csv")
# 
# 
# 
# ## PLOT --------------------
# library(MetBrewer)
# 
# scale_color_met_d(name = "Cassatt2")
# 
# #Carbon plots ---------
# 
# a <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50", linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = GPP, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = - 1) +
#   scale_fill_met_d(name = "Cassatt2", direction = - 1) +
#   labs(y = "CO2 (µmol/m2/s)", title = "Gross primary productivity (GPP)", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# a
# 
# 
# b <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = NPP, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "CO2 (µmol/m2/s)", title = "Net primary productivity (NPP)", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# b
# 
# c <- ggplot(data = dt.res[!abs(CUE) > 1]) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = CUE, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "NPP/GPP", title = "Carbon use efficiency (CUE)", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# c
# 
# d <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = ER, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "CO2 (µmol/m2/s)", title = "Ecosystem respiration (ER)", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# d
# 
# e <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = NEE, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "CO2 (µmol/m2/s)", title = "Net ecosystem exchange (NEE)", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# e
# f <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = co2_flux_sr, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "CO2 (µmol/m2/s)", title = "Soil respiration (SR)", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# f
# 
# library(gridExtra)
# p.co2 <- grid.arrange(a, b, c, d, e, f, ncol = 3, widths = c(1,1,1.3))
# ggsave(plot = p.co2,"outputs/plots/prelim_c_stuff.png", dpi = 600, height = 6, width = 10)
# 
# ## correlation matrix Carbon
# library(ggcorrplot)
# summary(dt.res)
# c.cor <- round(cor(dt.res[!is.na(dt.res$CUE),.(CUE, NPP, NEE, GPP, ER, co2_flux_sr)]), 1)
# head(c.cor[, 1:6])
# 
# ggcorrplot(c.cor, hc.order = TRUE, type = "lower",
#            lab = TRUE)
# 
# 
# 
# #Water plots -------
# ggplot(data = dt.res) +
#   geom_line(aes(x = elevation, y = TRANS_mean, color = "TRANS"), linewidth = 1.2) +
#   geom_line(aes(x = elevation, y = ET_mean, color = "ET"), linewidth = 1.2) +
#   geom_line(aes(x = elevation, y = WUE_mean, color = "WUE"), linewidth = 1.2) +
#   facet_wrap(~ aspect) +
#   labs(y = "mmol/m2/s", title = "H2O") +
#   theme_minimal()
# 
# wa <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = ET*-1, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "H2O (mmol/m2/s)", title = "Evapo-transpiration", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# wa
# 
# wb <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = TRANS*-1, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "H2O (mmol/m2/s)", title = "Transpiration", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = "none")
# 
# wb
# 
# wc <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = WUE, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "NPP/Transpiration", title = "Water use efficiency", x = "Elevation") +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# wc
# 
# wd <- ggplot(data = dt.res) +
#   geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1, color = "grey50") +
#   geom_boxplot(aes(x = as.factor(elevation), y = EVAP*-1, fill = aspect, color = aspect), linewidth = 1.2, alpha = 0.7) +
#   scale_color_met_d(name = "Cassatt2", direction = -1) +
#   scale_fill_met_d(name = "Cassatt2", direction = -1) +
#   labs(y = "H2O (mmol/m2/s)", title = "Evaporation", x = "Elevation") +
#   theme_minimal() +
#   ylim(-1, 4) +
#   theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
# 
# wd
# 
# p.h2o <- grid.arrange(wa, wb, wc, wd, ncol = 3, widths = c(1, 1, 1.3))
# ggsave(plot = p.h2o,"outputs/plots/prelim_water_stuff.png", dpi = 600, width = 10, height = 6)
# 
# ## correlation matrix Carbon
# library(ggcorrplot)
# summary(dt.res)
# w.cor <- round(cor(dt.res[!is.na(dt.res$WUE),.(WUE, TRANS, h2o_flux_sr)]), 1)
# head(w.cor[, 1:6])
# 
# ggcorrplot(w.cor, hc.order = TRUE, type = "lower",
#            lab = TRUE)
# 
