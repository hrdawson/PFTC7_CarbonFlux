library(tidyverse)
library(data.table)
library(traitstrap)


#load data 

#cover 
vegcov <- fread("raw_data/veg_cover_13Dec.csv", header = TRUE)
vegcov <- vegcov[,-1]
#traits
traits <- fread("raw_data/PFTC7_SA_raw_traits_2023 - Gradient.csv")
#flux results 
dt <- fread("outputs/prelim_flux_results.csv")


#clean traits: 

traits <- traits[!is.na(traits$ID) & !aspect == "" & !is.na(traits$plotID) & !plotID %in% c(0, 6) & !project == "P"  ,]
str(traits)
traits <- traits %>% mutate( 
  leaf_thickness_1_mm = as.numeric(leaf_thickness_1_mm),
  leaf_thickness_2_mm = as.numeric(leaf_thickness_2_mm),
  leaf_thickness_3_mm = as.numeric(leaf_thickness_3_mm),
  veg_height_cm = as.numeric(veg_height_cm),
  rep_height_cm = as.numeric(rep_height_cm),
  wet_mass_g = as.numeric(wet_mass_g),
  leaf_thickness_mm = ((leaf_thickness_1_mm +  leaf_thickness_2_mm + leaf_thickness_3_mm) / 3), 
  leaf_wet_mass = wet_mass_g/bulk_nr,
  elevation_clean = case_when(
    siteID == 1 ~ 2000,
    siteID == 2 ~ 2200,
    siteID == 3 ~ 2400,
    siteID == 4 ~ 2600,
    siteID == 5 ~ 2800,
  ),
  plotID = paste0("Plot_", plotID),
  elevation = elevation_clean,
  transectID = paste0(elevation, "_", aspect), ) %>% as.data.table()

#clean cover data
cov <- vegcov %>% 
  rename(plotID_old = plotID) %>%
  mutate(
  siteID = case_when(
    grepl("s_5", plotID_old) ~ 5,
    grepl("s_4", plotID_old) ~ 4,
    grepl("s_3", plotID_old) ~ 3,
    grepl("s_2", plotID_old) ~ 2,
    grepl("s_1", plotID_old) ~ 1
  ), 
  aspect = case_when(
    grepl("W", plotID_old) ~ "west",
    grepl("E", plotID_old) ~ "east"
  ), 
  plotID = case_when(
    grepl("W_5", plotID_old) ~ "Plot_5",
    grepl("W_4", plotID_old) ~ "Plot_4",
    grepl("W_3", plotID_old) ~ "Plot_3",
    grepl("W_2", plotID_old) ~ "Plot_2",
    grepl("W_1", plotID_old) ~ "Plot_1",
    grepl("E_5", plotID_old) ~ "Plot_5",
    grepl("E_4", plotID_old) ~ "Plot_4",
    grepl("E_3", plotID_old) ~ "Plot_3",
    grepl("E_2", plotID_old) ~ "Plot_2",
    grepl("E_1", plotID_old) ~ "Plot_1"
  ), 
  Cover = as.numeric(Cover)
) %>% filter(!is.na(Cover))
  
  
names(traits)

#make traits long
traits.long <- traits %>% select(ID, siteID, aspect, species, plotID, veg_height_cm,
                                 rep_height_cm, leaf_thickness_mm, leaf_wet_mass, elevation_clean) %>% 
  rename(Species = species) %>% 
  pivot_longer(cols = c("leaf_thickness_mm", "rep_height_cm",
                                                "leaf_wet_mass",
                                                "veg_height_cm"),
                                       names_to = "traits", 
                                       values_to = "values")


#try traitstrap -------------
tt1 <- traitstrap::trait_fill(traits = traits.long, comm = cov, scale_hierarchy = c("siteID", "aspect", "plotID"), 
                       global = TRUE, taxon_col = "Species", trait_col = "traits", value_col = "values", abundance_col = "Cover")

tt2 <- tt1 %>% trait_np_bootstrap() %>% trait_summarise_boot_moments()

tt3 <- tt2 %>% pivot_longer(cols = c("mean", "var"), 
                            names_to = "moment",
                            values_to = "values")

ggplot(data = tt3) +
  geom_smooth(aes(x = siteID, y = values, color = moment, linetype = aspect)) +
  facet_wrap(~traits, scales = "free") +
  theme_bw()


## get elevation data 

ele <- traits %>% select(plotID, aspect, siteID, elevation) %>% unique()

table(traits$aspect)
table(traits$plotID)


#join in flux and elevation data 

tt4 <- tt2 %>% 
  left_join(ele, by = c("siteID", "plotID", "aspect")) %>% 
  left_join(dt, by = c("plotID", "aspect", "elevation")) %>% 
  as.data.table()

summary(tt4)

ggplot(data = tt4) +
  geom_smooth(aes(x = mean, y = NPP, color = mean, linetype = aspect)) +
  facet_wrap(~traits, scales = "free") +
  theme_bw()


