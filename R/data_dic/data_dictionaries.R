# More info on making data dictionaries
# https://github.com/audhalbritter/dataDocumentation
# Note that we're using a script modified by HRD, not the originals by Aud

# load functions
source("R/functions/make_data_dic.R")
source("R/functions/get_started_data_dic.R")
library(tidyverse)

# Flux tent LI7500 data dic ----
## For CO2 ----
# Start by creating a template CSV
get_started(data = read.csv("clean_data/LI7500_Cflux_originalCalc.csv"))

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_fluxes <- make_data_dictionary(data = read.csv("clean_data/LI7500_Cflux_originalCalc.csv"),
                                 description_table = read.csv("data_dic/description_table_fluxes.csv"),
                                 table_ID = "LI7500",
                                 keep_table_ID = FALSE)
write.csv(data_dic_fluxes, "data_dic/dataDic_LI7500.csv")

## For water ----
# Start by creating a template CSV
LI7500.h2o = read.csv("clean_data/LI7500_H2Oflux_originalCalc.csv")

get_started(data = read.csv("clean_data/LI7500_H2Oflux_originalCalc.csv"))

data_dic_fluxes_ET <- make_data_dictionary(data = read.csv("clean_data/LI7500_H2Oflux_originalCalc.csv"),
                                        description_table = read.csv("data_dic/description_table_LI7500h2o.csv"),
                                        table_ID = "LI7500_H2O",
                                        keep_table_ID = FALSE)
write.csv(data_dic_fluxes_ET, "data_dic/dataDic_LI7500_H2O.csv")

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_fluxes <- make_data_dictionary(data = read.csv("raw_data/licor_nee_for_HRD.csv"),
                                        description_table = read.csv("data_dic/description_table_fluxes.csv"),
                                        table_ID = "LI7500",
                                        keep_table_ID = FALSE)
write.csv(data_dic_fluxes, "data_dic/dataDic_LI7500.csv")

# Soil respiration 8100 data dic ----
# Start by creating a template CSV
get_started(data = read.csv("clean_data/LI8100_fluxes.csv"))

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_soilResp <- make_data_dictionary(data = read.csv("clean_data/LI8100_fluxes.csv"),
                                        description_table = read.csv("data_dic/description_table_LI8100.csv"),
                                        table_ID = "LI8100",
                                        keep_table_ID = FALSE)
write.csv(data_dic_soilResp, "data_dic/dataDic_LI8100.csv")

## For raw SR data ----

data_dic_soilResp <- make_data_dictionary(data = read.csv("clean_data/LI8100_combined.csv"),
                                          description_table = read.csv("data_dic/description_table_LI8100_rawReads.csv"),
                                          table_ID = "soilResp",
                                          keep_table_ID = FALSE)
write.csv(data_dic_soilResp, "data_dic/dataDic_LI8100_rawReads.csv")
# Tomst data dic ----
# Start by creating a template CSV
get_started(data = read.csv("clean_data/PFTC7_Tomst_Data.csv"))

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_tomst <- make_data_dictionary(data = read.csv("clean_data/PFTC7_Tomst_Data.csv"),
                                        description_table = read.csv("data_dic/description_table_tomst.csv"),
                                        table_ID = "tomst",
                                        keep_table_ID = FALSE)

write.csv(data_dic_tomst, "data_dic/dataDic_tomst.csv")

# Handheld FLIR data dic ----
# Start by creating a template CSV
get_started(data = FLIRflat)

# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Note that this isn't the raw FLIR data as uploaded to OSF.
#  We need to re-upload a clean dataset once we have decent internet.

# Make sure to save it as something other than its default name
data_dic_FLIR <- make_data_dictionary(data = FLIRflat,
                                       description_table = read.csv("data_dic/description_table_FLIR.csv"),
                                       table_ID = "FLIR",
                                       keep_table_ID = FALSE)

write.csv(data_dic_FLIR, "data_dic/dataDic_FLIR.csv")

