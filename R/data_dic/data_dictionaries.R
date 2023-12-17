# More info on making data dictionaries
# https://github.com/audhalbritter/dataDocumentation
# Note that we're using a script modified by HRD, not the originals by Aud

# load functions
source("R/functions/make_data_dic.R")
source("R/functions/get_started_data_dic.R")
library(tidyverse)

# Flux tent LI7500 data dic ----
# Start by creating a template CSV
get_started(data = read.csv("raw_data/licor_nee_for_HRD.csv"))

data = read.csv("raw_data/licor_nee_for_HRD.csv")
table_ID = "LI7500"
description_table = read.csv("data_dic/description_table_fluxes.csv")


# Open the template CSV and fill in manually
# Save it in the data_dic folder and use the file name for the description_table argument

# Make sure to save it as something other than its default name
data_dic_fluxes <- make_data_dictionary(data = read.csv("raw_data/licor_nee_for_HRD.csv"),
                                 description_table = read.csv("data_dic/description_table_fluxes.csv"),
                                 table_ID = "LI7500",
                                 keep_table_ID = FALSE)
