# Ridgeline plot
library(tidyverse)
library(ggridges)
library(tidylog)

# Read in Tomst data
tomst = read.csv("clean_data/PFTC7_Tomst_Data.csv")

# Make trial plot with basic Tomst data
