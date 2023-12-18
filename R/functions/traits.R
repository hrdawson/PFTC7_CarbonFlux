#Load libraries

library(dplyr)
library(knitr)
library(kableExtra)
library(vegan)
library(ggplot2)


#Open the traits files
try_traits <- readRDS("raw_data_2/try_traits.rds")
bien_traits <- readRDS("raw_data_2/bien_traits.rds")

#Rename columns
names(bien_traits)[1] <- "species_name"
names(try_traits)[1] <- "species_name"
names(try_traits)[2] <- "trait_name"
names(try_traits)[3] <- "trait_value"

#Read the CSV file
veg_cover_data <- read.csv("raw_data_2/veg_cover.csv")

# Convert 'Cover' column to numeric
veg_cover_data$Cover <- as.numeric(veg_cover_data$Cover)

#I want a summary of the traits
table_summary_bientraits <- table(bien_traits$trait_name)
print(table_summary_bientraits)

table_summary_trytraits <- table(try_traits$TraitName)
print(table_summary_trytraits)

#Ok, now we merge both df

merged_traits <- bind_rows(try_traits, bien_traits)

#Let´s check everything is ok
table_summary <- table(merged_traits$trait_name)
print(table_summary)
# Print the HTML table
kable(table_summary, "html") %>%
  kable_styling()

#Now I want to filter certain traits
values_to_filter <- c("Fine root length per fine root dry mass (specific fine root length, SRL)", "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included", "Leaf carbon (C) content per leaf dry mass", "Leaf carbon (C) isotope signature (delta 13C)", "Leaf carbon/nitrogen (C/N) ratio", "Leaf length", "Leaf nitrogen (N) content per leaf dry mass", "Leaf phosphorus (P) content per leaf dry mass", "Leaf photosynthesis rate per leaf area","Leaf thickness", "Plant height vegetative", "Stomata conductance per leaf area")

filtered_traits <- merged_traits %>%
  filter(trait_name %in% values_to_filter)

#Let´s check everything is ok
table_summary <- table(filtered_traits$trait_name)
print(table_summary)
# Print the HTML table
kable(table_summary, "html") %>%
  kable_styling()

#Save the dataframe
write.csv(filtered_traits, "filtered_traits.csv", row.names = FALSE)

#Extract negative results
filtered_traits <- filtered_traits[filtered_traits$trait_value >= 0, ]
filtered_traits <- filtered_traits[filtered_traits$StdValue >= 0, ]
#Put them as numeric
filtered_traits$trait_value <- as.numeric(filtered_traits$trait_value)
filtered_traits$StdValue <- as.numeric(filtered_traits$StdValue)

filtered_traits <- filtered_traits[complete.cases(filtered_traits[, c("trait_value", "StdValue")]), ]




