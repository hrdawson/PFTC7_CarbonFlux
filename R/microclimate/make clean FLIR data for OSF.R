# Make clean FLIR data file for OSF

# Flatten Rds to data frame ----
FLIRraw = get(load("raw_data/flir_raw_2023-12-17.Rdata"))

FLIRrawdata = FLIRraw[["raw_dat"]]

FLIRraw = readRDS("raw_data/microclimate/FLIR_cropped.Rds") |>
  lapply(FUN = as.numeric)

# Code by Brian Maitner
flir_long <- NULL
for(i in 1:length(FLIRraw)){
  if(length(as.numeric(FLIRraw[[i]]))==0){next}
  data.frame(file_number=names(FLIRraw)[i],
             value = as.numeric(FLIRraw[[i]]))%>%
    bind_rows(flir_long) -> flir_long
}

# Add in meta data
FLIRmeta = read.csv("raw_data/microclimate/PFTC7_SA_FLIR_2023.csv")
FLIRmeta = getFLIRnames(data = FLIRmeta) |>
  mutate(file_number = as.character(file_number))

FLIRflat = flir_long |>
  left_join(FLIRmeta) |>
  # Filter out obs without file number
  filter(file_number != "") |>
  # Rename temp col
  rename(temp_C = value)

write.csv(FLIRflat, "clean_data/flir_values.csv")
