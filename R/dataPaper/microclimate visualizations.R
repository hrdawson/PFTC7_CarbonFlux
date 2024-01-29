# Use the tomst object from `microclimate_presentation`
# Someday I'll have a nice clean Tomst object to load...

# Modified (heavily) from pFTC6 density plot
## IRtemp is in `microclimate_presentation.R`

test = read.csv("clean_data/flir_values.csv")
str(test)

microclimate <- read.csv("clean_data/flir_values.csv") |>
  add_column(dataset = "FLIR") |>
  # Make site 6 aspect none
  mutate(aspect = case_when(
    siteID == 6 ~ "none",
    TRUE ~ aspect
  )) |>
  # Filter out negative values
  filter(temp_C > 0) |>
  bind_rows(IRtemp) |>
  pivot_longer(temp_C, names_to = "metric", values_to = "value") |>
  # Standardise columns for tomst
  mutate(metric = case_when(
    dataset == "IR Temp" ~ "Leaf T (ºC)",
    dataset == "FLIR" ~ "Ground T (ºC)",
  )) |>
  # Bind in Tomst
  bind_rows(read.csv("clean_data/PFTC7_Tomst_Data.csv") |> 
              pivot_longer(cols = temp_soil_C:moist_vol, names_to = "metric", values_to = "value") |>
              mutate(dataset = "Tomst")|>
              rename(siteID = site))  |>
  bind_rows(read.csv("clean_data/LI7500_temperature.csv") |> 
              pivot_longer('Temperature..C.', names_to = "metric", values_to = "value") |>
              mutate(dataset = "LI-7500")) |>
  # Factor variables
  mutate(metric = case_when(
    metric == "moist_vol" ~ "Soil moisture (%)",
    metric %in% c("temp_air_C", "Temperature..C.") ~ "Surface T (ºC)",
    metric == "temp_ground_C" ~ "Ground T (ºC)",
    metric == "temp_soil_C" ~ "Soil T (ºC)",
    TRUE ~ metric,
  ),
    metric = factor(metric, levels = c("Aboveground T (ºC)", "Leaf T (ºC)", "Surface T (ºC)",
                                            "Ground T (ºC)", "Soil T (ºC)", "Soil moisture (%)"))) |>
  # Drop unneeded Tomst variable
  drop_na(metric) |>
  # Select relevant rows
  select(day.night, siteID, aspect, plotID, dataset, metric, value) |>
  mutate(day.night = str_to_title(day.night)) |>
  mutate(aspect = factor(aspect, levels = c("east", "west", "none")),
         elevation = factor(case_when(
           siteID == 6 ~ 3000,
           siteID == 5 ~ 2800,
           siteID == 4 ~ 2600,
           siteID == 3 ~ 2400,
           siteID == 2 ~ 2200,
           siteID == 1 ~ 2000
         ), levels = c("3000", "2800", "2600", "2400", "2200", "2000")))

# Visualize

library(ggh4x)

ggplot(microclimate, aes(x=value, fill=aspect)) +
  geom_density(alpha=0.6, linewidth = 0.6) +
  scale_fill_viridis(discrete=T) +
  scale_y_continuous(position = "left") +
  facet_nested(elevation ~ dataset + metric, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  # facet_grid(elevation ~ metric, scales="free",
             # ncol = 1 # for facet_wrap
             # ) +
  labs(
    y="Density",
    x= "Microclimate value"
  ) +
  theme_bw() +
  theme(
    # legend.position="none",
        # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        text=element_text(size=11)
  )

ggsave("visualizations/2023.01.29_dataPaper_microclimate.png",
       width = 14, height = 10, units = "in")


test = read.csv("clean_data/PFTC7_Tomst_Data.csv") |> 
  pivot_longer(cols = temp_soil_C:moist_vol, names_to = "metric", values_to = "value") |>
  mutate(dataset = "Tomst") |>
  mutate(aspect = factor(aspect, levels = c("east", "west", "none")),
         elevation = factor(case_when(
           siteID == 6 ~ 3000,
           siteID == 5 ~ 2800,
           siteID == 4 ~ 2600,
           siteID == 3 ~ 2400,
           siteID == 2 ~ 2200,
           siteID == 1 ~ 2000
         ), levels = c("3000", "2800", "2600", "2400", "2200", "2000")))

str(test)
