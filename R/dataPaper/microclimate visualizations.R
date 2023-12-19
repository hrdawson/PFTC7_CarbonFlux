# Use the tomst object from `microclimate_presentation`
# Someday I'll have a nice clean Tomst object to load...

# Modified (heavily) from pFTC6 density plot
microclimate <- FLIRflat |>
  bind_rows(IRtemp) |>
  # Standardise columns for tomst
  rename(value = temp_C) |>
  mutate(metric = case_when(
    dataset == "IR Temp" ~ "Leaf T (ºC)",
    dataset == "FLIR" ~ "Ground T (ºC)",
  )) |>
  # Bind in Tomst
  bind_rows(tomst) |>
  # Factor variables
  mutate(metric = factor(metric, levels = c("Aboveground T (ºC)", "Leaf T (ºC)", "Surface T (ºC)",
                                            "Ground T (ºC)", "Soil T (ºC)", "Soil moisture (%)"))) |>
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

ggsave("visualizations/2023.12.19_dataPaper_microclimate.png",
       width = 14, height = 10, units = "in")
