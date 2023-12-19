# Visualize fluxes
# Note that these are messy data that haven't been cleaned yet

# Read in flux datasets
LI7500 = read.csv("outputs/2023.12.19_Cflux_originalCalc.csv") |>
  mutate(dataset = "Ecosystem fluxes") |>
  pivot_longer(cols = c(GPP, NEE, ER), names_to = "metric", values_to = "value") |>
  mutate(plotID = as.character(plot)) |>
  select(plotID, elevation, aspect, metric, value, dataset)

LI8100 = dt.sr |>
  mutate(dataset = "Soil respiration") |>
  rename(CO2 = co2_flux_sr, H2O = h2o_flux_sr) |>
  pivot_longer(cols = c(CO2, H2O), names_to = "metric", values_to = "value") |>
  select(plotID, elevation, aspect, metric, value, dataset)

LI7500H2O = read.csv("outputs/2023.12.19_H2Oflux_originalCalc.csv") |>
  mutate(dataset = "Water fluxes") |>
  pivot_longer(cols = c(TRANS, ET, EVAP), names_to = "metric", values_to = "value") |>
  mutate(plotID = as.character(plot)) |>
  select(plotID, elevation, aspect, metric, value, dataset)

# Join datasets
flux.all = LI7500 |>
  bind_rows(LI8100, LI7500H2O) |>
  # Factor relevant metrics
  mutate(aspect = factor(aspect, levels = c("east", "west", "none")),
         elevation = factor(elevation,
                            levels = c("3000", "2800", "2600", "2400", "2200", "2000")))

# Visualize

library(ggh4x)

ggplot(flux.all, aes(x=value, fill=aspect)) +
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
    x= "Flux value"
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

ggsave("visualizations/2023.12.19_dataPaper_flux.png",
       width = 14, height = 10, units = "in")

