library(data.table)
library(tidyverse)
library(gridExtra)
library(MetBrewer)

dt <- fread("outputs/prelim_flux_results.csv")
dt <- dt %>% rename(SR = co2_flux_sr)
#what different temperature sources do we have?
## chamber temperature


ggplot(data = dt[, ], aes(x = as.factor(elevation), y = temperature)) +
  geom_boxplot() +
  facet_wrap(~aspect, scales = "free_x") +
  theme_bw()

dt.long.c <- dt %>% pivot_longer(cols = c("NPP", "NEE",
                                        "GPP",
                                        "ER", "SR"),
                               names_to = "Flux", 
                               values_to = "values")

c.long <- ggplot(data = dt.long.c[, ]) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
  geom_point(aes(x = temperature, y = values, color = aspect, fill = aspect)) +
  geom_smooth(aes(x = temperature, y = values, color = aspect, fill = aspect), method = "gam") +
  facet_wrap(~Flux, scales = "free_y") +
  guides(color=guide_legend(override.aes=list(fill=NA)), fill = FALSE) +
  labs(x = "Temperature", y = "C (ppm/s/m2)", color = "") +
  scale_color_met_d(name = "Isfahan1") +
  scale_fill_met_d(name = "Isfahan1") +
  theme_bw()

c.long

ggsave(plot = c.long, "outputs/plots/sensor_temp_carbon2.png", dpi = 600, height = 6, width = 10)

c.p <- ggplot(data = dt[, ]) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
  geom_smooth(aes(x = temperature, y = NEE, color = "NEE", fill = "NEE"), method = "gam") +
  geom_smooth(aes(x = temperature, y = GPP, color = "GPP", fill = "GPP"), method = "gam") +
  geom_smooth(aes(x = temperature, y = NPP, color = "NPP", fill = "NPP"), method = "gam") +
  facet_wrap(~aspect, scales = "free_x") +
  guides(color=guide_legend(override.aes=list(fill=NA)), fill = FALSE) +
  labs(x = "Temperature", y = "C (ppm/s/m2)", color = "") +
  scale_color_met_d(name = "Isfahan1") +
  scale_fill_met_d(name = "Isfahan1") +
  theme_bw()

c.p

c.r <- ggplot(data = dt[!elevation == 2000, ]) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
  geom_smooth(aes(x = temperature, y = ER, color = "ER", fill = "ER"), method = "gam") +
  geom_smooth(aes(x = temperature, y = co2_flux_sr, color = "SR", fill = "SR"), method = "gam") +
  facet_wrap(~aspect, scales = "free_x") +
  scale_color_met_d(name = "Pillement") +
  scale_fill_met_d(name = "Pillement") +
  labs(x = "Temperature", y = "C (ppm/s/m2)", color = "") +
  guides(color=guide_legend(override.aes=list(fill=NA)), fill = FALSE) +
  theme_bw()
c.r

carb <- grid.arrange(c.p, c.r, ncol = 1)

ggsave(plot = carb, "outputs/plots/sensor_temp_carbon.png", dpi = 600, width = 10 )


w.p <- ggplot(data = dt[!elevation == 2000, ]) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
  geom_smooth(aes(x = temperature, y = TRANS, color = "TRANS", fill = "TRANS"), method = "gam") +
  geom_smooth(aes(x = temperature, y = ET, color = "ET", fill = "ET"), method = "gam") +
 # geom_point(aes(x = temperature, y = h2o_flux_sr, color = "EVAP", fill = "EVAP"), method = "gam") +
  facet_wrap(~aspect, scales = "free_x") +
  guides(color=guide_legend(override.aes=list(fill=NA)), fill = FALSE) +
  labs(x = "Temperature", y = "H2O (ppm/s/m2)", color = "") +
  scale_color_met_d(name = "Isfahan1") +
  scale_fill_met_d(name = "Isfahan1") +
  theme_bw()

w.p


dt.long.w <- dt %>% pivot_longer(cols = c("TRANS", "ET"),
                                 names_to = "Flux", 
                                 values_to = "values")

w.long <- ggplot(data = dt.long.w[, ]) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
  geom_point(aes(x = temperature, y = values, color = aspect, fill = aspect)) +
  geom_smooth(aes(x = temperature, y = values, color = aspect, fill = aspect), method = "gam") +
  facet_wrap(~Flux, scales = "free_x") +
  guides(color=guide_legend(override.aes=list(fill=NA)), fill = FALSE) +
  labs(x = "Temperature", y = "C (ppm/s/m2)", color = "") +
  scale_color_met_d(name = "Isfahan1") +
  scale_fill_met_d(name = "Isfahan1") +
  theme_bw()

w.long
