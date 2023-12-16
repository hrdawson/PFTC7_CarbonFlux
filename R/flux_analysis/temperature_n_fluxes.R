library(data.table)
library(tidyverse)
library(gridExtra)
library(MetBrewer)

dt <- fread("outputs/prelim_flux_results.csv")

ggplot(data = dt[!elevation == 2000, ], aes(x = as.factor(elevation), y = temperature)) +
  geom_boxplot() +
  facet_wrap(~aspect, scales = "free_x") +
  theme_bw()

c.p <- ggplot(data = dt[!elevation == 2000, ]) +
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



