library(co2fluxtent)
library(tidyverse)

#test
source("R/functions/flux_calc_own.R")
file.exists("R/functions/flux_calc_own.R")


# Look for flux files in a folder
licor_files <- Map(c, co2fluxtent::read_files("raw_data/Site 1"), 
                   co2fluxtent::read_files("raw_data/Site 2"),
                   co2fluxtent::read_files("raw_data/Site 3"),
                   co2fluxtent::read_files("raw_data/Site 4"),
                   co2fluxtent::read_files("raw_data/Site 5"))

# Check if the files are ok
licor_files <- test_flux_files(licor_files, skip = 3, min_rows = 50)

print(licor_files)

meta <- tibble(file_path = unlist(licor_files),
               file = basename(file_path)) %>% 
  mutate(site = unlist(lapply(file, function(x) str_split(x, "_")[[1]][1])),
         elevation = unlist(lapply(file, function(x) str_split(x, "_")[[1]][2])),
         aspect = unlist(lapply(file, function(x) str_split(x, "_")[[1]][3])),
         plot = unlist(lapply(file, function(x) str_split(x, "_")[[1]][4])),
         day_night = unlist(lapply(file, function(x) str_split(x, "_")[[1]][5])),
         measurement = unlist(lapply(file, function(x) gsub(".txt","",tail(str_split(x, "_")[[1]],1)))),
         redo = grepl("redo", file, ignore.case = T))

meta

licor_nee <- licor_files %>% 
  flux_calc_own(param = "nee", 
                skip = 3,
                vol = 1.2^3,
                area = 1.2^2, 
                tstart = 20, 
                tfinish = 80,
                signal_threshold = 95) %>%  
  mutate(filename = basename(filename)) 


dt.nee <- licor_nee |>
  rename(file = filename) |>
  left_join(meta)

day_resp <- dt.nee |>
  filter(day_night == "day" & measurement == "resp") |> 
  select(c("tav", "nee_lm", "site", "elevation", "aspect", "plot", "day_night")) |>
  rename(t2 = tav, 
         r2 = nee_lm)
night_resp <- dt.nee |>
  filter(day_night == "night" & measurement == "resp") |>
  select(c("tav", "nee_lm", "site", "elevation", "aspect", "plot", "day_night")) |>
  rename(t1 = tav, 
         r1 = nee_lm)
resp_long <- dt.nee |> 
  filter(measurement == "resp") |>
  mutate(r = nee_lm, 
         t = tav)

resp <- day_resp |>
  left_join(night_resp, by = c("site", "elevation", "aspect", "plot")) |>
  na.omit() |>
  mutate(Q10 = (r2/r1)^(10/(t2-t1)),
         delta_t = t2-t1) |> 
  na.omit()

resp$Ebar <- (log(((resp$t1+273)+(resp$t2+273))/2)-log(273))*(8.314*(273^2))/resp$t1

ebar.lm <- ggplot() +
  geom_boxplot(data = resp, aes(x = aspect, y = log(Ebar), fill = aspect), alpha = 0.8, outlier.shape = NA) +
  geom_jitter(data = resp, aes(x = aspect, y = log(Ebar), color = aspect), alpha = 0.3, size = 4) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  #geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "log(Ebar)", x = "Aspect", y = "log(Ebar)") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
ebar.lm

p.q10.lm <- ggplot() +
  geom_boxplot(data = resp, aes(x = aspect, y = Q10, fill = aspect), alpha = 0.8, outlier.shape = NA) +
  geom_jitter(data = resp, aes(x = aspect, y = Q10, color = aspect), alpha = 0.3, size = 4) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Q10", x = "Aspect", y = "Q10") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
p.q10.lm


p.resp.night <- ggplot() +
  geom_boxplot(data = resp, aes(x = aspect, y = -r1, fill = aspect), alpha = 0.8, outlier.shape = NA) +
  geom_jitter(data = resp, aes(x = aspect, y = -r1, color = aspect), alpha = 0.3, size = 4) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "night_resp", x = "Aspect", y = "night_resp") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
p.resp.night

p.resp.day <- ggplot() +
  geom_boxplot(data = resp, aes(x = aspect, y = -r2, fill = aspect), alpha = 0.8, outlier.shape = NA) +
  geom_jitter(data = resp, aes(x = aspect, y = -r2, color = aspect), alpha = 0.3, size = 4) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "day_resp", x = "Aspect", y = "day_resp") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
p.resp.day

p.dt.lm <- ggplot() +
  geom_boxplot(data = resp, aes(x = aspect, y = delta_t, fill = aspect), alpha = 0.8, outlier.shape = NA) +
  geom_jitter(data = resp, aes(x = aspect, y = delta_t, color = aspect), alpha = 0.3, size = 4) +
  facet_wrap(~ elevation) +
  scale_color_manual(values = c("darkolivegreen4", "khaki2")) + 
  scale_fill_manual(values = c("darkolivegreen4", "khaki2")) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "delta_t", x = "Aspect", y = "delta_t") +
  theme_bw() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
p.dt.lm

resp_long <- resp_long |>
  mutate(kT = 1/(8.314*(t+273)), 
         log_resp = log(-1*r)) |>
  na.omit()
resp_long |>
  ggplot(aes(x = kT, y = log_resp, col = elevation)) + 
  geom_point() + 
  geom_smooth(method = lm, se=T) +
  stat_poly_eq()+
  xlab("1/kT")+
  theme_classic()

library(ggpmisc)
ggplot()+geom_point(data = resp, aes(x = 1/(8.314*(t1+273)), y = t1+273, col = elevation)) + 
  geom_smooth(method = "lm") +
  theme_classic()

# get nice package 
#install.packages("data.table")
library(data.table)


