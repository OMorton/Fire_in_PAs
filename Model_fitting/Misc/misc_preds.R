
library(tidyverse)
library(mgcv)
library(marginaleffects)
options(scipen = 999)

## read in data

## Standardize all variables
load("Data/Matched_data/LAM.matched.data.RData")
# remove 6 countries -  United States, Dominican Republic, Jamaica, 
# Trinidad and Tobago, Haiti and Puerto Rico
LAM.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250)
LAM.matched.data.z <- LAM.matched.data %>% ungroup() %>% 
  mutate(x.z = (x-mean(x))/sd(x),
         y.z = (y-mean(y))/sd(y),
         travel.time.z = (travel.time-mean(travel.time))/sd(travel.time),
         elevation.z = (elevation-mean(elevation))/sd(elevation),
         slope.z = (slope-mean(slope))/sd(slope),
         pop.density.z = (pop.density-mean(pop.density))/sd(pop.density),
         precipitation.z = (precipitation-mean(precipitation))/sd(precipitation),
         precipitation.wettest.z = (precipitation.wettest-mean(precipitation.wettest))/sd(precipitation.wettest),
         precipitation.driest.z = (precipitation.driest-mean(precipitation.driest))/sd(precipitation.driest),
         temperature.z = (temperature-mean(temperature))/sd(temperature),
         temperature.hottest.z = (temperature.hottest-mean(temperature.hottest))/sd(temperature.hottest),
         fwi.95.z = (fwi.95-mean(fwi.95))/sd(fwi.95),
         forest.area.z = (forest.area-mean(forest.area))/sd(forest.area),
         ## log10
         #travel.time.logz = (travel.time-mean(travel.time))/sd(travel.time),
         #elevation.logz = (log10(elevation+25)-mean(log10(elevation+25)))/sd(log10(elevation+25)),
         slope.logz = (log10(slope+1)-mean(log10(slope+1)))/sd(log10(slope+1)),
         pop.density.logz = (log10(pop.density+1)-mean(log10(pop.density+1)))/sd(log10(pop.density+1)),
         #precipitation.logz = (precipitation-mean(precipitation))/sd(precipitation),
         #precipitation.wettest.z = (precipitation.wettest-mean(precipitation.wettest))/sd(precipitation.wettest),
         #precipitation.driest.z = (precipitation.driest-mean(precipitation.driest))/sd(precipitation.driest),
         #temperature.z = (temperature-mean(temperature))/sd(temperature),
         #temperature.hottest.z = (temperature.hottest-mean(temperature.hottest))/sd(temperature.hottest),
         #fwi.95.z = (fwi.95-mean(fwi.95))/sd(fwi.95),
         forest.area.logz = (log10(forest.area)-mean(log10(forest.area)))/sd(log10(forest.area)),
         non.burned.cells = total.cells - burned.cells,
         prop.cells = burned.cells/total.cells,
         country_dummy = 1, 
         country_trt_dummy = 1,
         treatment = as.factor(treatment),
         code = as.factor(code),
         total.cells.z = (total.cells-mean(total.cells))/sd(total.cells)) %>%
  rename("ISO3" = "code") %>% as.data.frame()

load("Data/Matched_data/AFR.matched.data.RData")
# removed 2 countries - swaziland and zimbabwe
AFR.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250)
AFR.matched.data.z <- AFR.matched.data %>% ungroup() %>% 
  mutate(x.z = (x-mean(x))/sd(x),
         y.z = (y-mean(y))/sd(y),
         travel.time.z = (travel.time-mean(travel.time))/sd(travel.time),
         elevation.z = (elevation-mean(elevation))/sd(elevation),
         slope.z = (slope-mean(slope))/sd(slope),
         pop.density.z = (pop.density-mean(pop.density))/sd(pop.density),
         precipitation.z = (precipitation-mean(precipitation))/sd(precipitation),
         precipitation.wettest.z = (precipitation.wettest-mean(precipitation.wettest))/sd(precipitation.wettest),
         precipitation.driest.z = (precipitation.driest-mean(precipitation.driest))/sd(precipitation.driest),
         temperature.z = (temperature-mean(temperature))/sd(temperature),
         temperature.hottest.z = (temperature.hottest-mean(temperature.hottest))/sd(temperature.hottest),
         fwi.95.z = (fwi.95-mean(fwi.95))/sd(fwi.95),
         forest.area.z = (forest.area-mean(forest.area))/sd(forest.area),
         ## log10
         #travel.time.logz = (travel.time-mean(travel.time))/sd(travel.time),
         #elevation.logz = (log10(elevation+25)-mean(log10(elevation+25)))/sd(log10(elevation+25)),
         slope.logz = (log10(slope+1)-mean(log10(slope+1)))/sd(log10(slope+1)),
         pop.density.logz = (log10(pop.density+1)-mean(log10(pop.density+1)))/sd(log10(pop.density+1)),
         #precipitation.logz = (precipitation-mean(precipitation))/sd(precipitation),
         #precipitation.wettest.z = (precipitation.wettest-mean(precipitation.wettest))/sd(precipitation.wettest),
         #precipitation.driest.z = (precipitation.driest-mean(precipitation.driest))/sd(precipitation.driest),
         #temperature.z = (temperature-mean(temperature))/sd(temperature),
         #temperature.hottest.z = (temperature.hottest-mean(temperature.hottest))/sd(temperature.hottest),
         #fwi.95.z = (fwi.95-mean(fwi.95))/sd(fwi.95),
         forest.area.logz = (log10(forest.area)-mean(log10(forest.area)))/sd(log10(forest.area)),
         non.burned.cells = total.cells - burned.cells,
         prop.cells = burned.cells/total.cells,
         country_dummy = 1, 
         country_trt_dummy = 1,
         treatment = as.factor(treatment),
         code = as.factor(code),
         total.cells.z = (total.cells-mean(total.cells))/sd(total.cells)) %>%
  rename("ISO3" = "code") %>% as.data.frame()

load("Data/Matched_data/SEA-AUS.matched.data.RData")
# removed 2 - fiji and singapore
SEA.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250)
SEA.matched.data.z <- matched.data %>% ungroup() %>% 
  mutate(x.z = (x-mean(x))/sd(x),
         y.z = (y-mean(y))/sd(y),
         travel.time.z = (travel.time-mean(travel.time))/sd(travel.time),
         elevation.z = (elevation-mean(elevation))/sd(elevation),
         slope.z = (slope-mean(slope))/sd(slope),
         pop.density.z = (pop.density-mean(pop.density))/sd(pop.density),
         precipitation.z = (precipitation-mean(precipitation))/sd(precipitation),
         precipitation.wettest.z = (precipitation.wettest-mean(precipitation.wettest))/sd(precipitation.wettest),
         precipitation.driest.z = (precipitation.driest-mean(precipitation.driest))/sd(precipitation.driest),
         temperature.z = (temperature-mean(temperature))/sd(temperature),
         temperature.hottest.z = (temperature.hottest-mean(temperature.hottest))/sd(temperature.hottest),
         fwi.95.z = (fwi.95-mean(fwi.95))/sd(fwi.95),
         forest.area.z = (forest.area-mean(forest.area))/sd(forest.area),
         ## log10
         travel.time.logz = (travel.time-mean(travel.time))/sd(travel.time),
         #elevation.logz = (log10(elevation+130)-mean(log10(elevation+130)))/sd(log10(elevation+130)),
         elevation.cubez = ((elevation)^1/3 -mean((elevation)^1/3))/sd((elevation)^1/3),
         slope.logz = (log10(slope+1)-mean(log10(slope+1)))/sd(log10(slope+1)),
         pop.density.logz = (log10(pop.density+1)-mean(log10(pop.density+1)))/sd(log10(pop.density+1)),
         #precipitation.logz = (precipitation-mean(precipitation))/sd(precipitation),
         #precipitation.wettest.z = (precipitation.wettest-mean(precipitation.wettest))/sd(precipitation.wettest),
         #precipitation.driest.z = (precipitation.driest-mean(precipitation.driest))/sd(precipitation.driest),
         #temperature.z = (temperature-mean(temperature))/sd(temperature),
         #temperature.hottest.z = (temperature.hottest-mean(temperature.hottest))/sd(temperature.hottest),
         #fwi.95.z = (fwi.95-mean(fwi.95))/sd(fwi.95),
         forest.area.logz = (log10(forest.area)-mean(log10(forest.area)))/sd(log10(forest.area)),
         non.burned.cells = total.cells - burned.cells,
         prop.cells = burned.cells/total.cells,
         country_dummy = 1, 
         country_trt_dummy = 1,
         treatment = as.factor(treatment),
         code = as.factor(code),
         total.cells.z = (total.cells-mean(total.cells))/sd(total.cells)) %>%
  rename("ISO3" = "code") %>% as.data.frame()


## actual summaries
LAM.raw.sum <- LAM.matched.data.z %>% group_by(ISO3, treatment) %>% summarise(mn = mean(prop.cells))

mn.dat <- data.frame(x.z = 0, y.z = 0, travel.time.logz = 0, travel.time.z = 0, elevation.z = 0,
                         slope.logz = 0, pop.density.logz = 0, precipitation.z = 0,
                         precipitation.wettest.z = 0, precipitation.driest.z = 0,
                         temperature.hottest.z = 0, fwi.95.z = 0, forest.area.z = 0,
                         ISO3 = "IDN", country_dummy = 0, 
                         country_trt_dummy = 0, treatment = c("PA", "Non-PA"))

LAM.ISO.mn.dat <- LAM.matched.data.z %>% group_by(ISO3) %>%
  summarise(x.z = mean(x.z), y.z = mean(y.z), 
            #travel.time.logz = mean(travel.time.logz),
            travel.time.z = mean(travel.time.z), 
            elevation.z = mean(elevation.z),
            slope.logz = mean(slope.logz), pop.density.logz = mean(pop.density.logz), 
            precipitation.z = mean(precipitation.z),
            precipitation.wettest.z = mean(precipitation.wettest.z), 
            precipitation.driest.z = mean(precipitation.driest.z),
            temperature.hottest.z = mean(temperature.hottest.z), 
            fwi.95.z = mean(fwi.95.z), forest.area.z = mean(forest.area.z),
            country_dummy = 1, country_trt_dummy = 1)
LAM.ISO.mn.dat2 <- rbind(mutate(LAM.ISO.mn.dat, treatment = "PA"), 
                         mutate(LAM.ISO.mn.dat, treatment = "Non-PA"))

AFR.ISO.mn.dat <- AFR.matched.data.z %>% group_by(ISO3) %>%
  summarise(x.z = mean(x.z), y.z = mean(y.z), 
            #travel.time.logz = mean(travel.time.logz),
            travel.time.z = mean(travel.time.z), 
            elevation.z = mean(elevation.z),
            slope.logz = mean(slope.logz), pop.density.logz = mean(pop.density.logz), 
            precipitation.z = mean(precipitation.z),
            precipitation.wettest.z = mean(precipitation.wettest.z), 
            precipitation.driest.z = mean(precipitation.driest.z),
            temperature.hottest.z = mean(temperature.hottest.z), 
            fwi.95.z = mean(fwi.95.z), forest.area.z = mean(forest.area.z),
            country_dummy = 1, country_trt_dummy = 1)
AFR.ISO.mn.dat2 <- rbind(mutate(AFR.ISO.mn.dat, treatment = "PA"), 
                         mutate(AFR.ISO.mn.dat, treatment = "Non-PA"))

SEA.ISO.mn.dat <- SEA.matched.data.z %>% group_by(ISO3) %>%
  summarise(x.z = mean(x.z), y.z = mean(y.z), 
            travel.time.logz = mean(travel.time.logz),
            travel.time.z = mean(travel.time.z), 
            elevation.z = mean(elevation.z),
            slope.logz = mean(slope.logz), pop.density.logz = mean(pop.density.logz), 
            precipitation.z = mean(precipitation.z),
            precipitation.wettest.z = mean(precipitation.wettest.z), 
            precipitation.driest.z = mean(precipitation.driest.z),
            temperature.hottest.z = mean(temperature.hottest.z), 
            fwi.95.z = mean(fwi.95.z), forest.area.z = mean(forest.area.z),
            country_dummy = 1, country_trt_dummy = 1)
SEA.ISO.mn.dat2 <- rbind(mutate(SEA.ISO.mn.dat, treatment = "PA"), 
                         mutate(SEA.ISO.mn.dat, treatment = "Non-PA"))

## LAM
LAM_mod_for50 <- readRDS("Outputs/Models/LAM.Mod.Forest.50.rds")

system.time(LAM.mn.trt.pred50 <- predictions(LAM_mod_for50, newdata = mn.dat, type = "link", 
                                                 conf_level = .95))

LAM.mn.trt.pred50 <- LAM.mn.trt.pred50 %>% as.data.frame() %>% 
  mutate(prob = boot::inv.logit(estimate),
         lwr.prob = boot::inv.logit(estimate - 1.96*std.error),
         upr.prob = boot::inv.logit(estimate + 1.96*std.error))

system.time(LAM.mn.ISO.trt.pred50 <- predictions(LAM_mod_for50, newdata = LAM.ISO.mn.dat2, type = "link", 
                                             conf_level = .95))

LAM.mn.ISO.trt.pred50 <- LAM.mn.ISO.trt.pred50 %>% as.data.frame() %>% 
  mutate(prob = boot::inv.logit(estimate),
         lwr.prob = boot::inv.logit(estimate - 1.96*std.error),
         upr.prob = boot::inv.logit(estimate + 1.96*std.error))

## AFR
AFR_mod_for50 <- readRDS("Outputs/Models/AFR.Mod.Forest.50.rds")

system.time(AFR.mn.trt.pred50 <- predictions(AFR_mod_for50, newdata = mn.dat, type = "link", 
                                             conf_level = .95))

AFR.mn.trt.pred50 <- AFR.mn.trt.pred50 %>% as.data.frame() %>% 
  mutate(prob = boot::inv.logit(estimate),
         lwr.prob = boot::inv.logit(estimate - 1.96*std.error),
         upr.prob = boot::inv.logit(estimate + 1.96*std.error))

system.time(AFR.mn.ISO.trt.pred50 <- predictions(AFR_mod_for50, newdata = AFR.ISO.mn.dat2, type = "link", 
                                                 conf_level = .95))

AFR.mn.ISO.trt.pred50 <- AFR.mn.ISO.trt.pred50 %>% as.data.frame() %>% 
  mutate(prob = boot::inv.logit(estimate),
         lwr.prob = boot::inv.logit(estimate - 1.96*std.error),
         upr.prob = boot::inv.logit(estimate + 1.96*std.error))

## SEA
SEA_mod_for50 <- readRDS("Outputs/Models/SEA.Mod.Forest.50.rds")

system.time(SEA.mn.trt.pred50 <- predictions(SEA_mod_for50, newdata = mn.dat, type = "link", 
                                             conf_level = .95))

SEA.mn.trt.pred50 <- SEA.mn.trt.pred50 %>% as.data.frame() %>% 
  mutate(prob = boot::inv.logit(estimate),
         lwr.prob = boot::inv.logit(estimate - 1.96*std.error),
         upr.prob = boot::inv.logit(estimate + 1.96*std.error))

system.time(SEA.mn.ISO.trt.pred50 <- predictions(SEA_mod_for50, newdata = SEA.ISO.mn.dat2, type = "link", 
                                                 conf_level = .95))

SEA.mn.ISO.trt.pred50 <- SEA.mn.ISO.trt.pred50 %>% as.data.frame() %>% 
  mutate(prob = boot::inv.logit(estimate),
         lwr.prob = boot::inv.logit(estimate - 1.96*std.error),
         upr.prob = boot::inv.logit(estimate + 1.96*std.error))

## plot 
LAM.ave.plt <- ggplot(LAM.mn.trt.pred50, aes(prob, treatment, colour = treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr.prob, xmax = upr.prob), height = 0) + 
  scale_color_manual(values = c("tomato", "dodgerblue")) +
  coord_cartesian(xlim = c(0, 0.01)) +
  xlab("Burn probability") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")

AFR.ave.plt <- ggplot(AFR.mn.trt.pred50, aes(prob, treatment, colour = treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr.prob, xmax = upr.prob), height = 0) + 
  scale_color_manual(values = c("tomato", "dodgerblue")) +
  coord_cartesian(xlim = c(0, 1)) +
  xlab("Burn probability") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")

SEA.ave.plt <- ggplot(SEA.mn.trt.pred50, aes(prob, treatment, colour = treatment)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr.prob, xmax = upr.prob), height = 0) + 
  scale_color_manual(values = c("tomato", "dodgerblue")) +
  coord_cartesian(xlim = c(0, 0.01)) +
  xlab("Burn probability") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")

LAM.iso.plt <- ggplot(LAM.mn.ISO.trt.pred50, aes(prob, reorder(ISO3, prob), colour = treatment)) +
  geom_rect(aes(ymin = 1.5, ymax = 2.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 3.5, ymax = 4.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 5.5, ymax = 6.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 7.5, ymax = 8.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 9.5, ymax = 10.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 11.5, ymax = 12.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 13.5, ymax = 14.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 15.5, ymax = 16.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 17.5, ymax = 18.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 19.5, ymax = 20.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 21.5, ymax = 22.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
  geom_point(position = position_dodge(.75)) +
  geom_errorbarh(aes(xmin = lwr.prob, xmax = upr.prob), height = 0,
                 position = position_dodge(.75)) + 
  scale_color_manual(values = c("tomato", "dodgerblue")) +
  xlab("Burn probability") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")

AFR.iso.plt <- ggplot(AFR.mn.ISO.trt.pred50, aes(prob, reorder(ISO3, prob), colour = treatment)) +
   geom_rect(aes(ymin = 1.5, ymax = 2.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 3.5, ymax = 4.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 5.5, ymax = 6.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 7.5, ymax = 8.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 9.5, ymax = 10.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
  geom_point(position = position_dodge(.75)) +
  geom_errorbarh(aes(xmin = lwr.prob, xmax = upr.prob), height = 0,
                 position = position_dodge(.75)) + 
  scale_color_manual(values = c("tomato", "dodgerblue")) +
  xlab("Burn probability") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")
  

SEA.iso.plt <- ggplot(SEA.mn.ISO.trt.pred50, aes(prob, reorder(ISO3, prob), colour = treatment)) +
   geom_rect(aes(ymin = 1.5, ymax = 2.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 3.5, ymax = 4.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 5.5, ymax = 6.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 7.5, ymax = 8.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 9.5, ymax = 10.5, xmin = -Inf, xmax = Inf),
             fill = "grey90", colour = NA) +
   geom_rect(aes(ymin = 11.5, ymax = 12.5, xmin = -Inf, xmax = Inf),
           fill = "grey90", colour = NA) +
  geom_rect(aes(ymin = 13.5, ymax = 14.5, xmin = -Inf, xmax = Inf),
            fill = "grey90", colour = NA) +
geom_point(position = position_dodge(.75)) +
  geom_errorbarh(aes(xmin = lwr.prob, xmax = upr.prob), height = 0,
                 position = position_dodge(.75)) + 
  scale_color_manual(values = c("tomato", "dodgerblue")) +
  xlab("Burn probability") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), legend.position = "none")

AFR.matched.data.z %>% group_by(ISO3) %>% tally()
LAM.matched.data.z %>% group_by(ISO3) %>% tally()
SEA.matched.data.z %>% group_by(ISO3) %>% tally()

library(ggpubr)

pred.plt1 <- ggarrange(LAM.ave.plt, AFR.ave.plt, SEA.ave.plt,
          LAM.iso.plt, AFR.iso.plt, SEA.iso.plt, 
          heights = c(1, 2),
          labels = c("a", "b", "c", "d", "e", "f"),
          align = "hv")

ggsave(path = "Outputs/Figures", filename = "Model.ave.vals.plt.png",
       pred.plt1, bg = "white",
       device = "png", width = 24, height = 24, units = "cm")



#### temporal explore


samp <- AFR.matched.data.z %>% slice_sample(n = 1000)
samp %>% group_by(treatment) %>% summarise(mn = mean(prop.cells))

AFR_full <- read_rds("Outputs/Models/AFR.Mod.Forest.200.new.rds")

system.time(full.preds <- avg_predictions(AFR_full, newdata = samp, 
                                          by = "treatment", conf_level = .95))
system.time(disc.preds <- avg_predictions(AFR.mod.for200.new, newdata = samp, 
                                          by = "treatment", conf_level = .95))

LAM_temp <- read_rds("Outputs/Models/LAM.Mod.Temporal.50.rds")

summary(LAM_temp)

system.time(pred.temp <- predictions(LAM_temp, newdata = "mean", 
                                     by = c("treatment", "year.z"), conf_level = .95))

ggplot(pred.temp, aes(year.z, estimate, colour = treatment)) +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = NA) +
  theme_minimal()

ggplot(LAM.raw.sum, aes(year, mn, colour = treatment)) +
  geom_line()

system.time(pred.slopes <- slopes(LAM_temp, newdata = "mean", variables = "year.z",
                                  by = c("treatment", "year.z"), conf_level = .95))

ggplot(pred.slopes, aes(year.z, estimate, colour = treatment)) +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = NA) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_minimal()

### speed test
samp <- LAM.temporal.z %>% slice_sample(n = 2000)

system.time(pred.temp <- predictions(LAM_temp, newdata = samp, 
                                     by = c("treatment", "year.z"), conf_level = .95))

system.time(pred.temp.av <- avg_predictions(LAM_temp, newdata = samp, 
                                            by = c("treatment", "year.z"), conf_level = .95))

system.time(pred.temp.ls <- predictions(LAM_temp, newdata = samp, 
                                        variables = list(treatment = c("PA", "Non-PA")), 
                                        by = c("treatment", "year.z"), conf_level = .95))


library(future.apply)
plan("multicore", workers = 4)
options(marginaleffects_parallel = TRUE)
system.time(  pred.temp.m <- predictions(LAM_temp, newdata = samp, 
                                         by = c("treatment", "year.z"), conf_level = .95))


LAM.nat.aves <- LAM.matched.data.z %>% group_by(ISO3) %>% 
  summarise(x.z = mean(x.z), y.z = mean(y.z), travel.time.z = mean(travel.time.z),
            elevation.z = mean(elevation.z), slope.logz = mean(slope.logz),
            slope.logz = mean(slope.logz), pop.density.logz = mean(pop.density.logz),
            precipitation.z = mean(precipitation.z), 
            precipitation.wettest.z = mean(precipitation.wettest.z),
            precipitation.driest.z = mean(precipitation.driest.z),
            temperature.hottest.z = mean(temperature.hottest.z),
            fwi.95.z = mean(fwi.95.z), forest.area.z = mean(forest.area.z),
            country_dummy = 1, country_trt_dummy = 1,
            treatment = "PA")
AFR.nat.aves <- AFR.matched.data.z %>% group_by(ISO3) %>% 
  summarise(x.z = mean(x.z), y.z = mean(y.z), travel.time.z = mean(travel.time.z),
            elevation.z = mean(elevation.z), slope.logz = mean(slope.logz),
            slope.logz = mean(slope.logz), pop.density.logz = mean(pop.density.logz),
            precipitation.z = mean(precipitation.z), 
            precipitation.wettest.z = mean(precipitation.wettest.z),
            precipitation.driest.z = mean(precipitation.driest.z),
            temperature.hottest.z = mean(temperature.hottest.z),
            fwi.95.z = mean(fwi.95.z), forest.area.z = mean(forest.area.z),
            country_dummy = 1, country_trt_dummy = 1,
            treatment = "PA")


LAM.reg.pred <- predictions(LAM.Mod.Forest.200, newdata = LAM.nat.aves, 
                            variables = list(treatment = c("PA", "Non-PA")),
                            conf_level = .95, by = "treatment")

LAM.reg.pred.raw <- predictions(LAM.Mod.Forest.200, newdata = LAM.nat.aves, 
                                variables = list(treatment = c("PA", "Non-PA")),
                                conf_level = .95)

LAM.test <- LAM.reg.pred.raw %>% group_by(treatment) %>% summarise(mn = mean(estimate), se = sd(estimate)/sqrt(length(estimate)))

LAM.reg.pred.av <- avg_predictions(LAM.Mod.Forest.200, newdata = LAM.nat.aves, 
                                   variables = list(treatment = c("PA", "Non-PA")),
                                   conf_level = .95, by = "treatment")

AFR.reg.pred <- predictions(AFR.Mod.Forest.200, newdata = AFR.nat.aves, 
                            variables = list(treatment = c("PA", "Non-PA")),
                            conf_level = .95, by = "treatment")

## National predictions
LAM.nat.pred <- predictions(LAM.Mod.Forest.200, newdata = LAM.nat.aves, 
                            variables = list(treatment = c("PA", "Non-PA")),
                            conf_level = .95, by = c("treatment", "ISO3"))

AFR.nat.pred <- predictions(AFR.Mod.Forest.200, newdata = AFR.nat.aves, 
                            variables = list(treatment = c("PA", "Non-PA")),
                            conf_level = .95, by = c("treatment", "ISO3"))

ggplot(LAM.nat.pred, aes(ISO3, estimate, colour = treatment)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0)

comp <- avg_comparisons(LAM.mod.for200.new, newdata = samp, conf_level = .95, 
                        variables = "treatment")
comp.ratio <- avg_comparisons(LAM.mod.for200.new, newdata = samp, conf_level = .95, 
                              variables = "treatment", comparison = "ratio")
comp.iso <- avg_comparisons(LAM.mod.for200.new, newdata = samp, conf_level = .95, 
                            variables = "treatment", by = c("ISO3", "treatment"))

pred <- avg_predictions(LAM.mod.for200.new, newdata = samp, 
                        by = "treatment", conf_level = .95)
## SEA

samp <- SEA.matched.data.z %>% slice_sample(n = 1000)
samp %>% group_by(treatment) %>% summarise(mn = mean(prop.cells))

summary(SEA_mod_for200)
summary(SEA_mod_for100)
summary(SEA_mod_for50)


SEA.mn.dat <- data.frame(x.z = SEA.matched.data.z$x.z, y.z = SEA.matched.data.z$y.z,
                         travel.time.logz = 0, elevation.z = 0,
                         slope.logz = 0, pop.density.logz = 0, precipitation.z = 0,
                         precipitation.wettest.z = 0, precipitation.driest.z = 0,
                         temperature.hottest.z = 0, fwi.95.z = 0, forest.area.z = 0,
                         ISO3 = SEA.matched.data.z$ISO3, country_dummy = 1, 
                         country_trt_dummy = 1, treatment = SEA.matched.data.z$treatment)

SEA_mod_for50 <- read_rds("Outputs/Models/SEA.Mod.Forest.50.rds")

## 50
system.time(SEA.mn.trt.pred50 <- avg_predictions(SEA_mod_for50, newdata = SEA.mn.dat, 
                                                 by = "treatment", conf_level = .95))

# 13000s
# treatment Estimate Std. Error   z Pr(>|z|)   S   2.5 %  97.5 %
#   Non-PA  0.00141  0.0000140 101   <0.001 Inf 0.00139 0.00144
#   PA      0.00116  0.0000116 100   <0.001 Inf 0.00114 0.00118

# system.time(SEA.mn.trt.pred50_sc <- avg_predictions(SEA_mod_for50, newdata = SEA.mn.dat, vcov = ~subclass,
#                                                  by = "treatment", conf_level = .95))

system.time(SEA.mn.trt.pred50_hc <- avg_predictions(SEA_mod_for50, newdata = SEA.mn.dat, vcov = "HC3",
                                                    by = "treatment", conf_level = .95))

gKRLS::estfun(SEA_mod_for50)

system.time(SEA.all.trt.pred50 <- avg_predictions(SEA_mod_for50, newdata = SEA.matched.data.z, 
                                                  by = "treatment", conf_level = .95))

system.time(SEA.all.trt.pred501 <- avg_predictions(SEA_mod_for50, newdata = samp, vcov = ~subclass,
                                                   by = "treatment", conf_level = .95))

system.time(SEA.all.trt.pred502 <- avg_predictions(SEA_mod_for50, newdata = samp, vcov = "HC3",
                                                   by = "treatment", conf_level = .95))

system.time(SEA.all.ISO.trt.pred50 <- avg_predictions(SEA_mod_for50, newdata = SEA.matched.data.z, 
                                                      by = c("ISO3", "treatment"), conf_level = .95))

system.time(SEA.all.comp50 <- avg_comparisons(SEA_mod_for50, 
                                              variables = list(treatment = c("Non-PA", "PA"))))


## 100
system.time(SEA.mn.trt.pred100 <- avg_predictions(SEA_mod_for100, newdata = SEA.mn.dat, 
                                                  by = "treatment", conf_level = .95))

system.time(SEA.all.trt.pred100 <- avg_predictions(SEA_mod_for100, newdata = SEA.matched.data.z, 
                                                   by = "treatment", conf_level = .95))

system.time(SEA.all.ISO.trt.pred100 <- avg_predictions(SEA_mod_for100, newdata = SEA.matched.data.z, 
                                                       by = c("ISO3", "treatment"), conf_level = .95))




SEA.trt.pred <- avg_predictions(SEA_mod_for100, by = "treatment", conf_level = .95)
SEA.ISO.trt.pred <- avg_predictions(SEA_mod_for100, 
                                    by = c("ISO3", "treatment"), conf_level = .95)