
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



## Check
hist(log10(AFR.matched.data.z$travel.time.z))
hist(log10(SEA.matched.data.z$travel.time))
hist((SEA.matched.data.z$elevation)^1/3) #y cube?
hist(log10(SEA.matched.data.z$slope)) #y
hist(log10(SEA.matched.data.z$pop.density)) #y
hist(log10(SEA.matched.data.z$temperature.hottest))
hist(log10((SEA.matched.data.z$forest.area))) #y

## Raw visualisation plots -----------------------------------------------------
LAM.matched.data.z %>% group_by(treatment) %>% 
  summarise(mn = mean(prop.cells), mdn = median(prop.cells), q5 = quantile(prop.cells, .05),
            q95 = quantile(prop.cells, .95))

AFR.matched.data.z %>% group_by(treatment) %>% 
  summarise(mn = mean(prop.cells), mdn = median(prop.cells), q5 = quantile(prop.cells, .05),
            q95 = quantile(prop.cells, .95))

SEA.matched.data.z %>% group_by(treatment) %>% 
  summarise(mn = mean(prop.cells), mdn = median(prop.cells), q5 = quantile(prop.cells, .05),
            q95 = quantile(prop.cells, .95))

ggplot(LAM.matched.data.z, aes(log(prop.cells))) +
  geom_histogram() +
  facet_wrap(~treatment, ncol = 1)

ggplot(LAM.matched.data.z, aes(treatment, burned.cells/total.cells)) +
  geom_point() +
  geom_boxplot()

ggplot(LAM.matched.data.z, aes(forest.area, prop.cells, colour = treatment)) +
  geom_point() +
 geom_smooth()

LAM.country.sum <- LAM.matched.data.z %>% group_by(treatment, ISO3) %>% 
  summarise(mn = mean(prop.cells), mdn = median(prop.cells), q5 = quantile(prop.cells, .05),
            q95 = quantile(prop.cells, .95), n = n())

AFR.country.sum <- AFR.matched.data.z %>% group_by(treatment, ISO3) %>% 
  summarise(mn = mean(prop.cells), mdn = median(prop.cells), q5 = quantile(prop.cells, .05),
            q95 = quantile(prop.cells, .95), n = n())

SEA.country.sum <- SEA.matched.data.z %>% group_by(treatment, ISO3) %>% 
  summarise(mn = mean(prop.cells), mdn = median(prop.cells), q5 = quantile(prop.cells, .05),
            q95 = quantile(prop.cells, .95), n = n())

ggplot(LAM.country.sum, aes(treatment, mn)) +
  geom_point() +
  geom_errorbar(aes(ymin = q5, ymax = q95), width = 0) +
  facet_wrap(~ISO3, scales = "free")

## Basic X Y only model --------------------------------------------------------
LAM_mod <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
      s(x.z, y.z, k = 50) + 
      s(ISO3, bs='re', by = country_dummy) +
      s(ISO3, treatment, bs='re', by = country_dummy),
    data = LAM.matched.data.z, family = binomial(),
    )
saveRDS(LAM_mod, "Outputs/Models/LAM.Mod1.XY.BAM.rds")
LAM_mod <- read_rds("Outputs/Models/LAM.Mod1.XY.BAM.rds")
summary(LAM_mod)




## LAM : All covars (raw forest, low K) ----------------------------------------
system.time(LAM_mod_for50 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                              s(x.z, y.z, k = 50) + 
                              s(travel.time.z, bs = "cr", k = 20) +
                              s(elevation.z, bs = "cr", k = 20) +
                              s(slope.logz, bs = "cr", k = 20) +
                              s(pop.density.logz, bs = "cr", k = 20) +
                              s(precipitation.z, bs = "cr", k = 20) +
                              s(precipitation.wettest.z, bs = "cr", k = 20) +
                              s(precipitation.driest.z, bs = "cr", k = 20) +
                              s(temperature.hottest.z, bs = "cr", k = 20) +
                              s(fwi.95.z, bs = "cr", k = 20) +
                              s(forest.area.z, bs = "cr", k = 20)+
                              s(ISO3, bs='re', by = country_dummy) +
                              s(ISO3, treatment, bs='re', by = country_trt_dummy),
                            data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod_for50, "Outputs/Models/LAM.Mod.Forest.50.rds")
summary(LAM_mod_for50)
LAM_mod_for50 <- read_rds("Outputs/Models/LAM.Mod.Forest.50.rds")
gam.check(LAM_mod_for50)

system.time(LAM_mod_for100 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                   s(x.z, y.z, k = 100) + 
                                   s(travel.time.z, bs = "cr", k = 20) +
                                   s(elevation.z, bs = "cr", k = 20) +
                                   s(slope.logz, bs = "cr", k = 20) +
                                   s(pop.density.logz, bs = "cr", k = 20) +
                                   s(precipitation.z, bs = "cr", k = 20) +
                                   s(precipitation.wettest.z, bs = "cr", k = 20) +
                                   s(precipitation.driest.z, bs = "cr", k = 20) +
                                   s(temperature.hottest.z, bs = "cr", k = 20) +
                                   s(fwi.95.z, bs = "cr", k = 50) +
                                   s(forest.area.z, bs = "cr", k = 20)+
                                   s(ISO3, bs='re', by = country_dummy) +
                                   s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                 data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod_for100, "Outputs/Models/LAM.Mod.Forest.100.rds")
summary(LAM_mod_for100)

LAM_mod_for100 <- read_rds("Outputs/Models/LAM.Mod.Forest.100.rds")
gam.check(LAM_mod_for100)

system.time(LAM_mod_for200 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                    s(x.z, y.z, k = 200) + 
                                    s(travel.time.z, bs = "cr", k = 20) +
                                    s(elevation.z, bs = "cr", k = 20) +
                                    s(slope.logz, bs = "cr", k = 20) +
                                    s(pop.density.logz, bs = "cr", k = 20) +
                                    s(precipitation.z, bs = "cr", k = 20) +
                                    s(precipitation.wettest.z, bs = "cr", k = 20) +
                                    s(precipitation.driest.z, bs = "cr", k = 20) +
                                    s(temperature.hottest.z, bs = "cr", k = 20) +
                                    s(fwi.95.z, bs = "cr", k = 100) +
                                    s(forest.area.z, bs = "cr", k = 20)+
                                    s(ISO3, bs='re', by = country_dummy) +
                                    s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                  data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod_for200, "Outputs/Models/LAM.Mod.Forest.200.rds")
summary(LAM_mod_for200)

plot(LAM_mod3)
gam.check(LAM_mod3)

system.time(LAM_mod_for200_new <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                    s(x.z, y.z, k = 200) + 
                                    s(travel.time.z, bs = "cr", k = 30) +
                                    s(elevation.z, bs = "cr", k = 30) +
                                    s(slope.logz, bs = "cr", k = 30) +
                                    s(pop.density.logz, bs = "cr", k = 50) +
                                    s(precipitation.z, bs = "cr", k = 30) +
                                    s(precipitation.wettest.z, bs = "cr", k = 30) +
                                    s(precipitation.driest.z, bs = "cr", k = 30) +
                                    s(temperature.hottest.z, bs = "cr", k = 30) +
                                    s(fwi.95.z, bs = "cr", k = 100) +
                                    s(forest.area.z, bs = "cr", k = 30)+
                                    s(ISO3, bs='re', by = country_dummy) +
                                    s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                  data = LAM.matched.data.z, family = binomial()))

saveRDS(LAM_mod_for200_new, "Outputs/Models/LAM.Mod.Forest.200.new.rds")



## AFR : All covars (raw forest, low K) ----------------------------------------
system.time(AFR_mod_for50 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                   s(x.z, y.z, k = 50) + 
                                   s(travel.time.z, bs = "cr", k = 20) +
                                   s(elevation.z, bs = "cr", k = 20) +
                                   s(slope.logz, bs = "cr", k = 20) +
                                   s(pop.density.logz, bs = "cr", k = 20) +
                                   s(precipitation.z, bs = "cr", k = 20) +
                                   s(precipitation.wettest.z, bs = "cr", k = 20) +
                                   s(precipitation.driest.z, bs = "cr", k = 20) +
                                   s(temperature.hottest.z, bs = "cr", k = 20) +
                                   s(fwi.95.z, bs = "cr", k = 20) +
                                   s(forest.area.z, bs = "cr", k = 20)+
                                   s(ISO3, bs='re', by = country_dummy) +
                                   s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                 data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR_mod_for50, "Outputs/Models/AFR.Mod.Forest.50.rds")
summary(AFR_mod_for50)

AFR_mod_for100 <- read_rds("Outputs/Models/AFR.Mod.Forest.100.rds")
gam.check(AFR_mod_for100)

system.time(AFR_mod_for100 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                    s(x.z, y.z, k = 100) + 
                                    s(travel.time.z, bs = "cr", k = 20) +
                                    s(elevation.z, bs = "cr", k = 20) +
                                    s(slope.logz, bs = "cr", k = 20) +
                                    s(pop.density.logz, bs = "cr", k = 20) +
                                    s(precipitation.z, bs = "cr", k = 20) +
                                    s(precipitation.wettest.z, bs = "cr", k = 20) +
                                    s(precipitation.driest.z, bs = "cr", k = 20) +
                                    s(temperature.hottest.z, bs = "cr", k = 20) +
                                    s(fwi.95.z, bs = "cr", k = 50) +
                                    s(forest.area.z, bs = "cr", k = 20)+
                                    s(ISO3, bs='re', by = country_dummy) +
                                    s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                  data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR_mod_for100, "Outputs/Models/AFR.Mod.Forest.100.rds")
summary(AFR_mod_for100)

system.time(AFR_mod_for200 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                    s(x.z, y.z, k = 200) + 
                                    s(travel.time.z, bs = "cr", k = 20) +
                                    s(elevation.z, bs = "cr", k = 20) +
                                    s(slope.logz, bs = "cr", k = 20) +
                                    s(pop.density.logz, bs = "cr", k = 20) +
                                    s(precipitation.z, bs = "cr", k = 20) +
                                    s(precipitation.wettest.z, bs = "cr", k = 20) +
                                    s(precipitation.driest.z, bs = "cr", k = 20) +
                                    s(temperature.hottest.z, bs = "cr", k = 20) +
                                    s(fwi.95.z, bs = "cr", k = 100) +
                                    s(forest.area.z, bs = "cr", k = 20)+
                                    s(ISO3, bs='re', by = country_dummy) +
                                    s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                  data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR_mod_for200, "Outputs/Models/AFR.Mod.Forest.200.rds")
summary(AFR_mod_for200)

gam.check(AFR_mod_for200)

## SEA : All covars (raw forest, low K) ----------------------------------------
system.time(SEA_mod_for50 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                   s(x.z, y.z, k = 50) + 
                                   s(travel.time.logz, bs = "cr", k = 20) + #fine
                                   s(elevation.z, bs = "cr", k = 5) +
                                   s(slope.logz, bs = "cr", k = 20) +
                                   s(pop.density.logz, bs = "cr", k = 20) +
                                   s(precipitation.z, bs = "cr", k = 20) +
                                   s(precipitation.wettest.z, bs = "cr", k = 20) +
                                   s(precipitation.driest.z, bs = "cr", k = 20) +
                                   s(temperature.hottest.z, bs = "cr", k = 20) +
                                    s(fwi.95.z, bs = "cr", k = 20) +
                                    s(forest.area.z, bs = "cr", k = 20)+
                                   s(ISO3, bs='re', by = country_dummy) +
                                   s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                 data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA_mod_for50, "Outputs/Models/SEA.Mod.Forest.50.rds")
summary(SEA_mod_for50)
gam.check(SEA_mod_for50)

system.time(SEA_mod_for100 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                    s(x.z, y.z, k = 100) + 
                                    s(travel.time.logz, bs = "cr", k = 30) +
                                    s(elevation.z, bs = "cr", k = 5) +
                                     s(slope.logz, bs = "cr", k = 30) +
                                     s(pop.density.logz, bs = "cr", k = 30) +
                                    s(precipitation.z, bs = "cr", k = 30) +
                                   s(precipitation.wettest.z, bs = "cr", k = 30) +
                                    s(precipitation.driest.z, bs = "cr", k = 30) +
                                    s(temperature.hottest.z, bs = "cr", k = 30) +
                                    s(fwi.95.z, bs = "cr", k = 50) +
                                    s(forest.area.z, bs = "cr", k = 30)+
                                    s(ISO3, bs='re', by = country_dummy) +
                                    s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                  data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA_mod_for100, "Outputs/Models/SEA.Mod.Forest.100.rds")
summary(SEA_mod_for100)
SEA_mod_for100 <- read_rds("Outputs/Models/SEA.Mod.Forest.100.rds")
gam.check(SEA_mod_for100)

system.time(SEA_mod_for200 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                    s(x.z, y.z, k = 200) + 
                                    s(travel.time.z, bs = "cr", k = 30) +
                                    s(elevation.z, bs = "cr", k = 5) +
                                    s(slope.logz, bs = "cr", k = 30) +
                                    s(pop.density.logz, bs = "cr", k = 30) +
                                    s(precipitation.z, bs = "cr", k = 30) +
                                    s(precipitation.wettest.z, bs = "cr", k = 30) +
                                    s(precipitation.driest.z, bs = "cr", k = 30) +
                                    s(temperature.hottest.z, bs = "cr", k = 30) +
                                    s(fwi.95.z, bs = "cr", k = 100) +
                                    s(forest.area.z, bs = "cr", k = 30)+
                                    s(ISO3, bs='re', by = country_dummy) +
                                    s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                  data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA_mod_for200, "Outputs/Models/SEA.Mod.Forest.200.rds")
summary(SEA_mod_for200)

gam.check(SEA_mod_for200)

system.time(SEA_mod_for50_comp <- avg_comparisons(SEA_mod_for50, 
                                                  variables = list(treatment = c("Non-PA", "PA"))))


## LAM : All covars (raw forest, low K, trt by forest area)---------------------
system.time(LAM_mod_fortrt50 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                              s(x.z, y.z, k = 50) + 
                              s(travel.time.z, bs = "cr", k = 20) +
                              s(elevation.z, bs = "cr", k = 20) +
                              s(slope.logz, bs = "cr", k = 20) +
                              s(pop.density.logz, bs = "cr", k = 20) +
                              s(precipitation.z, bs = "cr", k = 20) +
                              s(precipitation.wettest.z, bs = "cr", k = 20) +
                              s(precipitation.driest.z, bs = "cr", k = 20) +
                              s(temperature.hottest.z, bs = "cr", k = 20) +
                              s(fwi.95.z, bs = "cr", k = 20) +
                              s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                              s(ISO3, bs='re', by = country_dummy) +
                              s(ISO3, treatment, bs='re', by = country_trt_dummy),
                            data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod_fortrt50, "Outputs/Models/LAM.Mod.Forest.Trt.50.rds")
summary(LAM_mod_fortrt50)

system.time(LAM_mod_fortrt100 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                      s(x.z, y.z, k = 100) + 
                                      s(travel.time.z, bs = "cr", k = 20) +
                                      s(elevation.z, bs = "cr", k = 20) +
                                      s(slope.logz, bs = "cr", k = 20) +
                                      s(pop.density.logz, bs = "cr", k = 20) +
                                      s(precipitation.z, bs = "cr", k = 20) +
                                      s(precipitation.wettest.z, bs = "cr", k = 20) +
                                      s(precipitation.driest.z, bs = "cr", k = 20) +
                                      s(temperature.hottest.z, bs = "cr", k = 20) +
                                      s(fwi.95.z, bs = "cr", k = 50) +
                                      s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                      s(ISO3, bs='re', by = country_dummy) +
                                      s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                    data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod_fortrt100, "Outputs/Models/LAM.Mod.Forest.Trt.100.rds")
summary(LAM_mod_fortrt100)

system.time(LAM_mod_fortrt200 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                       s(x.z, y.z, k = 200) + 
                                       s(travel.time.z, bs = "cr", k = 20) +
                                       s(elevation.z, bs = "cr", k = 20) +
                                       s(slope.logz, bs = "cr", k = 20) +
                                       s(pop.density.logz, bs = "cr", k = 20) +
                                       s(precipitation.z, bs = "cr", k = 20) +
                                       s(precipitation.wettest.z, bs = "cr", k = 20) +
                                       s(precipitation.driest.z, bs = "cr", k = 20) +
                                       s(temperature.hottest.z, bs = "cr", k = 20) +
                                       s(fwi.95.z, bs = "cr", k = 100) +
                                       s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                       s(ISO3, bs='re', by = country_dummy) +
                                       s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                     data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod_fortrt200, "Outputs/Models/LAM.Mod.Forest.Trt.200.rds")
summary(LAM_mod_fortrt200)

## AFR : All covars (raw forest, low K, trt by forest area)---------------------
system.time(AFR_mod_fortrt50 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                      s(x.z, y.z, k = 50) + 
                                      s(travel.time.z, bs = "cr", k = 20) +
                                      s(elevation.z, bs = "cr", k = 20) +
                                      s(slope.logz, bs = "cr", k = 20) +
                                      s(pop.density.logz, bs = "cr", k = 20) +
                                      s(precipitation.z, bs = "cr", k = 20) +
                                      s(precipitation.wettest.z, bs = "cr", k = 20) +
                                      s(precipitation.driest.z, bs = "cr", k = 20) +
                                      s(temperature.hottest.z, bs = "cr", k = 20) +
                                      s(fwi.95.z, bs = "cr", k = 20) +
                                      s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                      s(ISO3, bs='re', by = country_dummy) +
                                      s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                    data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR_mod_fortrt50, "Outputs/Models/AFR.Mod.Forest.Trt.50.rds")
summary(AFR_mod_fortrt50)

system.time(AFR_mod_fortrt100 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                       s(x.z, y.z, k = 100) + 
                                       s(travel.time.z, bs = "cr", k = 20) +
                                       s(elevation.z, bs = "cr", k = 20) +
                                       s(slope.logz, bs = "cr", k = 20) +
                                       s(pop.density.logz, bs = "cr", k = 20) +
                                       s(precipitation.z, bs = "cr", k = 20) +
                                       s(precipitation.wettest.z, bs = "cr", k = 20) +
                                       s(precipitation.driest.z, bs = "cr", k = 20) +
                                       s(temperature.hottest.z, bs = "cr", k = 20) +
                                       s(fwi.95.z, bs = "cr", k = 50) +
                                       s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                       s(ISO3, bs='re', by = country_dummy) +
                                       s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                     data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR_mod_fortrt100, "Outputs/Models/AFR.Mod.Forest.Trt.100.rds")
summary(AFR_mod_fortrt100)

system.time(AFR_mod_fortrt200 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                       s(x.z, y.z, k = 200) + 
                                       s(travel.time.z, bs = "cr", k = 20) +
                                       s(elevation.z, bs = "cr", k = 20) +
                                       s(slope.logz, bs = "cr", k = 20) +
                                       s(pop.density.logz, bs = "cr", k = 20) +
                                       s(precipitation.z, bs = "cr", k = 20) +
                                       s(precipitation.wettest.z, bs = "cr", k = 20) +
                                       s(precipitation.driest.z, bs = "cr", k = 20) +
                                       s(temperature.hottest.z, bs = "cr", k = 20) +
                                       s(fwi.95.z, bs = "cr", k = 100) +
                                       s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                       s(ISO3, bs='re', by = country_dummy) +
                                       s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                     data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR_mod_fortrt200, "Outputs/Models/AFR.Mod.Forest.Trt.200.rds")
summary(AFR_mod_fortrt200)

## SEA : All covars (raw forest, low K, trt by forest area)---------------------
system.time(SEA_mod_fortrt50 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                      s(x.z, y.z, k = 50) + 
                                      s(travel.time.logz, bs = "cr", k = 20) +
                                      s(elevation.z, bs = "cr", k = 20) +
                                      s(slope.logz, bs = "cr", k = 20) +
                                      s(pop.density.logz, bs = "cr", k = 20) +
                                      s(precipitation.z, bs = "cr", k = 20) +
                                      s(precipitation.wettest.z, bs = "cr", k = 20) +
                                      s(precipitation.driest.z, bs = "cr", k = 20) +
                                      s(temperature.hottest.z, bs = "cr", k = 20) +
                                      s(fwi.95.z, bs = "cr", k = 20) +
                                      s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                      s(ISO3, bs='re', by = country_dummy) +
                                      s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                    data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA_mod_fortrt50, "Outputs/Models/SEA.Mod.Forest.Trt.50.rds")
summary(SEA_mod_fortrt50)

system.time(SEA_mod_fortrt100 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                       s(x.z, y.z, k = 100) + 
                                       s(travel.time.z, bs = "cr", k = 20) +
                                       s(elevation.z, bs = "cr", k = 20) +
                                       s(slope.logz, bs = "cr", k = 20) +
                                       s(pop.density.logz, bs = "cr", k = 20) +
                                       s(precipitation.z, bs = "cr", k = 20) +
                                       s(precipitation.wettest.z, bs = "cr", k = 20) +
                                       s(precipitation.driest.z, bs = "cr", k = 20) +
                                       s(temperature.hottest.z, bs = "cr", k = 20) +
                                       s(fwi.95.z, bs = "cr", k = 50) +
                                       s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                       s(ISO3, bs='re', by = country_dummy) +
                                       s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                     data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA_mod_fortrt100, "Outputs/Models/SEA.Mod.Forest.Trt.100.rds")
summary(SEA_mod_fortrt100)

system.time(SEA_mod_fortrt200 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                       s(x.z, y.z, k = 200) + 
                                       s(travel.time.z, bs = "cr", k = 20) +
                                       s(elevation.z, bs = "cr", k = 20) +
                                       s(slope.logz, bs = "cr", k = 20) +
                                       s(pop.density.logz, bs = "cr", k = 20) +
                                       s(precipitation.z, bs = "cr", k = 20) +
                                       s(precipitation.wettest.z, bs = "cr", k = 20) +
                                       s(precipitation.driest.z, bs = "cr", k = 20) +
                                       s(temperature.hottest.z, bs = "cr", k = 20) +
                                       s(fwi.95.z, bs = "cr", k = 100) +
                                       s(forest.area.z, by = treatment, bs = "cr", k = 20)+
                                       s(ISO3, bs='re', by = country_dummy) +
                                       s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                     data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA_mod_fortrt200, "Outputs/Models/SEA.Mod.Forest.Trt.200.rds")
summary(SEA_mod_fortrt200)