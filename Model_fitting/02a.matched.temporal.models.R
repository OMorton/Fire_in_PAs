library(tidyverse)
library(mgcv)

## Read and format data
data.path <- "X:/morton_research/User/bi1om/Research/Fire/Fire_in_PAs/Analysis/"

## LAM data --------------------------------------------------------------------
load(paste0(data.path,"Data/Matched_data/LAM.matched.data.annual.burn.Mar26.RData"))
LAM.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250) %>%
  ungroup() %>% mutate(pixel_id = 1:n(), pixel_id = as.factor(pixel_id))
LAM.long <- pivot_longer(!c(x:subclass, pixel_id), names_to = "year", values_to = "annual.burn.cells",
                         data = LAM.matched.data)
LAM.long <- LAM.long %>% 
  rename("Fyear" = "year") %>%
  mutate(year = as.numeric(as.character(gsub("burned.", "", Fyear)))) %>% 
  group_by(x,y) %>% arrange(x, y, year) %>% 
  mutate(cum.burn = lag(cumsum(annual.burn.cells)),
         cum.burn = ifelse(is.na(cum.burn), 0, cum.burn),
         remaining.total = total.cells - cum.burn,
         remaining.unburn = remaining.total - annual.burn.cells) %>% 
  # remove those cells in those later years where all forest is lost
  filter((annual.burn.cells + remaining.unburn)!=0)

## standardise 
LAM.temporal.z <- LAM.long %>% ungroup() %>% 
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
         surrounding.forest.z = (surrounding.forest-mean(surrounding.forest))/sd(surrounding.forest),
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
         year.z = (year-mean(year))/sd(year),
         prop.cells = annual.burn.cells/remaining.total,
         country_dummy = 1, 
         country_trt_dummy = 1,
         year_trt_dummy = 1,
         year_country_dummy = 1,
         treatment = as.factor(treatment),
         code = as.factor(code),
         Fyear = as.factor(Fyear)) %>%
  rename("ISO3" = "code") %>% as.data.frame()


## AFR data --------------------------------------------------------------------

load(paste0(data.path,"Data/Matched_data/AFR.matched.data.annual.burn.Mar26.RData"))
AFR.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250)
AFR.long <- pivot_longer(!c(x:subclass), names_to = "year", values_to = "annual.burn.cells",
                         data = AFR.matched.data)
AFR.long <- AFR.long %>% 
  rename("Fyear" = "year") %>%
  mutate(year = as.numeric(as.character(gsub("burned.", "", Fyear)))) %>% 
  group_by(x,y) %>% arrange(x, y, year) %>% 
  mutate(cum.burn = lag(cumsum(annual.burn.cells)),
         cum.burn = ifelse(is.na(cum.burn), 0, cum.burn),
         remaining.total = total.cells - cum.burn,
         remaining.unburn = remaining.total - annual.burn.cells) %>% 
  filter((annual.burn.cells + remaining.unburn)!=0)

## standardise 
AFR.temporal.z <- AFR.long %>% ungroup() %>% 
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
         surrounding.forest.z = (surrounding.forest-mean(surrounding.forest))/sd(surrounding.forest),
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
         year.z = (year-mean(year))/sd(year),
         prop.cells = annual.burn.cells/remaining.total,
         country_dummy = 1, 
         country_trt_dummy = 1,
         year_trt_dummy = 1,
         year_country_dummy = 1,
         treatment = as.factor(treatment),
         code = as.factor(code),
         Fyear = as.factor(Fyear)) %>%
  rename("ISO3" = "code") %>% as.data.frame()

## SEA data --------------------------------------------------------------------

load(paste0(data.path,"Data/Matched_data/SEA-AUS.matched.data.annual.burn.Mar26.RData"))
SEA.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250)
SEA.long <- pivot_longer(!c(x:subclass), names_to = "year", values_to = "annual.burn.cells",
                         data = SEA.matched.data)
SEA.long <- SEA.long %>% 
  rename("Fyear" = "year") %>%
  mutate(year = as.numeric(as.character(gsub("burned.", "", Fyear)))) %>% 
  group_by(x,y) %>% arrange(x, y, year) %>% 
  mutate(cum.burn = lag(cumsum(annual.burn.cells)),
         cum.burn = ifelse(is.na(cum.burn), 0, cum.burn),
         remaining.total = total.cells - cum.burn,
         remaining.unburn = remaining.total - annual.burn.cells) %>% 
  filter((annual.burn.cells + remaining.unburn)!=0)

## standardise 
SEA.temporal.z <- SEA.long %>% ungroup() %>% 
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
         surrounding.forest.z = (surrounding.forest-mean(surrounding.forest))/sd(surrounding.forest),
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
         year.z = (year-mean(year))/sd(year),
         prop.cells = annual.burn.cells/remaining.total,
         country_dummy = 1, 
         country_trt_dummy = 1,
         year_trt_dummy = 1,
         year_country_dummy = 1,
         treatment = as.factor(treatment),
         code = as.factor(code),
         Fyear = as.factor(Fyear)) %>%
  rename("ISO3" = "code") %>% as.data.frame()

save(LAM.temporal.z, file = paste0(data.path,"Data/Model_fitting/LAM.temporal.z.Mar26.RData"))
save(AFR.temporal.z, file = paste0(data.path,"Data/Model_fitting/AFR.temporal.z.Mar26.RData"))
save(SEA.temporal.z, file = paste0(data.path,"Data/Model_fitting/SEA.temporal.z.Mar26.RData"))

# precipitation.z  * precipitation.wettest.z 0.78
# precipitation.driest.z * fwi.95.z 0.78
# temperature.hottest.z * elevation.z 0.94
# surrounding.forest.z * forest.area.z 0.81
LAM.temporal.z %>% select(x.z, y.z, year.z, travel.time.z, elevation.z,
                          slope.logz, pop.density.logz, precipitation.z, 
                          precipitation.wettest.z, precipitation.driest.z,
                          temperature.hottest.z, fwi.95.z, forest.area.z, 
                          surrounding.forest.z) %>% cor() %>% round(digits = 2)

# temperature.hottest.z * elevation.z 0.94
# precipitation.driest.z * fwi.95.z 0.76
# surrounding.forest.z * forest.area.z 0.86
# precipitation.z  * precipitation.wettest.z 0.71
AFR.temporal.z %>% select(x.z, y.z, year.z, travel.time.z, elevation.z,
                          slope.logz, pop.density.logz, precipitation.z, 
                          precipitation.wettest.z, precipitation.driest.z,
                          temperature.hottest.z, fwi.95.z, forest.area.z, 
                          surrounding.forest.z) %>% cor() %>% round(digits = 2)

# surrounding.forest.z * forest.area.z 0.81
# precipitation.driest.z * fwi.95.z 0.71
# temperature.hottest.z * elevation.z 0.98
# precipitation.z  * precipitation.wettest.z 0.61
SEA.temporal.z %>% select(x.z, y.z, year.z, travel.time.z, elevation.z,
                          slope.logz, pop.density.logz, precipitation.z, 
                          precipitation.wettest.z, precipitation.driest.z,
                          temperature.hottest.z, fwi.95.z, forest.area.z, 
                          surrounding.forest.z) %>% cor() %>% round(digits = 2)

## to drop from all models
# forest.area.z - surrounding forest has greater landscape implications
# precipitation.driest.z
# temperature.hottest.z 
# precipitation.wettest.z
LAM.raw.sum <- LAM.temporal.z %>% group_by(treatment, Fyear, year) %>% 
  summarise(mn = mean(prop.cells, na.rm = T), sd = sd(prop.cells, na.rm = T))
AFR.raw.sum <- AFR.temporal.z %>% group_by(treatment, Fyear, year) %>% 
  summarise(mn = mean(prop.cells, na.rm = T), sd = sd(prop.cells, na.rm = T))
SEA.raw.sum <- SEA.temporal.z %>% group_by(treatment, Fyear, year) %>% 
  summarise(mn = mean(prop.cells, na.rm = T), sd = sd(prop.cells, na.rm = T))

ggplot(LAM.raw.sum, aes(year, mn, colour = treatment)) +
  geom_point()
ggplot(AFR.raw.sum, aes(year, mn, colour = treatment)) +
  geom_point()
ggplot(SEA.raw.sum, aes(year, mn, colour = treatment)) +
  geom_point()

## Model aim
## To estimate whether PAs are decoupled from the unprotected area fire regime
## The target quantity in this case is the annual slope in and outside of PA's.

## Model description
## In addition to matching to ensure reasonable comparison, all matching variables
## are also included in the full model.
## The main treatment effect is included and also allowed to vary by country.
## Effectiveness varying by country is expected due to a range of unmeasured/unmeasureable
## nationally varying factors e.g. PA staffing, PA funding, Law enforcement etc.
## To estimate a 



## LAM Model fitting -----------------------------------------------------------

system.time(LAM_mod_temp50 <- bam(cbind(annual.burn.cells, remaining.unburn) ~ 1 + treatment +
                                    s(x.z, y.z, by = treatment, k = 30) + 
                                    s(year.z, by = treatment, bs='tp', k = 6) +
                                    s(year.z, treatment, ISO3, bs = c("re")) +
                                    s(travel.time.z, bs = "cr", k = 10) +
                                    s(elevation.z, bs = "cr", k = 10) +
                                    s(slope.logz, bs = "cr", k = 10) +
                                    s(pop.density.logz, bs = "cr", k = 10) +
                                    s(precipitation.z, bs = "cr", k = 10) +
                                    s(fwi.95.z, bs = "cr", k = 10) +
                                    s(surrounding.forest.z, bs = "cr", k = 10),
                                 data = LAM.temporal.z, family = quasibinomial()))
saveRDS(LAM_mod_temp50, paste0(data.path,"Outputs/Models/Temporal/LAM.Mod.Temporal.50.spat.2026.QB.no.corr.rds"))

## AFR Model fitting -----------------------------------------------------------



system.time(AFR.temp.simp3 <- bam(cbind(annual.burn.cells, remaining.unburn) ~ 1 + treatment +
                                    #s(x.z, y.z, by = treatment, k = 30) + 
                                    s(x.z, y.z, by = treatment, k = 3) + 
                                    s(year.z, by = treatment, bs='tp', k = 6) +
                                    s(year.z, treatment, ISO3, bs = c("re")) +
                                    s(travel.time.z, bs = "cr", k = 10) +
                                    s(elevation.z, bs = "cr", k = 5) +
                                    s(slope.logz, bs = "cr", k = 10) +
                                    s(pop.density.logz, bs = "cr", k = 10) +
                                    s(precipitation.z, bs = "cr", k = 10) +
                                    s(fwi.95.z, bs = "cr", k = 10) +
                                    s(surrounding.forest.z, bs = "cr", k = 10),
                                  data = AFR.temporal.z, family = quasibinomial()))
saveRDS(AFR.temp.simp3, paste0(data.path,"Outputs/Models/Temporal/AFR.Mod.Temporal.3.spat.2026.QB.no.corr.rds"))
summary(AFR.temp.simp3)
## SEA Model fitting -----------------------------------------------------------

system.time(SEA_mod_temp30 <- bam(cbind(annual.burn.cells, remaining.unburn) ~ 1 + treatment +
                                    s(x.z, y.z, by = treatment, k = 30) + 
                                    s(year.z, by = treatment, bs='tp', k = 6) +
                                    s(year.z, treatment, ISO3, bs = c("re")) +
                                    s(travel.time.z, bs = "cr", k = 10) +
                                    s(elevation.z, bs = "cr", k = 5) +
                                    s(slope.logz, bs = "cr", k = 10) +
                                    s(pop.density.logz, bs = "cr", k = 10) +
                                    s(precipitation.z, bs = "cr", k = 10) +
                                    s(fwi.95.z, bs = "cr", k = 10) +
                                    s(surrounding.forest.z, bs = "cr", k = 10),
                                  data = SEA.temporal.z, family = quasibinomial()))
saveRDS(SEA_mod_temp30, paste0(data.path,"Outputs/Models/Temporal/SEA.Mod.Temporal.30.spat.2026.QB.no.corr.rds"))


