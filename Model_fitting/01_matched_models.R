
library(tidyverse)
library(mgcv)
library(marginaleffects)
options(scipen = 999)

## Read and format data
data.path <- "X:/morton_research/User/bi1om/Research/Fire/Fire_in_PAs/Analysis/"
## Standardize all variables
load(paste0(data.path,"Data/Matched_data/LAM.matched.data.annual.burn.new.RData"))
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
         surrounding.forest.z = (surrounding.forest-mean(surrounding.forest))/sd(surrounding.forest),
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

load(paste0(data.path,"Data/Matched_data/AFR.matched.data.annual.burn.new.RData"))
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
         surrounding.forest.z = (surrounding.forest-mean(surrounding.forest))/sd(surrounding.forest),
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

load(paste0(data.path,"Data/Matched_data/SEA-AUS.matched.data.annual.burn.new.RData"))
# removed 2 - fiji and singapore
SEA.matched.data <- matched.data %>% group_by(country) %>% filter(n() >= 250)
SEA.matched.data.z <- SEA.matched.data %>% ungroup() %>% 
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

save(LAM.matched.data.z, file = paste0(data.path,"Data/Model_fitting/LAM.matched.data.z.RData"))
save(AFR.matched.data.z, file = paste0(data.path,"Data/Model_fitting/AFR.matched.data.z.RData"))
save(SEA.matched.data.z, file = paste0(data.path,"Data/Model_fitting/SEA.matched.data.z.RData"))

## LAM Model -------------------------------------------------------------------

system.time(LAM.mod.for100.simp <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                         s(x.z, y.z, by = treatment, k = 100) + 
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
                                        s(surrounding.forest.z, bs = "cr", k = 20)+
                                        s(ISO3, bs='re', by = country_dummy) +
                                        s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                      data = LAM.matched.data.z, family = binomial()))

saveRDS(LAM.mod.for100.simp, paste0(data.path,"Outputs/Models/LAM.Mod.Forest.100.spat.2025.rds"))

## AFR Model -------------------------------------------------------------------

system.time(AFR.mod.for100.simp <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                         s(x.z, y.z, by = treatment, k = 100) + 
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
                                         s(surrounding.forest.z, bs = "cr", k = 20)+
                                         s(ISO3, bs='re', by = country_dummy) +
                                        s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                      data = AFR.matched.data.z, family = binomial()))
saveRDS(AFR.mod.for100.simp, paste0(data.path,"Outputs/Models/AFR.Mod.Forest.100.spat.2025.rds"))

system.time(AFR.mod.for100.simp.QB2 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                 s(x.z, y.z, by = treatment, k = 75) + 
                                 s(travel.time.z, bs = "cr", k = 20)+
                                 s(elevation.z, bs = "cr", k = 5) +
                                 s(slope.logz, bs = "cr", k = 20)+
                                 s(pop.density.logz, bs = "cr", k = 20) +
                                 s(precipitation.z, bs = "cr", k = 20) +
                                 s(precipitation.wettest.z, bs = "cr", k = 20) +
                                 s(precipitation.driest.z, bs = "cr", k = 20) +
                                 s(temperature.hottest.z, bs = "cr", k = 20) +
                                 s(fwi.95.z, bs = "cr", k = 50) +
                                 s(forest.area.z, bs = "cr", k = 20)+
                                 s(surrounding.forest.z, bs = "cr", k = 20)+
                                 s(ISO3, bs='re', by = country_dummy) +
                                 s(ISO3, treatment, bs='re', by = country_trt_dummy),
                               data = AFR.matched.data.z, family = quasibinomial()))


saveRDS(AFR.mod.for100.simp.QB2, paste0(data.path,"Outputs/Models/AFR.Mod.Forest.75.spat.2025.QB.rds"))

## SEA Model -------------------------------------------------------------------


system.time(SEA.mod.for100.simp4 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                          s(x.z, y.z, by = treatment, k = 100) + 
                                          s(travel.time.logz, bs = "cr", k = 20) +
                                          s(elevation.z, bs = "cr", k = 5) +
                                          s(slope.logz, bs = "cr", k = 20) +
                                          s(pop.density.logz, bs = "cr", k = 20) +
                                          s(precipitation.z, bs = "cr", k = 20) +
                                          s(precipitation.wettest.z, bs = "cr", k = 20) +
                                          s(precipitation.driest.z, bs = "cr", k = 20) +
                                          s(temperature.hottest.z, bs = "cr", k = 20) +
                                          s(fwi.95.z, bs = "cr", k = 20) +
                                          s(forest.area.z, bs = "cr", k = 25)+
                                          s(surrounding.forest.z, bs = "cr", k = 20)+
                                          s(ISO3, bs='re', by = country_dummy) +
                                          s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                        data = SEA.matched.data.z, family = binomial()))
saveRDS(SEA.mod.for100.simp4, paste0(data.path,"Outputs/Models/SEA.Mod.Forest.25.100.spat.2025.rds"))


system.time(SEA.mod.for100.simp.QB <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                            s(x.z, y.z, by = treatment, k = 100) + 
                                            s(travel.time.logz, bs = "cr", k = 20) +
                                         s(elevation.z, bs = "cr", k = 5) +
                                         s(slope.logz, bs = "cr", k = 20) +
                                         s(pop.density.logz, bs = "cr", k = 20) +
                                         s(precipitation.z, bs = "cr", k = 20) +
                                         s(precipitation.wettest.z, bs = "cr", k = 20) +
                                         s(precipitation.driest.z, bs = "cr", k = 20) +
                                         s(temperature.hottest.z, bs = "cr", k = 20) +
                                         s(fwi.95.z, bs = "cr", k = 50) +
                                         s(forest.area.z, bs = "cr", k = 20)+
                                         s(surrounding.forest.z, bs = "cr", k = 20)+
                                         s(ISO3, bs='re', by = country_dummy) +
                                         s(ISO3, treatment, bs='re', by = country_trt_dummy),
                                       data = SEA.matched.data.z, family = quasibinomial()))
saveRDS(SEA.mod.for100.simp.QB, paste0(data.path,"Outputs/Models/SEA.Mod.Forest.100.spat.2025.QB.rds"))

