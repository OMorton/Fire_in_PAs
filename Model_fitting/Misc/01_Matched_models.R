
library(brms)
library(tidyverse)

## read in data
load("Data/Matched_data/LAM.matched.data.RData")

## Standardize all variables
LAM.matched.data.z <- matched.data %>% ungroup() %>% 
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
         forest.area.z = (forest.area-mean(forest.area))/sd(forest.area)) %>%
  rename("ISO3" = "code")

## Set priors
zc_prior <- c(prior(normal(0,2), class = b),
            prior(normal(0,2), class = Intercept),
            prior(normal(0,2), class = sd))

## fit model
LAM.mod1 <- brm(burned.cells|trials(total.cells) ~ 1 + treatment +
                  s(x.z, y.z, k = 50) +
                  #s(travel.time.z, bs = "cr", k = 20) + 
                  #s(elevation.z, bs = "cr", k = 20) + 
                  #s(slope.z, bs = "cr", k = 20) + 
                  #s(pop.density.z, bs = "cr", k = 20) + 
                  #s(precipitation.wettest.z, bs = "cr", k = 20) + 
                  #s(precipitation.driest.z, bs = "cr", k = 20) + 
                  #s(temperature.hottest.z, bs = "cr", k = 20) + 
                  #s(fwi.95.z, bs = "cr", k = 20) + 
                  #s(forest.area.z, bs = "cr", k = 20) + 
                  (1 + treatment|ISO3),
                prior = zc_prior,
                file = "Outputs/Models/LAM.Mod1.XY.rds",
                data = LAM.matched.data.z, family = binomial(),
                cores = 4, chains = 4, warmup = 1000, iter = 2000)
