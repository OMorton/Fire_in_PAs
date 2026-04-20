library(tidyverse)
library(mgcv)
library(marginaleffects)
library(ggpubr)
options(scipen = 999)
data.path <- "X:/morton_research/User/bi1om/Research/Fire/Fire_in_PAs/Analysis/"

## Read in fitting data
load(paste0(data.path,"Data/Model_fitting/LAM.temporal.z.Mar26.RData"))
load(paste0(data.path,"Data/Model_fitting/AFR.temporal.z.Mar26.RData"))
load(paste0(data.path,"Data/Model_fitting/SEA.temporal.z.Mar26.RData"))

## Read in models
LAM.Mod.Temporal.50 <- read_rds(paste0(data.path,"Outputs/Models/Temporal/LAM.Mod.Temporal.50.spat.2026.QB.no.corr.rds"))
AFR.Mod.Temporal.50 <- read_rds(paste0(data.path,"Outputs/Models/Temporal/AFR.Mod.Temporal.3.spat.2026.QB.no.corr.rds"))
SEA.Mod.Temporal.50 <- read_rds(paste0(data.path,"Outputs/Models/Temporal/SEA.Mod.Temporal.30.spat.2026.QB.no.corr.rds"))


## Predicted temporal burn trend -----------------------------------------------
## fix at the mean
LAM.temp.preds <- predictions(LAM.Mod.Temporal.50, newdata = "mean", 
                                   exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                               "s(year.z,treatment,ISO3)"),
                                     by = c("treatment", "year.z"), conf_level = .95)

AFR.temp.preds <- predictions(AFR.Mod.Temporal.50, newdata = "mean", 
                              exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                          "s(year.z,treatment,ISO3)"),
                              by = c("treatment", "year.z"), conf_level = .95)

SEA.temp.preds <- predictions(SEA.Mod.Temporal.50, newdata = "mean", 
                              exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                          "s(year.z,treatment,ISO3)"),
                              by = c("treatment", "year.z"), conf_level = .95)

LAM.temp.preds <- LAM.temp.preds %>% mutate(year = (year.z*sd(LAM.temporal.z$year)) + 
                                               mean(LAM.temporal.z$year))
AFR.temp.preds <- AFR.temp.preds %>% mutate(year = (year.z*sd(AFR.temporal.z$year)) + 
                                              mean(AFR.temporal.z$year))
SEA.temp.preds <- SEA.temp.preds %>% mutate(year = (year.z*sd(SEA.temporal.z$year)) + 
                                              mean(SEA.temporal.z$year))
write.csv(LAM.temp.preds, paste0(data.path,"Outputs/Summaries/Temporal/LAM.ave.preds.Mar26.csv"))
write.csv(AFR.temp.preds, paste0(data.path,"Outputs/Summaries/Temporal/AFR.ave.preds.Mar26.csv"))
write.csv(SEA.temp.preds, paste0(data.path,"Outputs/Summaries/Temporal/SEA.ave.preds.Mar26.csv"))

## Contrasted annual burn ------------------------------------------------------
LAM.temp.comp <- comparisons(LAM.Mod.Temporal.50, newdata = "mean", 
                              exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                          "s(year.z,treatment,ISO3)"),
                             variables = list(treatment = c("PA", "Non-PA")),
                              by = c("year.z"), conf_level = .95)

AFR.temp.comp <- comparisons(AFR.Mod.Temporal.50, newdata = "mean", 
                              exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                          "s(year.z,treatment,ISO3)"),
                             variables = list(treatment = c("PA", "Non-PA")),
                             by = c("year.z"), conf_level = .95)

SEA.temp.comp <- comparisons(SEA.Mod.Temporal.50, newdata = "mean", 
                              exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                          "s(year.z,treatment,ISO3)"),
                             variables = list(treatment = c("PA", "Non-PA")),
                             by = c("year.z"), conf_level = .95)

LAM.temp.comp <- LAM.temp.comp %>% mutate(year = (year.z*sd(LAM.temporal.z$year)) + 
                                              mean(LAM.temporal.z$year))
AFR.temp.comp <- AFR.temp.comp %>% mutate(year = (year.z*sd(AFR.temporal.z$year)) + 
                                              mean(AFR.temporal.z$year))
SEA.temp.comp <- SEA.temp.comp %>% mutate(year = (year.z*sd(SEA.temporal.z$year)) + 
                                              mean(SEA.temporal.z$year))
write.csv(LAM.temp.comp, paste0(data.path,"Outputs/Summaries/Temporal/LAM.ave.comp.Mar26.csv"))
write.csv(AFR.temp.comp, paste0(data.path,"Outputs/Summaries/Temporal/AFR.ave.comp.Mar26.csv"))
write.csv(SEA.temp.comp, paste0(data.path,"Outputs/Summaries/Temporal/SEA.ave.comp.Mar26.csv"))

## annual slopes ---------------------------------------------------------------
LAM.slopes.ave.exc.raw <- slopes(LAM.Mod.Temporal.50, newdata = "mean", 
                                         exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                                     "s(year.z,treatment,ISO3)"),
                                         variables = "year.z", by = c("treatment", "year.z"), 
                                         conf_level = .95)

AFR.slopes.ave.exc.raw <- slopes(AFR.Mod.Temporal.50, newdata = "mean", 
                                 exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                             "s(year.z,treatment,ISO3)"),
                                 variables = "year.z", by = c("treatment", "year.z"), 
                                 conf_level = .95)

SEA.slopes.ave.exc.raw <- slopes(SEA.Mod.Temporal.50, newdata = "mean", 
                                 exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                             "s(year.z,treatment,ISO3)"),
                                 variables = "year.z", by = c("treatment", "year.z"), 
                                 conf_level = .95)

## correct z scoring and convert from the change per sd to change per year
LAM.slopes.ave.exc.raw <- LAM.slopes.ave.exc.raw %>% mutate(year = (year.z*sd(LAM.temporal.z$year)) + 
                                              mean(LAM.temporal.z$year),
                                              estimate = estimate/sd(LAM.temporal.z$year),
                                              conf.low = conf.low/sd(LAM.temporal.z$year),
                                              conf.high = conf.high/sd(LAM.temporal.z$year))
AFR.slopes.ave.exc.raw <- AFR.slopes.ave.exc.raw %>% mutate(year = (year.z*sd(AFR.temporal.z$year)) + 
                                              mean(AFR.temporal.z$year),
                                              estimate = estimate/sd(LAM.temporal.z$year),
                                              conf.low = conf.low/sd(LAM.temporal.z$year),
                                              conf.high = conf.high/sd(LAM.temporal.z$year))
SEA.slopes.ave.exc.raw <- SEA.slopes.ave.exc.raw %>% mutate(year = (year.z*sd(SEA.temporal.z$year)) + 
                                              mean(SEA.temporal.z$year),
                                              estimate = estimate/sd(LAM.temporal.z$year),
                                              conf.low = conf.low/sd(LAM.temporal.z$year),
                                              conf.high = conf.high/sd(LAM.temporal.z$year))
write.csv(LAM.slopes.ave.exc.raw, paste0(data.path,"Outputs/Summaries/Temporal/LAM.slopes.ave.exc.raw.Mar26.csv"))
write.csv(AFR.slopes.ave.exc.raw, paste0(data.path,"Outputs/Summaries/Temporal/AFR.slopes.ave.exc.raw.Mar26.csv"))
write.csv(SEA.slopes.ave.exc.raw, paste0(data.path,"Outputs/Summaries/Temporal/SEA.slopes.ave.exc.raw.Mar26.csv"))


## Average marginal slopes
LAM.slopes.ave.exc <- slopes(LAM.Mod.Temporal.50, newdata = "mean", 
                                         exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                                     "s(year.z,treatment,ISO3)"),
                                         variables = "year.z", by = c("treatment"), 
                                         conf_level = .95)


AFR.slopes.ave.exc <- slopes(AFR.Mod.Temporal.50, newdata = "mean", 
                                         exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                                     "s(year.z,treatment,ISO3)"),
                                         variables = "year.z", by = c("treatment"), 
                                         conf_level = .95)

SEA.slopes.ave.exc <- slopes(SEA.Mod.Temporal.50, newdata = "mean", 
                                         exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                                     "s(year.z,treatment,ISO3)"),
                                         variables = "year.z", by = c("treatment"), 
                                         conf_level = .95)

## convert from the change per sd to change per year
LAM.slopes.ave.exc <- LAM.slopes.ave.exc %>% mutate(estimate = estimate/sd(LAM.temporal.z$year),
                                                            conf.low = conf.low/sd(LAM.temporal.z$year),
                                                            conf.high = conf.high/sd(LAM.temporal.z$year))
AFR.slopes.ave.exc <- AFR.slopes.ave.exc %>% mutate(estimate = estimate/sd(LAM.temporal.z$year),
                                                            conf.low = conf.low/sd(LAM.temporal.z$year),
                                                            conf.high = conf.high/sd(LAM.temporal.z$year))
SEA.slopes.ave.exc <- SEA.slopes.ave.exc %>% mutate(estimate = estimate/sd(LAM.temporal.z$year),
                                                            conf.low = conf.low/sd(LAM.temporal.z$year),
                                                            conf.high = conf.high/sd(LAM.temporal.z$year))
write.csv(LAM.slopes.ave.exc, paste0(data.path,"Outputs/Summaries/Temporal/LAM.slopes.ave.exc.Mar26.csv"))
write.csv(AFR.slopes.ave.exc, paste0(data.path,"Outputs/Summaries/Temporal/AFR.slopes.ave.exc.Mar26.csv"))
write.csv(SEA.slopes.ave.exc, paste0(data.path,"Outputs/Summaries/Temporal/SEA.slopes.ave.exc.Mar26.csv"))

## Contrasting slopes 
LAM.slopes.ave.exc.hyp <- slopes(LAM.Mod.Temporal.50, newdata = "mean", 
                                             exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                                         "s(year.z,treatment,ISO3)"),
                                             variables = "year.z", by = c("treatment", "year.z"), 
                                             hypothesis = "b1 - b2 = 0",
                                             conf_level = .95)

AFR.slopes.ave.exc.hyp <- slopes(AFR.Mod.Temporal.50, newdata = "mean", 
                                 exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                             "s(year.z,treatment,ISO3)"),
                                 variables = "year.z", by = c("treatment"), 
                                 hypothesis = "b1 - b2 = 0",
                                 conf_level = .95)

SEA.slopes.ave.exc.hyp <- slopes(SEA.Mod.Temporal.50, newdata = "mean", 
                                 exclude = c("s(x.z,y.z):treatmentNon-PA", "s(x.z,y.z):treatmentPA",
                                             "s(year.z,treatment,ISO3)"),
                                 variables = "year.z", by = c("treatment"), 
                                 hypothesis = "b1 - b2 = 0",
                                 conf_level = .95)

## convert from the change per sd to change per year
LAM.slopes.ave.exc.hyp <- LAM.slopes.ave.exc.hyp %>% mutate(estimate = estimate/sd(LAM.temporal.z$year),
                                                    conf.low = conf.low/sd(LAM.temporal.z$year),
                                                    conf.high = conf.high/sd(LAM.temporal.z$year))
AFR.slopes.ave.exc.hyp <- AFR.slopes.ave.exc.hyp %>% mutate(estimate = estimate/sd(LAM.temporal.z$year),
                                                    conf.low = conf.low/sd(LAM.temporal.z$year),
                                                    conf.high = conf.high/sd(LAM.temporal.z$year))
SEA.slopes.ave.exc.hyp <- SEA.slopes.ave.exc.hyp %>% mutate(estimate = estimate/sd(LAM.temporal.z$year),
                                                    conf.low = conf.low/sd(LAM.temporal.z$year),
                                                    conf.high = conf.high/sd(LAM.temporal.z$year))

write.csv(LAM.slopes.ave.exc.hyp, paste0(data.path,"Outputs/Summaries/Temporal/LAM.slopes.ave.exc.hyp.Mar26.csv"))
write.csv(AFR.slopes.ave.exc.hyp, paste0(data.path,"Outputs/Summaries/Temporal/AFR.slopes.ave.exc.hyp.Mar26.csv"))
write.csv(SEA.slopes.ave.exc.hyp, paste0(data.path,"Outputs/Summaries/Temporal/SEA.slopes.ave.exc.hyp.Mar26.csv"))

