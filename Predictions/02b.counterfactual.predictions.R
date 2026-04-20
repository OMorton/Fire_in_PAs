
library(tidyverse)
library(mgcv)
library(marginaleffects)

options(scipen = 999)
data.path <- "X:/morton_research/User/bi1om/Research/Fire/Fire_in_PAs/Analysis/"

## read in data
load(paste0(data.path,"Data/Model_fitting/LAM.matched.data.z.Mar26.RData"))
load(paste0(data.path,"Data/Model_fitting/AFR.matched.data.z.Mar26.RData"))
load(paste0(data.path,"Data/Model_fitting/SEA.matched.data.z.Mar26.RData"))

## read in fitted models
LAM.Mod.Forest.100.QB <- read_rds(paste0(data.path,"Outputs/Models/LAM.Mod.Forest.100.spat.2026.QB.no.corr.rds"))
AFR.Mod.Forest.75.QB <- read_rds(paste0(data.path,"Outputs/Models/AFR.Mod.Forest.5.spat.2026.QB.no.corr.rds"))
SEA.Mod.Forest.75.QB <- read_rds(paste0(data.path,"Outputs/Models/SEA.Mod.Forest.75.spat.2026.QB.no.corr.rds"))

LAM.matched.data.z.ATT <- subset(LAM.matched.data.z, treatment == "PA")
AFR.matched.data.z.ATT <- subset(AFR.matched.data.z, treatment == "PA")
SEA.matched.data.z.ATT <- subset(SEA.matched.data.z, treatment == "PA") 

## Predicted burn probabilities ATT - treatment -------------------------------------

# Predict the observed burn probability in and out of matched PAs cf
LAM.reg.pred.sum.cf.ATT.response.QB <- avg_predictions(LAM.Mod.Forest.100.QB, conf_level = .95, 
                                                    by = "treatment", type = "response",
                                                    newdata = LAM.matched.data.z.ATT,
                                                    variables = list(treatment = c("PA", "Non-PA")))
save(LAM.reg.pred.sum.cf.ATT.response.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.reg.pred.sum.cf.ATT.response.spat.2026.QB.RData"))


AFR.reg.pred.sum.QB.cf.ATT.response <- avg_predictions(AFR.Mod.Forest.75.QB, conf_level = .95, 
                                                       by = "treatment",  type = "response",
                                                       newdata = AFR.matched.data.z.ATT,
                                                       variables = list(treatment = c("PA", "Non-PA")))
save(AFR.reg.pred.sum.QB.cf.ATT.response, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.reg.pred.sum.QB.cf.ATT.response.spat.2026.RData"))


SEA.reg.pred.sum.cf.ATT.response.QB <- avg_predictions(SEA.Mod.Forest.75.QB, conf_level = .95,
                                                    by = "treatment", type = "response",
                                                    newdata = SEA.matched.data.z.ATT,
                                                    variables = list(treatment = c("PA", "Non-PA")))
save(SEA.reg.pred.sum.cf.ATT.response.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.reg.pred.sum.cf.ATT.response.spat.2026.QB.RData"))

## Contrasted burn probabilities ATT - treatment pp  -------------------------------------------------
## By treatment
LAM.trt.comp.QB <- avg_comparisons(LAM.Mod.Forest.100.QB, newdata = LAM.matched.data.z.ATT,
                                variables = list(treatment = c("PA", "Non-PA")), 
                                conf_level = .95)
save(LAM.trt.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.trt.comp.ATT.spat.2026.QB.RData"))

AFR.trt.comp.QB <- avg_comparisons(AFR.Mod.Forest.75.QB,  newdata = AFR.matched.data.z.ATT,
                                   variables = list(treatment = c("PA", "Non-PA")), 
                                   conf_level = .95)
save(AFR.trt.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.trt.comp.QB.ATT.spat.2026.RData"))


SEA.trt.comp.QB <- avg_comparisons(SEA.Mod.Forest.75.QB,  newdata = SEA.matched.data.z.ATT,
                                variables = list(treatment = c("PA", "Non-PA")), 
                                conf_level = .95)
save(SEA.trt.comp.QB, file =paste0(data.path, "Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.trt.comp.ATT.spat.2026.QB.RData"))



## Contrasted burn probabilities ATT - treatment ratio -------------------------

## By treatment ratio
LAM.trt.comp.ratio.QB <- avg_comparisons(LAM.Mod.Forest.100.QB, newdata = LAM.matched.data.z.ATT,
                                      comparison = "ratio",
                                      variables = list(treatment = c("Non-PA", "PA")), 
                                      conf_level = .95)
save(LAM.trt.comp.ratio.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.trt.comp.ratio.ATT.spat.2026.QB.RData"))


AFR.trt.comp.ratio.QB <- avg_comparisons(AFR.Mod.Forest.75.QB,  newdata = AFR.matched.data.z.ATT,
                                         comparison = "ratio",
                                         variables = list(treatment = c("Non-PA", "PA")), 
                                         conf_level = .95)
save(AFR.trt.comp.ratio.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.trt.comp.QB.ratio.ATT.spat.2026.RData"))


SEA.trt.comp.ratio.QB <- avg_comparisons(SEA.Mod.Forest.75.QB,  newdata = SEA.matched.data.z.ATT,
                                      comparison = "ratio",
                                      variables = list(treatment = c("Non-PA", "PA")), 
                                      conf_level = .95)
save(SEA.trt.comp.ratio.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.trt.comp.ratio.ATT.spat.2026.QB.RData"))

## Predicted burn probabilities ATT - country ----------------------------------


## Predict the observed burn probability in and out of matched PAs cf
LAM.reg.pred.sum.cf.ISO.ATT.QB <- avg_predictions(LAM.Mod.Forest.100.QB, conf_level = .95, 
                                               by = c("treatment", "ISO3"),
                                               newdata = LAM.matched.data.z.ATT,
                                               variables = list(treatment = c("PA", "Non-PA")))
save(LAM.reg.pred.sum.cf.ISO.ATT.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.reg.pred.sum.cf.ISO.ATT.spat.2026.QB.RData"))


AFR.reg.pred.sum.QB.cf.ISO.ATT <- avg_predictions(AFR.Mod.Forest.75.QB, conf_level = .95, 
                                                  by = c("treatment", "ISO3"),
                                                  newdata = AFR.matched.data.z.ATT,
                                                  variables = list(treatment = c("PA", "Non-PA")))
save(AFR.reg.pred.sum.QB.cf.ISO.ATT, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.reg.pred.sum.QB.cf.ISO.ATT.spat.2026.RData"))


SEA.reg.pred.sum.cf.ISO.ATT.QB <- avg_predictions(SEA.Mod.Forest.75.QB, conf_level = .95,
                                               by = c("treatment", "ISO3"), 
                                               newdata = SEA.matched.data.z.ATT,
                                               variables = list(treatment = c("PA", "Non-PA")))
save(SEA.reg.pred.sum.cf.ISO.ATT.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.reg.pred.sum.cf.ISO.ATT.spat.2026.QB.RData"))

## Contrasted burn probabilities ATT - country pp ---------------------------------
## By country
LAM.iso.trt.comp.QB <- avg_comparisons(LAM.Mod.Forest.100.QB, newdata = LAM.matched.data.z.ATT,
                                    variables = list(treatment = c("PA", "Non-PA")), 
                                    conf_level = .95, by = c("ISO3"))
save(LAM.iso.trt.comp, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.iso.trt.comp.ATT.spat.2026.QB.RData"))

AFR.iso.trt.comp.QB <- avg_comparisons(AFR.Mod.Forest.75.QB, newdata = AFR.matched.data.z.ATT,
                                    variables = list(treatment = c("PA", "Non-PA")), 
                                    conf_level = .95, by = c("ISO3"))
save(AFR.iso.trt.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.iso.trt.comp.QB.ATT.spat.2026.RData"))


SEA.iso.trt.comp.QB <- avg_comparisons(SEA.Mod.Forest.75.QB, newdata = SEA.matched.data.z.ATT,
                                    variables = list(treatment = c("PA", "Non-PA")), 
                                    conf_level = .95, by = c("ISO3"))
save(SEA.iso.trt.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.iso.trt.comp.ATT.spat.2026.QB.RData"))

## Contrasted burn probabilities ATT - country ratio ---------------------------
LAM.iso.trt.comp.ratio.QB <- avg_comparisons(LAM.Mod.Forest.100.QB, newdata = LAM.matched.data.z.ATT,
                                          comparison = "ratio",
                                          variables = list(treatment = c("Non-PA", "PA")), 
                                          conf_level = .95, by = c("ISO3"))
save(LAM.iso.trt.comp.ratio.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.iso.trt.comp.ratio.ATT.spat.2026.QB.RData"))

AFR.iso.trt.comp.ratio <- avg_comparisons(AFR.Mod.Forest.75.QB, newdata = AFR.matched.data.z.ATT,
                                          comparison = "ratio",
                                          variables = list(treatment = c("Non-PA", "PA")), 
                                          conf_level = .95, by = c("ISO3"))
save(AFR.iso.trt.comp.ratio, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.iso.trt.comp.QB.ratio.ATT.spat.2026.RData"))


SEA.iso.trt.comp.ratio.QB <- avg_comparisons(SEA.Mod.Forest.75.QB, newdata = SEA.matched.data.z.ATT,
                                          comparison = "ratio",
                                          variables = list(treatment = c("Non-PA", "PA")), 
                                          conf_level = .95, by = c("ISO3"))
save(SEA.iso.trt.comp.ratio.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.iso.trt.comp.ratio.ATT.spat.2026.QB.RData"))
## Contrasted burn probabilities ATT - CDF -------------------------------------


LAM.raw.comp.QB <- comparisons(LAM.Mod.Forest.100.QB, newdata = LAM.matched.data.z.ATT,
                            variables = list(treatment = c("PA", "Non-PA")), 
                            conf_level = .95)
save(LAM.raw.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/LAM.raw.comp.spat.2026.QB.RData"))


AFR.raw.comp.QB <- comparisons(AFR.Mod.Forest.75.QB, newdata = AFR.matched.data.z.ATT,
                               variables = list(treatment = c("PA", "Non-PA")), 
                               conf_level = .95)
save(AFR.raw.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/AFR.raw.comp.spat.2026.QB.RData"))


SEA.raw.comp.QB <- comparisons(SEA.Mod.Forest.75.QB, newdata = SEA.matched.data.z.ATT,
                            variables = list(treatment = c("PA", "Non-PA")), 
                            conf_level = .95)
save(SEA.raw.comp.QB, file = paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2026.NC/SEA.raw.comp.spat.2026.QB.RData"))
