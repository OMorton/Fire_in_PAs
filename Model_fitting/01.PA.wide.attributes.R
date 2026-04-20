
library(tidyverse)
library(mgcv)
library(marginaleffects)
library(ggpubr)
options(scipen = 999)

## Read and format data
data.path <- "X:/morton_research/User/bi1om/Research/Fire/Fire_in_PAs/Analysis/"

load(paste0(data.path,"Data/Matched_data/PA.wide.attributes.Apr26/individual.pa.burn.summary.plus.attributes.and.covariates.Rdata"))

## 4351
pa.burn.summary.all.df <- pa.burn.summary.all.df  %>% filter(STATUS_YR < 2001)

# categorize protection - 4241 (reduces to 2775 if dropping not reported)
pa.summary <- pa.burn.summary.all.df %>%
  mutate(pa.type = case_when(IUCN_CAT == "VI" ~ "Multi-use",
                             IUCN_CAT == "V" ~ "Multi-use",
                             IUCN_CAT == "IV" ~ "Strict",
                             IUCN_CAT == "III" ~ "Strict",
                             IUCN_CAT == "II" ~ "Strict",
                             IUCN_CAT == "Ia" ~ "Strict",
                             IUCN_CAT == "Ib" ~ "Strict",
                             IUCN_CAT %in% c("Not Reported", "Not Assigned") ~ "Not known",
                             is.na(IUCN_CAT) ~ "Not known")) %>%
  filter(total.forest.area != 0, !is.na(pa_size), !is.na(x), !is.na(y),
         !is.na(fwi.95), !is.na(pop.density), !is.na(travel.time), !is.na(elevation),
         !is.na(slope), !is.na(precipitation), !is.na(buffer.forest.prop),
         !is.na(temperature), !is.na(temperature.hottest))
  #filter(pa.type != "Not known")

## raw plots
ggplot(pa.summary, aes(GIS_AREA, prop.burned)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~region) +
  scale_x_log10()

ggplot(pa.summary, aes(total.forest.area, prop.burned)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~region)

ggplot(pa.summary, aes(pa.type, prop.burned)) +
  geom_violin() +
  facet_wrap(~region) 

## standardize
pa.summary.z <- pa.summary %>% group_by(region) %>%
  mutate(
    # controls
    x.z = (x-mean(x))/sd(x),
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
    slope.logz = (log10(slope+1)-mean(log10(slope+1)))/sd(log10(slope+1)),
    pop.density.logz = (log10(pop.density+1)-mean(log10(pop.density+1)))/sd(log10(pop.density+1)),
    PA.area.logz = (log10(pa_size)-mean(log10(pa_size)))/sd(log10(pa_size)),
         buffer.forest.prop.z = (buffer.forest.prop-mean(buffer.forest.prop))/sd(buffer.forest.prop),
         prop.burned = prop.burned/100, 
         country_dummy = 1,
         ISO3 = as.factor(ISO3)) %>% ungroup()

## Model fitting ---------------------------------------------------------------

LAM.PA <- pa.summary.z %>% filter(region == "LAM")
AFR.PA <- pa.summary.z %>% filter(region == "AFR")
SEA.PA <- pa.summary.z %>% filter(region == "SEA-AUS")

#temperature.hottest.z elevation.z -0.96
#precipitation.z precipitation.wettest.z 0.89
f <- LAM.PA %>% select(x.z, y.z, travel.time.z, elevation.z,
                           slope.logz, pop.density.logz, precipitation.z, 
                           precipitation.wettest.z, precipitation.driest.z,
                           temperature.hottest.z, fwi.95.z,  temperature.z,
                           buffer.forest.prop.z, PA.area.logz) %>%
  cor() %>% round(digits = 2) %>% as.data.frame() %>% rownames_to_column(var = "var1") %>%
  pivot_longer(!var1, names_to = "var2", values_to = "corr")

#elevation.z temperature.hottest.z -0.93
# elevation.z temperature.z -0.89
#temperature.hottest.z temperature.z 0.9
# precipitation.z precipitation.wettest.z 0.92
f <- AFR.PA %>% select(x.z, y.z, travel.time.z, elevation.z,
                       slope.logz, pop.density.logz, precipitation.z, 
                       precipitation.wettest.z, precipitation.driest.z,
                       temperature.hottest.z, fwi.95.z,  temperature.z,
                       buffer.forest.prop.z, PA.area.logz) %>%
  cor() %>% round(digits = 2) %>% as.data.frame() %>% rownames_to_column(var = "var1") %>%
  pivot_longer(!var1, names_to = "var2", values_to = "corr")

#elevation.z temperature.hottest.z -0.93
#elevation.z temperature.z -0.91
# precipitation.driest.z fwi.95.z -0.72
f <- SEA.PA %>% select(x.z, y.z, travel.time.z, elevation.z,
                       slope.logz, pop.density.logz, precipitation.z, 
                       precipitation.wettest.z, precipitation.driest.z,
                       temperature.hottest.z, fwi.95.z,  temperature.z,
                       buffer.forest.prop.z, PA.area.logz) %>%
  cor() %>% round(digits = 2) %>% as.data.frame() %>% rownames_to_column(var = "var1") %>%
  pivot_longer(!var1, names_to = "var2", values_to = "corr")

system.time(LAM.simp.v1 <- bam(prop.burned ~ 1 + pa.type +
                                 s(PA.area.logz, bs = "cr", k = 10) +
                                 s(buffer.forest.prop.z, bs = "cr", k = 10) +
                                 # control 
                                 s(x.z, y.z, k = 10) + 
                                 s(travel.time.z, bs = "cr", k = 10) +
                                 s(elevation.z, bs = "cr", k = 10) +
                                 s(slope.logz, bs = "cr", k = 10) +
                                 s(pop.density.logz, bs = "cr", k = 10) +
                                 s(precipitation.z, bs = "cr", k = 10) +
                                 s(fwi.95.z, bs = "cr", k = 10) +
                                 s(ISO3, bs='re', by = country_dummy),
                               weights = total.forest.area,
                               data = filter(pa.summary.z, region == "LAM"), family = quasibinomial()))
summary(LAM.simp.v1)
saveRDS(LAM.simp.v1, paste0(data.path,"Outputs/Models/Heterogeneity/LAM.Mod.PA.level.rds"))


system.time(AFR.simp.v1 <- bam(prop.burned ~ 1 + pa.type +
                                 s(PA.area.logz, bs = "cr", k = 10) +
                                 s(buffer.forest.prop.z, bs = "cr", k = 10) +
                                 # control 
                                 s(x.z, y.z, k = 10) + 
                                 s(travel.time.z, bs = "cr", k = 10) +
                                 s(elevation.z, bs = "cr", k = 10) +
                                 s(slope.logz, bs = "cr", k = 10) +
                                 s(pop.density.logz, bs = "cr", k = 10) +
                                 s(precipitation.z, bs = "cr", k = 10) +
                                 s(fwi.95.z, bs = "cr", k = 10) +
                                 s(ISO3, bs='re', by = country_dummy),
                               weights = total.forest.area,
                               data = filter(pa.summary.z, region == "AFR"), family = quasibinomial()))
summary(AFR.simp.v1)
saveRDS(AFR.simp.v1, paste0(data.path,"Outputs/Models/Heterogeneity/AFR.Mod.PA.level.rds"))


system.time(SEA.simp.v1 <- bam(prop.burned ~ 1 + pa.type +
                                 s(PA.area.logz, bs = "cr", k = 10) +
                                 s(buffer.forest.prop.z, bs = "cr", k = 10) +
                                 # control 
                                 s(x.z, y.z, k = 10) + 
                                 s(travel.time.z, bs = "cr", k = 10) +
                                 s(elevation.z, bs = "cr", k = 10) +
                                 s(slope.logz, bs = "cr", k = 10) +
                                 s(pop.density.logz, bs = "cr", k = 10) +
                                 s(precipitation.z, bs = "cr", k = 10) +
                                 s(fwi.95.z, bs = "cr", k = 10) +
                                 s(ISO3, bs='re', by = country_dummy),
                               weights = total.forest.area,
                               data = filter(pa.summary.z, region == "SEA-AUS"), family = quasibinomial()))
summary(SEA.simp.v1)
saveRDS(SEA.simp.v1, paste0(data.path,"Outputs/Models/Heterogeneity/SEA.Mod.PA.level.rds"))

## Predictions -----------------------------------------------------------------  
LAM.simp.v1 <- readRDS(paste0(data.path,"Outputs/Models/Heterogeneity/LAM.Mod.PA.level.rds"))
AFR.simp.v1 <- readRDS(paste0(data.path,"Outputs/Models/Heterogeneity/AFR.Mod.PA.level.rds"))
SEA.simp.v1 <- readRDS(paste0(data.path,"Outputs/Models/Heterogeneity/SEA.Mod.PA.level.rds"))


LAM.area <- seq(from = min(LAM.PA$PA.area.logz), to = max(LAM.PA$PA.area.logz), length.out = 50)
LAM.buff.forest <- seq(from = min(LAM.PA$buffer.forest.prop.z), to = max(LAM.PA$buffer.forest.prop.z), length.out = 50)

LAM.area.preds <- predictions(LAM.simp.v1, 
                              newdata = datagrid(country_dummy = 0, 
                                                 PA.area.logz = LAM.area),
                              #Calculate on the logit scale
                              type = "link", 
                              #ack-transform the estimate
                              transform = plogis, 
                              exclude = c("s(ISO3)", "s(x.z, y.z)"),
                              by = "PA.area.logz", conf_level = 0.95)

LAM.area.preds <- LAM.area.preds %>% mutate(pa.area = 10^((PA.area.logz*sd(log10(LAM.PA$pa_size))) + 
                                                            mean(log10(LAM.PA$pa_size))))

LAM.type.preds <- predictions(LAM.simp.v1, 
                              newdata = datagrid(country_dummy = 0, 
                                                 PA.area.logz = 0, 
                                                 pa.type = c("Multi-use", "Strict", "Not known")),
                              #Calculate on the logit scale
                              type = "link", 
                              #ack-transform the estimate
                              transform = plogis, 
                              exclude = c("s(ISO3)", "s(x.z, y.z)"),
                              by = "pa.type", conf_level = 0.95)

LAM.forest.preds <- predictions(LAM.simp.v1, 
                                newdata = datagrid(country_dummy = 0, 
                                                   buffer.forest.prop.z = LAM.buff.forest),
                                #Calculate on the logit scale
                                type = "link", 
                                #ack-transform the estimate
                                transform = plogis, 
                                exclude = c("s(ISO3)", "s(x.z, y.z)"),
                                by = "buffer.forest.prop.z", conf_level = 0.95)

LAM.forest.preds <- LAM.forest.preds %>% mutate(buffer.forest = (buffer.forest.prop.z*sd(LAM.PA$buffer.forest.prop)) + 
                                                  mean(LAM.PA$buffer.forest.prop))

## AFR
AFR.area <- seq(from = min(AFR.PA$PA.area.logz), to = max(AFR.PA$PA.area.logz), length.out = 50)
AFR.buff.forest <- seq(from = min(AFR.PA$buffer.forest.prop.z), to = max(AFR.PA$buffer.forest.prop.z), length.out = 50)

AFR.area.preds <- predictions(AFR.simp.v1, 
                              newdata = datagrid(country_dummy = 0, 
                                                 PA.area.logz = AFR.area),
                              #Calculate on the logit scale
                              type = "link", 
                              #ack-transform the estimate
                              transform = plogis, 
                              exclude = c("s(ISO3)", "s(x.z, y.z)"),
                              by = "PA.area.logz", conf_level = 0.95)

AFR.area.preds <- AFR.area.preds %>% mutate(pa.area = 10^((PA.area.logz*sd(log10(AFR.PA$pa_size))) + 
                                                            mean(log10(AFR.PA$pa_size))))

AFR.type.preds <- predictions(AFR.simp.v1, 
                              newdata = datagrid(country_dummy = 0, 
                                                 PA.area.logz = 0, 
                                                 pa.type = c("Multi-use", "Strict", "Not known")),
                              #Calculate on the logit scale
                              type = "link", 
                              #ack-transform the estimate
                              transform = plogis, 
                              exclude = c("s(ISO3)", "s(x.z, y.z)"),
                              by = "pa.type", conf_level = 0.95)

AFR.forest.preds <- predictions(AFR.simp.v1, 
                                  newdata = datagrid(country_dummy = 0, 
                                                     buffer.forest.prop.z = AFR.buff.forest),
                                  #Calculate on the logit scale
                                  type = "link", 
                                  #ack-transform the estimate
                                  transform = plogis, 
                                  exclude = c("s(ISO3)", "s(x.z, y.z)"),
                                  by = "buffer.forest.prop.z", conf_level = 0.95)

AFR.forest.preds <- AFR.forest.preds %>% mutate(buffer.forest = (buffer.forest.prop.z*sd(AFR.PA$buffer.forest.prop)) + 
                                                      mean(AFR.PA$buffer.forest.prop))

## SEA
SEA.area <- seq(from = min(SEA.PA$PA.area.logz), to = max(SEA.PA$PA.area.logz), length.out = 50)
SEA.buff.forest <- seq(from = min(SEA.PA$buffer.forest.prop.z), to = max(SEA.PA$buffer.forest.prop.z), length.out = 50)

SEA.area.preds <- predictions(SEA.simp.v1, 
                              newdata = datagrid(country_dummy = 0, 
                                                 PA.area.logz = SEA.area),
                              #Calculate on the logit scale
                              type = "link", 
                              #ack-transform the estimate
                              transform = plogis, 
                              exclude = c("s(ISO3)", "s(x.z, y.z)"),
                              by = "PA.area.logz", conf_level = 0.95)

SEA.area.preds <- SEA.area.preds %>% mutate(pa.area = 10^((PA.area.logz*sd(log10(SEA.PA$pa_size))) + 
                                                            mean(log10(SEA.PA$pa_size))))

SEA.type.preds <- predictions(SEA.simp.v1, 
                              newdata = datagrid(country_dummy = 0, 
                                                 PA.area.logz = 0, 
                                                 pa.type = c("Multi-use", "Strict", "Not known")),
                              #Calculate on the logit scale
                              type = "link", 
                              #ack-transform the estimate
                              transform = plogis, 
                              exclude = c("s(ISO3)", "s(x.z, y.z)"),
                              by = "pa.type", conf_level = 0.95)

SEA.forest.preds <- predictions(SEA.simp.v1, 
                                newdata = datagrid(country_dummy = 0, 
                                                   buffer.forest.prop.z = SEA.buff.forest),
                                #Calculate on the logit scale
                                type = "link", 
                                #ack-transform the estimate
                                transform = plogis, 
                                exclude = c("s(ISO3)", "s(x.z, y.z)"),
                                by = "buffer.forest.prop.z", conf_level = 0.95)

SEA.forest.preds <- SEA.forest.preds %>% mutate(buffer.forest = (buffer.forest.prop.z*sd(SEA.PA$buffer.forest.prop)) + 
                                                  mean(SEA.PA$buffer.forest.prop))

## plotting --------------------------------------------------------------------

LAM.area.plt <- ggplot(filter(LAM.area.preds), aes(pa.area, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = "black") +
  geom_line() +
  ylab("Probability \nof fire") +
  xlab("PA area") +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()

LAM.type.plt <- ggplot(LAM.type.preds, aes(pa.type, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point() +
  xlab("Protection type") +
  ylab("Probability \nof fire") +
  theme_bw()

LAM.buffer.plt <- ggplot(LAM.forest.preds, aes(buffer.forest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = "black") +
  geom_line() +
  ylab("Probability \nof fire") +
  xlab("FOrest within 50km") +
  #scale_y_log10() +
  #scale_x_log10()
  theme_bw()

AFR.area.plt <- ggplot(filter(AFR.area.preds, pa.area >10), aes(pa.area, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = "black") +
  geom_line() +
  ylab("Probability \nof fire") +
  xlab("PA area") +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()

AFR.type.plt <- ggplot(AFR.type.preds, aes(pa.type, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point() +
  xlab("Protection type") +
  ylab("Probability \nof fire") +
  theme_bw()


AFR.buffer.plt <- ggplot(AFR.forest.preds, aes(buffer.forest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = "black") +
  geom_line() +
  ylab("Probability \nof fire") +
  xlab("FOrest within 50km") +
  #scale_y_log10() +
  #scale_x_log10()
  theme_bw()

SEA.area.plt <- ggplot(filter(SEA.area.preds, pa.area >10), aes(pa.area, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = "black") +
  geom_line() +
  ylab("Probability \nof fire") +
  xlab("PA area") +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()

SEA.type.plt <- ggplot(SEA.type.preds, aes(pa.type, estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point() +
  xlab("Protection type") +
  ylab("Probability \nof fire") +
  theme_bw()


SEA.buffer.plt <- ggplot(SEA.forest.preds, aes(buffer.forest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = "black") +
  geom_line() +
  ylab("Probability \nof fire") +
  xlab("FOrest within 50km") +
  #scale_y_log10() +
  #scale_x_log10()
  theme_bw()
  

fits <- ggpubr::ggarrange(LAM.type.plt, LAM.area.plt, LAM.buffer.plt,
                  AFR.type.plt, AFR.area.plt, AFR.buffer.plt,
                  SEA.type.plt, SEA.area.plt, SEA.buffer.plt,
                  ncol = 3, nrow = 3)

ggsave(path = paste0(data.path,"Outputs/Figures/Spat.QB.2026"), 
       filename = "PA.wide.attr.v1.png",
       fits, bg = "white",
       device = "png", width = 25, height = 20, units = "cm")

## Comparisons -----------------------------------------------------------------

## LAM
LAM.comp <- comparisons(LAM.simp.v1, newdata = datagrid(country_dummy = 0),
                        variables = list(pa.type = "pairwise"),
                        exclude = c("s(ISO3)", "s(x.z, y.z)"),
                        type = "response", conf_level = 0.95)

LAM.area.slopes <- slopes(LAM.simp.v1, 
                          newdata = datagrid(country_dummy = 0, PA.area.logz = LAM.area),
                          exclude = c("s(x.z,y.z)", "s(ISO3)"),
                          type = "response", variables = "PA.area.logz",
                          by = "PA.area.logz", conf_level = 0.95)

LAM.area.slopes <- LAM.area.slopes %>% mutate(pa.area = 10^((PA.area.logz*sd(log10(LAM.PA$pa_size))) + 
                                                              mean(log10(LAM.PA$pa_size))))

LAM.forest.slopes <- slopes(LAM.simp.v1, 
                            newdata = datagrid(country_dummy = 0, buffer.forest.prop.z = LAM.buff.forest),
                            exclude = c("s(x.z,y.z)", "s(ISO3)"),
                            type = "response", variables = "buffer.forest.prop.z",
                            by = "buffer.forest.prop.z", conf_level = 0.95)

LAM.forest.slopes <- LAM.forest.slopes %>% mutate(buffer.forest = (buffer.forest.prop.z*sd(LAM.PA$buffer.forest.prop)) + 
                                                    mean(LAM.PA$buffer.forest.prop))

## AFR
AFR.comp <- comparisons(AFR.simp.v1, newdata = datagrid(country_dummy = 0),
                        variables = list(pa.type = "pairwise"),
                        exclude = c("s(ISO3)", "s(x.z, y.z)"),
                        type = "response", conf_level = 0.95)

AFR.area.slopes <- slopes(AFR.simp.v1, 
                          newdata = datagrid(country_dummy = 0, PA.area.logz = AFR.area),
                          exclude = c("s(x.z,y.z)", "s(ISO3)"),
                          type = "response", variables = "PA.area.logz",
                          by = "PA.area.logz", conf_level = 0.95)

AFR.area.slopes <- AFR.area.slopes %>% mutate(pa.area = 10^((PA.area.logz*sd(log10(AFR.PA$pa_size))) + 
                                                              mean(log10(AFR.PA$pa_size))))

AFR.forest.slopes <- slopes(AFR.simp.v1, 
                            newdata = datagrid(country_dummy = 0, buffer.forest.prop.z = AFR.buff.forest),
                            exclude = c("s(x.z,y.z)", "s(ISO3)"),
                            type = "response", variables = "buffer.forest.prop.z",
                            by = "buffer.forest.prop.z", conf_level = 0.95)

AFR.forest.slopes <- AFR.forest.slopes %>% mutate(buffer.forest = (buffer.forest.prop.z*sd(AFR.PA$buffer.forest.prop)) + 
                                                    mean(AFR.PA$buffer.forest.prop))

## SEA
SEA.comp <- comparisons(SEA.simp.v1, newdata = datagrid(country_dummy = 0),
                        variables = list(pa.type = "pairwise"),
                        exclude = c("s(ISO3)", "s(x.z, y.z)"),
                        type = "response", conf_level = 0.95)

SEA.area.slopes <- slopes(SEA.simp.v1, 
                          newdata = datagrid(country_dummy = 0, PA.area.logz = SEA.area),
                          exclude = c("s(x.z,y.z)", "s(ISO3)"),
                          type = "response", variables = "PA.area.logz",
                          by = "PA.area.logz", conf_level = 0.95)

SEA.area.slopes <- SEA.area.slopes %>% mutate(pa.area = 10^((PA.area.logz*sd(log10(SEA.PA$pa_size))) + 
                                                              mean(log10(SEA.PA$pa_size))))

SEA.forest.slopes <- slopes(SEA.simp.v1, 
                            newdata = datagrid(country_dummy = 0, buffer.forest.prop.z = SEA.buff.forest),
                            exclude = c("s(x.z,y.z)", "s(ISO3)"),
                            type = "response", variables = "buffer.forest.prop.z",
                            by = "buffer.forest.prop.z", conf_level = 0.95)

SEA.forest.slopes <- SEA.forest.slopes %>% mutate(buffer.forest = (buffer.forest.prop.z*sd(SEA.PA$buffer.forest.prop)) + 
                                                    mean(SEA.PA$buffer.forest.prop))

## Comparison plotting ---------------------------------------------------------

LAM.type.contr <- ggplot(LAM.comp, aes(contrast, estimate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Contrast") +
  ylab("Difference") +
  ggtitle("Americas") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12, face = "bold"))

LAM.area.contr <- ggplot(LAM.area.slopes, aes(pa.area, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope") +
  xlab(expression("PA area ("*km^2*")")) +
  scale_x_log10() +
  theme_bw()


LAM.forest.contr <- ggplot(LAM.forest.slopes, aes(buffer.forest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope") +
  xlab("Proportion of \nforest within 50km") +
  theme_bw()

## AFR
AFR.type.contr <- ggplot(AFR.comp, aes(contrast, estimate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Contrast") +
  ylab("Difference") +
  ggtitle("Africa") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12, face = "bold"))

AFR.area.contr <- ggplot(AFR.area.slopes, aes(pa.area, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope") +
  xlab(expression("PA area ("*km^2*")")) +
  scale_x_log10(breaks = c(10000000, 1000000000, 100000000000),
                # convert to km
                labels = c("10", "1000", "100,000")) +
  theme_bw()


AFR.forest.contr <- ggplot(AFR.forest.slopes, aes(buffer.forest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope") +
  xlab("Proportion of \nforest within 50km") +
  theme_bw()

## SEA
SEA.type.contr <- ggplot(SEA.comp, aes(contrast, estimate)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Contrast") +
  ylab("Difference") +
  ggtitle("Asia-Pacific") +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(size = 12, face = "bold"))

SEA.area.contr <- ggplot(SEA.area.slopes, aes(pa.area, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope") +
  xlab(expression("PA area ("*km^2*")")) +
  scale_x_log10() +
  theme_bw()


SEA.forest.contr <- ggplot(SEA.forest.slopes, aes(buffer.forest, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = .2,
              linetype = "dashed", colour = NA) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope") +
  xlab("Proportion of \nforest within 50km") +
  theme_bw()

attr.contr.plt <- ggarrange(LAM.type.contr,LAM.area.contr, LAM.forest.contr, 
                            AFR.type.contr,AFR.area.contr, AFR.forest.contr,
                            SEA.type.contr,SEA.area.contr, SEA.forest.contr, 
                            ncol = 3, nrow = 3, align = "hv",
                            labels = c("a", "b","c", "d", "e", "f", "g", "h", "i"))

ggsave(path = paste0(data.path,"Outputs/Figures/Spat.QB.2026"), 
       filename = "PA.attr.contr.v2.png",
       attr.contr.plt, bg = "white",
       device = "png", width = 25, height = 20, units = "cm")
