
library(tidyverse)
library(terra)
library(tidyterra)
library(rnaturalearth)
data.path <- "X:/morton_research/User/bi1om/Research/Fire/Fire_in_PAs/Analysis/"

# data
load(paste0(data.path,"Outputs/Summaries/Overall/Spat.QB.2025.NC/LAM.raw.comp.spat.2025.QB.RData"))

# tidy into raster
LAM.raw.comp <- LAM.raw.comp %>% 
  mutate(dir = case_when(conf.low < 0 & conf.high < 0 ~ "Increased burn",
                         conf.low > 0 & conf.high > 0 ~ "Decreased burn",
                         .default = "Uncertain"),
         estimate = estimate*-1) %>%
  filter(country == "Brazil")
LAM.xy <- LAM.raw.comp %>% as.data.frame() %>% select(x, y, dir)
LAM.rast <- as_spatraster(LAM.xy)


world <- ne_countries()
#LAM.reg <- world %>% filter(iso_a3 %in% unique(LAM.raw.comp$ISO3))
LAM.reg <- world %>% filter(subregion %in% c("South America", "Central America"))

# plot
ggplot() +
  geom_spatvector(data = LAM.reg, fill = "grey80") +
  geom_spatraster(data = LAM.rast, aes(fill = dir)) +
  scale_fill_manual(values = c("#2166ac", "black", "#b2182b"), 
                    labels = c("Decreased burn", "Uncertain",
                               "Increased burn"),
                    limits = c("Decreased burn", "Uncertain",
                               "Increased burn"), na.value = NA, "ATT") +
  coord_sf(ylim = c(-30, 24)) +
  theme_minimal() +
  theme(legend.position = "bottom")
