library(terra)
library(sf)
sf_use_s2(F)
library(dplyr)
options(scipen = 999)
library(tidyterra)
library(ggplot2)
library(ggpubr)

#### Generating fire inside PA data from Tyukavina et al. fire data and WDPA ####

#### Pre 2000 - Get overlap with fire - Tyukavina et al. ####
tmpFiles(remove = T)
rm(list = ls())
gc()
fire.regions<- c("LAM", "AFR", "SEA-AUS")
for (i in 1:3){
  fire.regions<- c("LAM", "AFR", "SEA-AUS")
  # load in fire data
  region.code<- fire.regions[i]
  print(region.code)
  region.fire<- rast(paste0("Data/fire.rasters/", region.code, "_fire_forest_loss_2001-24_annual.tif"))
  region.fire.25p<- rast(paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_final.tif"))
  #region.fire.25p<- crop(region.fire.25p, region.fire, filename = paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_cropped.tif"), datatype = 'INT1U', overwrite = T)
  #region.fire.25p<- rast(paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_cropped.tif"))
  
  # mask by PAs
  wdpa.polygons.tmf<- vect("Data/pas/wdpa.polygons.pre.2000.final.shp")
  # crop pas to match region
  wdpa.polygons.tmf.region<- crop(wdpa.polygons.tmf, ext(region.fire))
  region.fires.in.pas<- mask(region.fire.25p, wdpa.polygons.tmf.region, filename = paste0("Data/fire.rasters/", region.code, "_tropical_forest_pas_forest_fires_2001-24_25p.tif"), datatype = 'INT1U', overwrite = T)
  region.fires.in.pas<- rast(paste0("Data/fire.rasters/", region.code, "_tropical_forest_pas_forest_fires_2001-24_25p.tif"))
  #plot(region.fires.in.pas)
  # get annual fire in PAs
  region.annual.pa.fire.area<- expanse(region.fires.in.pas, unit = "ha", byValue = T)
  # get total forest area in pas (burned + unburned)
  total.forest.area<- sum(region.annual.pa.fire.area$area)
  # get area of PAs
  #region.pa.area<- sum(expanse(wdpa.polygons.tmf.region, unit = "ha"))
  # get % burned in each year
  region.annual.pa.fire.area<- mutate(region.annual.pa.fire.area, prop.burned = (area/total.forest.area)*100)
  save(region.annual.pa.fire.area, file = paste0("Data/pa.burned.area.dfs/", region.code, "pa.total.burned.areas.25p.PAs.RData"))
  rm(list = ls())
  gc()
  tmpFiles(remove = T)
}

#### All (including post 2000) - Get overlap with fire - Tyukavina et al. ####
tmpFiles(remove = T)
rm(list = ls())
gc()
fire.regions<- c("LAM", "AFR", "SEA-AUS")
for (i in 1:3){
  fire.regions<- c("LAM", "AFR", "SEA-AUS")
  # load in fire data
  region.code<- fire.regions[i]
  print(region.code)
  region.fire<- rast(paste0("Data/fire.rasters/", region.code, "_fire_forest_loss_2001-24_annual.tif"))
  region.fire.25p<- rast(paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_final.tif"))
  #region.fire.25p<- crop(region.fire.25p, region.fire, filename = paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_cropped.tif"), datatype = 'INT1U', overwrite = T)
  #region.fire.25p<- rast(paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_cropped.tif"))
  
  # mask by PAs
  wdpa.polygons.tmf<- vect("Data/pas/wdpa.polygons.all.final.shp")
  # crop pas to match region
  wdpa.polygons.tmf.region<- crop(wdpa.polygons.tmf, ext(region.fire))
  region.fires.in.pas<- mask(region.fire.25p, wdpa.polygons.tmf.region, filename = paste0("Data/fire.rasters/", region.code, "_tropical_forest_ALL_pas_forest_fires_2001-24_25p.tif"), datatype = 'INT1U', overwrite = T)
  region.fires.in.pas<- rast(paste0("Data/fire.rasters/", region.code, "_tropical_forest_ALL_pas_forest_fires_2001-24_25p.tif"))
  #plot(region.fires.in.pas)
  # get annual fire in PAs
  region.annual.pa.fire.area<- expanse(region.fires.in.pas, unit = "ha", byValue = T)
  # get total forest area in pas (burned + unburned)
  total.forest.area<- sum(region.annual.pa.fire.area$area)
  # get area of PAs
  #region.pa.area<- sum(expanse(wdpa.polygons.tmf.region, unit = "ha"))
  # get % burned in each year
  region.annual.pa.fire.area<- mutate(region.annual.pa.fire.area, prop.burned = (area/total.forest.area)*100)
  save(region.annual.pa.fire.area, file = paste0("Data/pa.burned.area.dfs/", region.code, "pa.total.burned.areas.25p.ALL.PAs.RData"))
  rm(list = ls())
  gc()
  tmpFiles(remove = T)
}

#### raterise PA network by PA year to mask out fires that existed before PA ####
tmpFiles(remove = T)
rm(list = ls())
gc()
fire.regions<- c("LAM", "AFR", "SEA-AUS")
for (i in 1:3){
  fire.regions<- c("LAM", "AFR", "SEA-AUS")
  # load in fire data
  region.code<- fire.regions[i]
  print(region.code)
  region.fires.in.pas<- rast(paste0("Data/fire.rasters/", region.code, "_tropical_forest_ALL_pas_forest_fires_2001-24_25p.tif"))
  # mask by PAs
  wdpa.polygons.tmf<- vect("Data/pas/wdpa.polygons.all.final.shp")
  # crop pas to match region
  wdpa.polygons.tmf.region<- crop(wdpa.polygons.tmf, ext(region.fires.in.pas))
  # rasterise by year
  wdpa.polygons.tmf.region.year.raster<- rasterize(wdpa.polygons.tmf.region, region.fires.in.pas, field = "STATUS_YR",fun = min, filename = paste0("Data/pas/", region.code, ".PA.year.rasterized.tif"), overwrite = T)
  
  library(terra)
  region.fires.in.pas<- rast(paste0("Data/fire.rasters/", region.code, "_tropical_forest_ALL_pas_forest_fires_2001-24_25p.tif"))
  
  region.fires.in.pas.after.PA.establishment <- lapp(c(region.fires.in.pas, wdpa.polygons.tmf.region.year.raster), fun = function(Layer_1, STATUS_YR) {
    ifelse((Layer_1 + 2000) >= STATUS_YR | Layer_1 == 0, Layer_1, NA)
  }, filename = paste0("Data/fire.rasters/", region.code, "_tropical_forest_ALL_pas_forest_fires_after_PA_establishment_2001-24_25p.tif"), overwrite = T)
  region.fires.in.pas.after.PA.establishment<- rast(paste0("Data/fire.rasters/", region.code, "_tropical_forest_ALL_pas_forest_fires_after_PA_establishment_2001-24_25p.tif"))
  # get annual fire in PAs
  region.annual.pa.fire.area<- expanse(region.fires.in.pas.after.PA.establishment, unit = "ha", byValue = T)
  # get total forest area in pas (burned + unburned)
  total.forest.area<- sum(region.annual.pa.fire.area$area)
  # get area of PAs
  #region.pa.area<- sum(expanse(wdpa.polygons.tmf.region, unit = "ha"))
  # get % burned in each year
  region.annual.pa.fire.area<- mutate(region.annual.pa.fire.area, prop.burned = (area/total.forest.area)*100)
  save(region.annual.pa.fire.area, file = paste0("Data/pa.burned.area.dfs/", region.code, "pa.total.burned.areas.after.pa.establishment.25p.RData"))
  rm(list = ls())
  gc()
  tmpFiles(remove = T)
}

load(file = paste0("Data/pa.burned.area.dfs/LAMpa.total.burned.areas.after.pa.establishment.25p.RData"))



