options(scipen = 999)
library(tidyterra)
library(ggplot2)
library(ggpubr)

#### matching - pre processing ####

## aggregate forest fire data to 1km ##
## function to get area burned/unburned and aggregate up to 50km for plotting
rm(list = ls())
gc()
fire.regions<- c("LAM", "AFR", "SEA-AUS")
for (i in 1:3){
  region.code<- fire.regions[i]
  region.fire.25p<- rast(paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_final.tif"))
  # get burned areas only
  tropics.tyukavina.25p.all.forest.fires.burned<- subst(region.fire.25p, 1:24, 1, others = NA, datatype = 'INT1U', filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_pixels.tif"), overwrite = T)
  tropics.tyukavina.25p.all.forest.fires.burned<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_pixels.tif"))
  # aggregate to 1km
  tropics.tyukavina.25p.all.forest.fires.burned.cells.1km<- aggregate(tropics.tyukavina.25p.all.forest.fires.burned, fact = 33, fun = "sum", na.rm = T, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_cells_1km.tif"), overwrite = T)
  # get unburned areas
  tropics.tyukavina.25p.all.forest.fires.unburned<- subst(region.fire.25p, 0, 1, others = NA, datatype = 'INT1U', filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_unburned_pixels.tif"), overwrite = T)
  tropics.tyukavina.25p.all.forest.fires.unburned<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_unburned_pixels.tif"))
  # aggregate to 1km
  tropics.tyukavina.25p.all.forest.fires.unburned.cells.1km<- aggregate(tropics.tyukavina.25p.all.forest.fires.unburned, fact = 33, fun = "sum", na.rm = T, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_unburned_cells_1km.tif"), overwrite = T)
  # get cellsize
  tropics.tyukavina.25p.all.forest.fires.cellsize<- cellSize(region.fire.25p, mask = T, unit = "ha", filename =paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_cell_size.tif"), overwrite = T)
  # get area burned
  tropics.tyukavina.25p.all.forest.fires.burned.area<- tropics.tyukavina.25p.all.forest.fires.burned * tropics.tyukavina.25p.all.forest.fires.cellsize
  writeRaster(tropics.tyukavina.25p.all.forest.fires.burned.area, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "forest_fires_2001-24_25p_burned_area.tif"), overwrite = T)
  tropics.tyukavina.25p.all.forest.fires.unburned.area<- tropics.tyukavina.25p.all.forest.fires.unburned * tropics.tyukavina.25p.all.forest.fires.cellsize
  writeRaster(tropics.tyukavina.25p.all.forest.fires.unburned.area, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "forest_fires_2001-24_25p_unburned_area.tif"), overwrite = T)
  # aggregate to 1km
  tropics.tyukavina.25p.all.forest.fires.burned.area.1km<- aggregate(tropics.tyukavina.25p.all.forest.fires.burned.area, fact = 33, fun = "sum", na.rm = T, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_area_1km.tif"), overwrite = T)
  tropics.tyukavina.25p.all.forest.fires.unburned.area.1km<- aggregate(tropics.tyukavina.25p.all.forest.fires.unburned.area, fact = 33, fun = "sum", na.rm = T, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_unburned_area_1km.tif"), overwrite = T)
  tropics.tyukavina.25p.forest.area.1km<- aggregate(tropics.tyukavina.25p.all.forest.fires.cellsize, fact = 33, fun = "sum", na.rm = T, filename = paste0("Data/covariates/", region.code, "_25p_forest_area_2000_1km.tif"), overwrite = T)
  # get prop burned in cell
  tropics.tyukavina.25p.all.forest.fires.prop.burned.15km<- (tropics.tyukavina.25p.all.forest.fires.burned.area.1km / (tropics.tyukavina.25p.all.forest.fires.burned.area.1km+tropics.tyukavina.25p.all.forest.fires.unburned.area.1km))*100
  writeRaster(tropics.tyukavina.25p.all.forest.fires.prop.burned.15km, filename = paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_prop_burned_1km.tif"), overwrite = T)
  
}


### Get sample of 1km PA pixels for matching ###

rm(list = ls())
gc()
fire.regions<- c("LAM", "AFR", "SEA-AUS")
for (i in 1:3){
  region.code<- fire.regions[i]
  print(region.code)
  # load in 1km forest pixels
  region.forest.pixels.1km<- rast(paste0("Data/covariates/", region.code, "_25p_forest_area_2000_1km.tif"))
  # mask for tropical forest biome
  tropical.forest.biome<- vect("Data/biomes/tropical.moist.forest.biome.shp")
  region.forest.pixels.1km<- mask(region.forest.pixels.1km, tropical.forest.biome)
  
  # mask for cells with <33% forest cover
  region.forest.pixels.1km<- clamp(region.forest.pixels.1km, lower = 33, values = F)
  
  ### get inside PA sample ###
  
  # mask for pixels entirely inside PAs
  wdpa.polygons.tmf<- vect("Data/pas/wdpa.polygons.pre.2000.final.shp")
  # crop pas to match region
  wdpa.polygons.tmf.region<- crop(wdpa.polygons.tmf, ext(region.forest.pixels.1km))
  region.pa.forest.pixels.1km<- mask(region.forest.pixels.1km, wdpa.polygons.tmf.region)
  # remove those cells that aren't fully in the PAs
  region.pa.forest.pixels.1km <- mask(region.pa.forest.pixels.1km, as.lines(wdpa.polygons.tmf.region), inverse=T)
  # get 100k sample for matching
  system.time(pa.1km.forest.pixels.random.sample.300k<- spatSample(region.pa.forest.pixels.1km, 250000, na.rm =T, as.df = T, xy = T, method = "random", exhaustive = T))
  # save
  save(pa.1km.forest.pixels.random.sample.300k, file = paste0("Data/matching/samples.250k/", region.code, ".250k.pa.1km.forest.sample.RData"))
  
  ### get outside PA sample ###
  
  # add 10km buffer to pa's - don't take cells from here
  wdpa.polygons.tmf.region.10km.buffer<- vect("Data/pas/wdpa.polygons.all.10km.buffer.shp")
  # crop pas to match region
  wdpa.polygons.tmf.region.10km.buffer<- crop(wdpa.polygons.tmf.region.10km.buffer, ext(region.forest.pixels.1km))
  # remove cells in PAs and 10km buffer
  region.non.pa.forest.pixels.1km <- mask(region.forest.pixels.1km, wdpa.polygons.tmf.region.10km.buffer, inverse=T)
  # get 100k sample for matching
  system.time(non.pa.1km.forest.pixels.random.sample.300k<- spatSample(region.non.pa.forest.pixels.1km, 1500000, na.rm =T, as.df = T, xy = T, method = "random", exhaustive = T))
  # save
  save(non.pa.1km.forest.pixels.random.sample.300k, file = paste0("Data/matching/samples.1500k/", region.code, ".1500k.non.pa.1km.forest.sample.RData"))
  tmpFiles(remove = T)
}





#### Get covariate data ####

### Get covariates for samples ###


### Protect area sample ###

region.areas<- c("LAM", "AFR", "SEA-AUS")
for (j in 1:3){
  
  region.code <- region.areas[j]
  print(region.code)
  
  region.forest.pixels.1km<- rast(paste0("Data/covariates/", region.code, "_25p_forest_area_2000_1km.tif"))
  load(file = paste0("Data/matching/samples.250k/", region.code, ".250k.pa.1km.forest.sample.RData"))
  region.total.sample<- pa.1km.forest.pixels.random.sample.300k
  
  ### Forest in surrounding 5km buffer ###
  print("Surrounding forest 5km")
  surrounding.forest<- rast(paste0("Data/covariates/", region.code, "25p_forest_area_2000_2_focal_window_1km.tif"))
  # extract values for sample #
  extracted.surrounding.forest<- terra::extract(surrounding.forest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.surrounding.forest)[2]<- "surrounding.forest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.surrounding.forest[2])
  
  # region.fire.25p<- rast(paste0("Data/fire.rasters/25.percent.tree.cover/", region.code, "_fire_forest_loss_2001-24_annual_25p_final.tif"))
  # make 5km buffer around point
  #  buffer.forest.list<- list()
  # for (i in 1:nrow(region.total.sample)){
  #print(i)
  #    point<- region.total.sample[i,]
  # make 5km buffer
  #  point<- vect(point, geom=c("x", "y"), crs=crs(region.fire.25p))
  #  point.buffer<- buffer(point, 5000)
  #  buffer.area<- as.numeric(expanse(point.buffer, unit = "ha"))
  #  # get forest cells in 5km buffer
  #  buffer.forest<- crop(region.fire.25p, point.buffer)
  #  buffer.forest<- mask(buffer.forest, point.buffer)
  #  # get area of forest in this 5km buffer
  #  buffer.forest.area<- as.numeric(expanse(buffer.forest, unit = "ha")[2])
  #  buffer.forest.prop<- buffer.forest.area / buffer.area
  #  buffer.forest.df<- data.frame("buffer.forest.area" = buffer.forest.area, "buffer.forest.prop" = buffer.forest.prop)
  #  buffer.forest.list[[i]]<- buffer.forest.df
  #  tmpFiles(remove = T)
  #  }
  
  ### Travel time to urban area ###
  print("Travel time")
  travel.time<- rast("Data/covariates/travel_time_to_cities_12.tif")
  travel.time<- crop(travel.time, region.forest.pixels.1km)
  # extract values for sample #
  extracted.travel<- terra::extract(travel.time, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.travel)[2]<- "travel.time"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.travel[2])
  rm(travel.time)
  
  ## Elevation ##
  print("Elevation")
  
  elevation<- rast("Data/covariates/elevation_1KMmn_SRTM.tif")
  
  # extract values for sample #
  extracted.elevation<- terra::extract(elevation, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.elevation)[2]<- "elevation"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.elevation[2])
  rm(elevation)
  
  ## Slope ##
  print("Slope")
  
  slope<- rast("Data/covariates/slope_1KMmn_SRTM.tif")
  # extract values for sample #
  extracted.slope<- terra::extract(slope, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.slope)[2]<- "slope"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.slope[2])
  rm(slope)
  
  ## Population Density ##
  print("Pop density")
  
  pop.density<- rast("Data/covariates/gpw.2000.2020.mean.tif")
  # extract values for sample #
  extracted.pop.density<- terra::extract(pop.density, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.pop.density)[2]<- "pop.density"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.pop.density[2])
  rm(pop.density)
  
  ## Precipitation ##
  print("precip")
  
  precipitation<- rast("Data/covariates/wc2.1_30s_bio_12.tif")
  # extract values for sample #
  extracted.precipitation<- terra::extract(precipitation, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.precipitation)[2]<- "precipitation"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.precipitation[2])
  rm(precipitation)
  
  ## Precipitation - wettest quarter ##
  print("precip")
  
  precipitation.wettest<- rast("Data/covariates/wc2.1_30s_bio_16.tif")
  # extract values for sample #
  extracted.precipitation.wettest<- terra::extract(precipitation.wettest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.precipitation.wettest)[2]<- "precipitation.wettest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.precipitation.wettest[2])
  rm(precipitation.wettest)
  
  ## Precipitation - driest quarter ##
  print("precip")
  
  precipitation.driest<- rast("Data/covariates/wc2.1_30s_bio_17.tif")
  # extract values for sample #
  extracted.precipitation.driest<- terra::extract(precipitation.driest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.precipitation.driest)[2]<- "precipitation.driest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.precipitation.driest[2])
  rm(precipitation.driest)
  
  ## Temperature ##
  print("temp")
  
  temperature<- rast("Data/covariates/wc2.1_30s_bio_1.tif")
  # extract values for sample #
  extracted.temperature<- terra::extract(temperature, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.temperature)[2]<- "temperature"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.temperature[2])
  rm(temperature)
  
  ## Temperature - hottest quarter ##
  print("temp")
  
  temperature.hottest<- rast("Data/covariates/wc2.1_30s_bio_10.tif")
  # extract values for sample #
  extracted.temperature.hottest<- terra::extract(temperature.hottest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.temperature.hottest)[2]<- "temperature.hottest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.temperature.hottest[2])
  rm(temperature.hottest)
  
  ## FWI 95 PERCENTILE ##
  print("temp")
  
  fwi.95<- rast("Data/covariates/fwi.95th.percentile.2001.2023.tif")
  # extract values for sample #
  extracted.fwi.95<- terra::extract(fwi.95, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.fwi.95)[2]<- "fwi.95"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.fwi.95[2])
  rm(fwi.95)
  
  ## Country ##
  
  print("country")
  
  # intersect and extract state name
  # convert to sf points
  region.total.sample.sf <- st_as_sf(region.total.sample, coords = c("x","y"))
  region.total.sample.sf<- st_set_crs(region.total.sample.sf, 4326)
  region.total.sample.sf<- st_transform(region.total.sample.sf, "ESRI:54009")
  
  gadm.countries<- st_read("Data/gadm_410-levels.gpkg", layer = "ADM_0")
  gadm.countries<- st_transform(gadm.countries, "ESRI:54009")
  #extract values for sample #
  region.total.sample.countries<- (as.numeric(st_intersects(region.total.sample.sf, gadm.countries)))
  region.total.sample$code<- as.factor(gadm.countries$GID_0[as.numeric((region.total.sample.countries))])
  region.total.sample$country<- as.factor(gadm.countries$COUNTRY[as.numeric((region.total.sample.countries))])
  # convert all india related codes
  region.total.sample<- mutate(region.total.sample,code = case_when(code == "Z07" ~ "IND", TRUE ~ code ))
  
  ## Biome ##
  print("biome")
  
  biomes<- st_read("Data/biomes/biome.numbered.ecoregions.shp")
  biomes<- select(biomes, BIOME_NUM)
  biomes<- st_transform(biomes, "ESRI:54009")
  # intersect and extract state name
  # convert to sf points
  region.total.sample$biome<- as.factor(as.numeric(st_intersects(region.total.sample.sf, biomes)))
  
  ## Get outcome variables ##
  
  ## burned area ##
  
  burned.area<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_area_1km.tif"))
  # extract values for sample #
  extracted.burned.area<- terra::extract(burned.area, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.burned.area)[2]<- "burned.area"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.burned.area[2])
  rm(burned.area)
  
  ## burned prop ##
  
  burned.prop<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_prop_burned_1km.tif"))
  # extract values for sample #
  extracted.burned.prop<- terra::extract(burned.prop, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.burned.prop)[2]<- "burned.prop"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.burned.prop[2])
  rm(burned.prop)
  
  # change variable name
  colnames(region.total.sample)[3]<- "forest.area"
  
  ## burned pixels ##
  print("unburned.cells")
  
  unburned.cells<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_unburned_cells_1km.tif"))
  
  # extract values for sample #
  extracted.unburned.cells<- terra::extract(unburned.cells, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.unburned.cells)[2]<- "unburned.cells"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.unburned.cells[2])
  rm(unburned.cells)
  
  ## unburned pixels
  
  print("burned.cells")
  
  burned.cells<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_cells_1km.tif"))
  
  # extract values for sample #
  extracted.burned.cells<- terra::extract(burned.cells, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.burned.cells)[2]<- "burned.cells"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.burned.cells[2])
  rm(burned.cells)
  
  region.total.sample$burned.cells[is.na(region.total.sample$burned.cells)] <- 0
  region.total.sample$unburned.cells[is.na(region.total.sample$unburned.cells)] <- 0
  region.total.sample$total.cells<- region.total.sample$burned.cells+region.total.sample$unburned.cells
  
  save(region.total.sample, file = paste0("Data/matching/samples.plus.covariates/", region.code, ".250k.pa.1km.forest.sample.covariates.RData")) 
}



### Non Protect area sample ###
region.areas<- c("LAM", "AFR", "SEA-AUS")
for (j in 1:3){
  
  region.code <- region.areas[j]
  print(region.code)
  region.forest.pixels.1km<- rast(paste0("Data/covariates/", region.code, "_25p_forest_area_2000_1km.tif"))
  load(file = paste0("Data/matching/samples.1500k/", region.code, ".1500k.non.pa.1km.forest.sample.RData"))
  region.total.sample<- non.pa.1km.forest.pixels.random.sample.300k
  
  ### Forest in surrounding 5km buffer ###
  print("Surrounding forest 5km")
  surrounding.forest<- rast(paste0("Data/covariates/", region.code, "25p_forest_area_2000_2_focal_window_1km.tif"))
  # extract values for sample #
  extracted.surrounding.forest<- terra::extract(surrounding.forest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.surrounding.forest)[2]<- "surrounding.forest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.surrounding.forest[2])
  
  ### Travel time to urban area ###
  print("Travel time")
  travel.time<- rast("Data/covariates/travel_time_to_cities_12.tif")
  travel.time<- crop(travel.time, region.forest.pixels.1km)
  # extract values for sample #
  extracted.travel<- terra::extract(travel.time, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.travel)[2]<- "travel.time"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.travel[2])
  rm(travel.time)
  
  ## Elevation ##
  print("Elevation")
  
  elevation<- rast("Data/covariates/elevation_1KMmn_SRTM.tif")
  
  # extract values for sample #
  extracted.elevation<- terra::extract(elevation, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.elevation)[2]<- "elevation"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.elevation[2])
  rm(elevation)
  
  ## Slope ##
  print("Slope")
  
  slope<- rast("Data/covariates/slope_1KMmn_SRTM.tif")
  # extract values for sample #
  extracted.slope<- terra::extract(slope, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.slope)[2]<- "slope"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.slope[2])
  rm(slope)
  
  ## Population Density ##
  print("Pop density")
  
  pop.density<- rast("Data/covariates/gpw.2000.2020.mean.tif")
  # extract values for sample #
  extracted.pop.density<- terra::extract(pop.density, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.pop.density)[2]<- "pop.density"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.pop.density[2])
  rm(pop.density)
  
  ## Precipitation ##
  print("precip")
  
  precipitation<- rast("Data/covariates/wc2.1_30s_bio_12.tif")
  # extract values for sample #
  extracted.precipitation<- terra::extract(precipitation, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.precipitation)[2]<- "precipitation"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.precipitation[2])
  rm(precipitation)
  
  ## Precipitation - wettest quarter ##
  print("precip")
  
  precipitation.wettest<- rast("Data/covariates/wc2.1_30s_bio_16.tif")
  # extract values for sample #
  extracted.precipitation.wettest<- terra::extract(precipitation.wettest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.precipitation.wettest)[2]<- "precipitation.wettest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.precipitation.wettest[2])
  rm(precipitation.wettest)
  
  ## Precipitation - driest quarter ##
  print("precip")
  
  precipitation.driest<- rast("Data/covariates/wc2.1_30s_bio_17.tif")
  # extract values for sample #
  extracted.precipitation.driest<- terra::extract(precipitation.driest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.precipitation.driest)[2]<- "precipitation.driest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.precipitation.driest[2])
  rm(precipitation.driest)
  
  ## Temperature ##
  print("temp")
  
  temperature<- rast("Data/covariates/wc2.1_30s_bio_1.tif")
  # extract values for sample #
  extracted.temperature<- terra::extract(temperature, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.temperature)[2]<- "temperature"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.temperature[2])
  rm(temperature)
  
  ## Temperature - hottest quarter ##
  print("temp")
  
  temperature.hottest<- rast("Data/covariates/wc2.1_30s_bio_10.tif")
  # extract values for sample #
  extracted.temperature.hottest<- terra::extract(temperature.hottest, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.temperature.hottest)[2]<- "temperature.hottest"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.temperature.hottest[2])
  rm(temperature.hottest)
  
  ## FWI 95 PERCENTILE ##
  print("fwi")
  
  fwi.95<- rast("Data/covariates/fwi.95th.percentile.2001.2023.tif")
  # extract values for sample #
  extracted.fwi.95<- terra::extract(fwi.95, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.fwi.95)[2]<- "fwi.95"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.fwi.95[2])
  rm(fwi.95)
  
  ## Country ##
  
  print("country")
  
  # intersect and extract state name
  # convert to sf points
  region.total.sample.sf <- st_as_sf(region.total.sample, coords = c("x","y"))
  region.total.sample.sf<- st_set_crs(region.total.sample.sf, 4326)
  region.total.sample.sf<- st_transform(region.total.sample.sf, "ESRI:54009")
  
  gadm.countries<- st_read("Data/gadm_410-levels.gpkg", layer = "ADM_0")
  gadm.countries<- st_transform(gadm.countries, "ESRI:54009")
  #extract values for sample #
  region.total.sample.countries<- (as.numeric(st_intersects(region.total.sample.sf, gadm.countries)))
  region.total.sample$code<- as.factor(gadm.countries$GID_0[as.numeric((region.total.sample.countries))])
  region.total.sample$country<- as.factor(gadm.countries$COUNTRY[as.numeric((region.total.sample.countries))])
  # convert all india related codes
  region.total.sample<- mutate(region.total.sample,code = case_when(code == "Z07" ~ "IND", TRUE ~ code ))
  
  ## Biome ##
  #print("biome")
  
  #biomes<- st_read("Data/biomes/biome.numbered.ecoregions.shp")
  #biomes<- select(biomes, BIOME_NUM)
  #biomes<- st_transform(biomes, "ESRI:54009")
  # intersect and extract state name
  # convert to sf points
  #region.total.sample$biome<- as.factor(as.numeric(st_intersects(region.total.sample.sf, biomes)))
  
  ## Get outcome variables ##
  
  ## burned area ##
  
  burned.area<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_area_1km.tif"))
  # extract values for sample #
  extracted.burned.area<- terra::extract(burned.area, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.burned.area)[2]<- "burned.area"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.burned.area[2])
  rm(burned.area)
  
  ## burned prop ##
  
  burned.prop<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_prop_burned_1km.tif"))
  # extract values for sample #
  extracted.burned.prop<- terra::extract(burned.prop, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.burned.prop)[2]<- "burned.prop"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.burned.prop[2])
  rm(burned.prop)
  
  # change variable name
  colnames(region.total.sample)[3]<- "forest.area"
  
  
  ## burned pixels ##
  print("unburned.cells")
  
  unburned.cells<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_unburned_cells_1km.tif"))
  
  # extract values for sample #
  extracted.unburned.cells<- terra::extract(unburned.cells, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.unburned.cells)[2]<- "unburned.cells"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.unburned.cells[2])
  rm(unburned.cells)
  
  ## unburned pixels
  
  print("burned.cells")
  
  burned.cells<- rast(paste0("Data/covariates/1km.forest.cells/", region.code, "_forest_fires_2001-24_25p_burned_cells_1km.tif"))
  
  # extract values for sample #
  extracted.burned.cells<- terra::extract(burned.cells, region.total.sample[1:2], cells=FALSE, method="simple")
  colnames(extracted.burned.cells)[2]<- "burned.cells"
  # bind with other data
  region.total.sample<- cbind(region.total.sample, extracted.burned.cells[2])
  rm(burned.cells)
  
  region.total.sample$burned.cells[is.na(region.total.sample$burned.cells)] <- 0
  region.total.sample$unburned.cells[is.na(region.total.sample$unburned.cells)] <- 0
  region.total.sample$total.cells<- region.total.sample$burned.cells+region.total.sample$unburned.cells
  
  save(region.total.sample, file = paste0("Data/matching/samples.plus.covariates/", region.code, ".non.pa.1km.forest.sample.1500k.covariates.RData")) 
  
  
  
}

#### Matching the samples ####
rm(list = ls())
gc()
## Native and long Plantation ##
region.areas<- c("LAM", "AFR", "SEA-AUS")
for (j in 1:3){
  region.code <- region.areas[j]
  print(region.code)
  
  # load in covariate data
  # pa data
  load(file = paste0("Data/matching/samples.plus.covariates/", region.code, ".250k.pa.1km.forest.sample.covariates.RData")) 
  region.total.sample<- select(region.total.sample, -biome)
  region.total.sample.pa<- mutate(region.total.sample, treatment = "PA")
  # non pa data
  load(file = paste0("Data/matching/samples.plus.covariates/", region.code, ".non.pa.1km.forest.sample.1500k.covariates.RData")) 
  #region.total.sample<- select(region.total.sample, -biome)
  region.total.sample.non.pa<- mutate(region.total.sample, treatment = "Non-PA")
  # put together
  region.total.sample.all<- rbind(region.total.sample.pa, region.total.sample.non.pa)
  region.total.sample.all$treatment<- factor(region.total.sample.all$treatment)
  # convert NA burned area to 0
  region.total.sample.all$burned.area[is.na(region.total.sample.all$burned.area)] <- 0
  region.total.sample.all$burned.prop[is.na(region.total.sample.all$burned.prop)] <- 0
  
  #remove < third forest
  region.total.sample.all<- filter(region.total.sample.all, forest.area>=33 )
  
  # remove countries with > 0.1% burn area
  # get country level burn
  country.level.burn<- region.total.sample.all %>% group_by(country) %>% 
    summarise(mean.burn = mean(burned.prop)) %>% 
    filter(mean.burn >= 0.1)
  country.names<- country.level.burn$country
  # remove samples not in countries with >0.1% burn
  region.total.sample.all<- filter(region.total.sample.all, country %in% country.names)
  unique(region.total.sample.all$country)
  # remove samples with NAs for covariates
  region.total.sample.all<- region.total.sample.all %>% na.omit()
  
  save(region.total.sample.all, file = paste0("Data/matching/pre.matched.data/", region.code, ".pre.matched.data.RData"))
  
  print ("Ready for matching") 
  
  ## PSM - Caliper = 0.2 ##
  print("PSM - 0.2")
  library(MatchIt)
  
  myMatch <- matchit(treatment~forest.area+surrounding.forest+travel.time+elevation+slope+pop.density+precipitation+precipitation.wettest+precipitation.driest+temperature+temperature.hottest+fwi.95, data=region.total.sample.all, exact = 'country', method='nearest', caliper=0.2)
  # Checking balance after full matching
  my.match.summary<- summary(myMatch)
  print(my.match.summary)
  save(my.match.summary, file = paste0("Data/matching/match.summaries/psm/0.2/", region.code, ".match.summary.RData"))
  matched.data <- match.data(myMatch)
  save(matched.data, file = paste0("Data/matching/matched.data/psm/0.2/", region.code, ".matched.data.RData"))
  
  ## PSM - Caliper = 0.5 ##
  
  library(MatchIt)
  print("PSM - 0.5")
  
  myMatch <- matchit(treatment~forest.area+surrounding.forest+travel.time+elevation+slope+pop.density+precipitation+precipitation.wettest+precipitation.driest+temperature+temperature.hottest+fwi.95, data=region.total.sample.all, exact = 'country', method='nearest', caliper=0.50)
  # Checking balance after full matching
  my.match.summary<- summary(myMatch)
  print(my.match.summary)
  save(my.match.summary, file = paste0("Data/matching/match.summaries/psm/0.5/", region.code, ".match.summary.RData"))
  matched.data <- match.data(myMatch)
  save(matched.data, file = paste0("Data/matching/matched.data/psm/0.5/", region.code, ".matched.data.RData"))
  
  ## Mahalanobis - Caliper = 0.2 ##
  print("Mahal - 0.2")
  myMatch <- matchit(treatment~forest.area+surrounding.forest+travel.time+elevation+slope+pop.density+precipitation+precipitation.wettest+precipitation.driest+temperature+temperature.hottest+fwi.95, data=region.total.sample.all, distance = 'glm',exact = 'country',  caliper=0.20,
                     mahvars = ~forest.area+surrounding.forest+travel.time+elevation+slope+pop.density+precipitation+precipitation.wettest+precipitation.driest+temperature+temperature.hottest+fwi.95 )
  # Checking balance after full matching
  my.match.summary<- summary(myMatch)
  print(my.match.summary)
  save(my.match.summary, file = paste0("Data/matching/match.summaries/mahalanobis/0.2/", region.code, ".match.summary.RData"))
  matched.data <- match.data(myMatch)
  save(matched.data, file = paste0("Data/matching/matched.data/mahalanobis/0.2/", region.code, ".matched.data.RData"))
  
  ## Mahalanobis - Caliper = 0.5 ##
  print("Mahal - 0.5")
  
  myMatch <- matchit(treatment~forest.area+surrounding.forest+travel.time+elevation+slope+pop.density+precipitation+precipitation.wettest+precipitation.driest+temperature+temperature.hottest+fwi.95, data=region.total.sample.all, distance = 'glm',exact = 'country',  caliper=0.50,
                     mahvars = ~forest.area+surrounding.forest+travel.time+elevation+slope+pop.density+precipitation+precipitation.wettest+precipitation.driest+temperature+temperature.hottest+fwi.95 )
  # Checking balance after full matching
  my.match.summary<- summary(myMatch)
  print(my.match.summary)
  save(my.match.summary, file = paste0("Data/matching/match.summaries/mahalanobis/0.5/", region.code, ".match.summary.RData"))
  matched.data <- match.data(myMatch)
  save(matched.data, file = paste0("Data/matching/matched.data/mahalanobis/0.5/", region.code, ".matched.data.RData"))
  
}
