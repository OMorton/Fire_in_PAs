## no forest area
system.time(LAM_mod4.5 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                                s(x.z, y.z, k = 100) + 
                                s(travel.time.z, bs = "cr", k = 20) +
                                s(elevation.logz, bs = "cr", k = 20) +
                                s(slope.logz, bs = "cr", k = 20) +
                                s(pop.density.logz, bs = "cr", k = 20) +
                                s(precipitation.wettest.z, bs = "cr", k = 20) +
                                s(precipitation.driest.z, bs = "cr", k = 20) +
                                s(temperature.hottest.z, bs = "cr", k = 20) +
                                s(fwi.95.z, bs = "cr", k = 50) +
                                #s(forest.area.logz, by = treatment, bs = "cr", k = 20)+
                                s(ISO3, bs='re', by = country_dummy) +
                                s(ISO3, treatment, bs='re', by = country_dummy),
                              data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod4.5, "Outputs/Models/LAM.Mod4.5.Full.BAM.rds")

system.time(LAM_mod5 <- bam(cbind(burned.cells, non.burned.cells) ~ 1 + treatment +
                              s(x.z, y.z, k = 100) + 
                              s(travel.time.z, bs = "cr", k = 50) +
                              s(elevation.logz, bs = "cr", k = 50) +
                              s(slope.logz, bs = "cr", k = 50) +
                              s(pop.density.logz, bs = "cr", k = 50) +
                              s(precipitation.wettest.z, bs = "cr", k = 50) +
                              s(precipitation.driest.z, bs = "cr", k = 50) +
                              s(temperature.hottest.z, bs = "cr", k = 50) +
                              s(fwi.95.z, bs = "cr", k = 50) +
                              s(forest.area.logz, by = treatment, bs = "cr", k = 50)+
                              s(ISO3, bs='re', by = country_dummy) +
                              s(ISO3, treatment, bs='re', by = country_dummy),
                            data = LAM.matched.data.z, family = binomial()))
saveRDS(LAM_mod5, "Outputs/Models/LAM.Mod5.Full.BAM.rds")
summary(LAM_mod5)


system.time(LAM_mod_pois1 <- bam(burned.cells~ 1 + treatment +
                                   s(x.z, y.z, k = 50) + 
                                   s(travel.time.z, bs = "cr", k = 20) +
                                   s(elevation.logz, bs = "cr", k = 20) +
                                   s(slope.logz, bs = "cr", k = 20) +
                                   s(pop.density.logz, bs = "cr", k = 20) +
                                   s(precipitation.wettest.z, bs = "cr", k = 20) +
                                   s(precipitation.driest.z, bs = "cr", k = 20) +
                                   s(temperature.hottest.z, bs = "cr", k = 20) +
                                   s(fwi.95.z, bs = "cr", k = 20) +
                                   s(total.cells.z, bs = "cr", k = 20)+
                                   s(ISO3, bs='re', by = country_dummy) +
                                   s(ISO3, treatment, bs='re', by = country_dummy),
                                 data = LAM.matched.data.z, family = poisson()))
saveRDS(LAM_mod_pois1, "Outputs/Models/LAM.Mod.pois1.Full.BAM.rds")
summary(LAM_mod_pois1)

system.time(LAM_mod_pois2 <- bam(burned.cells~ 1 + treatment +
                                   s(x.z, y.z, k = 50) + 
                                   s(travel.time.z, bs = "cr", k = 20) +
                                   s(elevation.logz, bs = "cr", k = 20) +
                                   s(slope.logz, bs = "cr", k = 20) +
                                   s(pop.density.logz, bs = "cr", k = 20) +
                                   s(precipitation.wettest.z, bs = "cr", k = 20) +
                                   s(precipitation.driest.z, bs = "cr", k = 20) +
                                   s(temperature.hottest.z, bs = "cr", k = 20) +
                                   s(fwi.95.z, bs = "cr", k = 20) +
                                   s(total.cells.z, by = treatment, bs = "cr", k = 20)+
                                   s(ISO3, bs='re', by = country_dummy) +
                                   s(ISO3, treatment, bs='re', by = country_dummy),
                                 data = LAM.matched.data.z, family = poisson()))
saveRDS(LAM_mod_pois2, "Outputs/Models/LAM.Mod.pois2.Full.BAM.rds")
summary()

system.time(LAM_mod_pois3 <- bam(burned.cells~ 1 + treatment +
                                   s(x.z, y.z, k = 50) + 
                                   s(travel.time.z, bs = "cr", k = 20) +
                                   s(elevation.logz, bs = "cr", k = 20) +
                                   s(slope.logz, bs = "cr", k = 20) +
                                   s(pop.density.logz, bs = "cr", k = 20) +
                                   s(precipitation.wettest.z, bs = "cr", k = 20) +
                                   s(precipitation.driest.z, bs = "cr", k = 20) +
                                   s(temperature.hottest.z, bs = "cr", k = 20) +
                                   s(fwi.95.z, bs = "cr", k = 20) +
                                   s(ISO3, bs='re', by = country_dummy) +
                                   s(ISO3, treatment, bs='re', by = country_dummy) +
                                   offset(log(total.cells)),
                                 data = LAM.matched.data.z, family = poisson()))
saveRDS(LAM_mod_pois3, "Outputs/Models/LAM.Mod.pois3.Full.BAM.rds")
summary(LAM_mod_pois3)
