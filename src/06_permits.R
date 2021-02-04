# Settings --------------------------------------------------------------------------------------------------------
setwd('Downloads/DSSG/dallas/dallas_housing/src/')

source("utils/utils.R")
library(broom)
library(data.table)
library(rgdal)
library(ggmap)
library(purrr)
library(raster)
library(sp)
library(stringi)


geoFile <- paste0('../data/', 'DSSG.gdb')
fcList <- ogrListLayers(geoFile)


tract = readOGR(geoFile, "Tracts_DallasCounty")




permits_2020 <- LoadLayer(geoFile, "Permits_2020_thruMay", 'geometry')
pmt_20 = spTransform(permits_2020, crs(tract))
pmt_20$tract = over(pmt_20, tract)$GEOID

permits_2019 <- LoadLayer(geoFile, "Permits_2019", 'geometry')
pmt_19 = spTransform(permits_2019, crs(tract))
pmt_19$tract = over(pmt_19, tract)$GEOID



permits_2018 <- LoadLayer(geoFile, "Permits_2018", 'geometry')
pmt_18 = spTransform(permits_2018, crs(tract))
pmt_18$tract = over(pmt_18, tract)$GEOID

permits_2017 <- LoadLayer(geoFile, "Permits_2017", 'geometry')
pmt_17 = spTransform(permits_2017, crs(tract))
pmt_17$tract = over(pmt_17, tract)$GEOID


write.csv(pmt_20@data, "../data/permits/permit_2020_tillMay.csv", row.names=F)
write.csv(pmt_19@data, "../data/permits/permit_2019.csv", row.names=F)
write.csv(pmt_18@data, "../data/permits/permit_2018.csv", row.names=F)
write.csv(pmt_17@data, "../data/permits/permit_2017.csv", row.names=F)
