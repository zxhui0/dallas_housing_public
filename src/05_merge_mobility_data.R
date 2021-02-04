# This script aims at merging together eviction information with school mobility data

# Settings --------------------------------------------------------------------------------------------------------
source("utils/utils.R")
library(broom)
library(data.table)
library(ggmap)
library(ggnewscale)
library(purrr)
library(raster)
library(RColorBrewer)
library(rgdal)
library(sp)
library(stringi)
setwd("..")

dataPath <- LoadConfigFile()[['data_path']]

# Load complete geo file ------------------------------------------------------------------------------------------

geoFile <- paste0(dataPath, 'DSSG_updated.gdb')
fcList <- ogrListLayers(geoFile)
print(fcList)

# Read mobility data ----------------------------------------------------------------------------------------------

# Read the latest mobility data directly from geoDB
mobilityDt <- LoadLayer(geoFile, "DISD_MobilityData_2016_2017SY", 'dataset')

names(mobilityDt) <- tolower(names(mobilityDt))

# Get SLN code from 
mobilityDt[, ":="(sln = as.numeric(unlist(map(campus, SubstringRight, 3))))]


# Read attendance boundaries --------------------------------------------------------------------------------------

# Read the corresponding attendance boundaries data directly from geoDB
boundaries <- LoadLayer(geoFile, "DISD_AttendanceBoundaries_2016", 'geometry')

boundaries@data$seq_id <- seq(1:nrow(boundaries@data))
names(boundaries@data) <- tolower(names(boundaries@data))

boundaries@data <- merge(boundaries@data, mobilityDt, by = 'sln', all.x = T)

# writeOGR(boundaries, "data/clean/attendance_boundaries_mobility",
#          "mobility_boundaries", "ESRI Shapefile")
# write.csv(boundaries@data, "data/clean/attendance_boundaries_mobility.csv", row.names = F)

boundaries@data$id <- rownames(boundaries@data)

# Transform coordinates so that we can assign mobility data to attendance boundaries
maptools::gpclibPermit()
aux <- tidy(boundaries, region = "id")
utmCoord <- setDT(aux)[, .(long, lat)]
names(utmCoord) <- c('x', 'y')
sp::coordinates(utmCoord) <- ~x+y
raster::crs(utmCoord) <- sp::CRS(boundaries@proj4string@projargs)
longLatCoord <- spTransform(utmCoord, CRS("+proj=longlat +datum=WGS84"))
longLatCoord <- setDT(as.data.frame(coordinates(longLatCoord)))
longLatCoord <- cbind(longLatCoord, aux)
rm(aux)

dt <- merge(longLatCoord, boundaries@data, by = 'id')


# Load evictions --------------------------------------------------------------------------------------------------
evictionsDt <- fread("data/clean/eviction_clean.csv")  # Load evictions from clean panel


# Generate plots --------------------------------------------------------------------------------------------------

# Basic map (all years): Mobility rate information with evictions as points on top of it, from geoDB data
map <- get_stamenmap(bbox = c(left = -97, bottom = 32.5, right = -96.5, top = 33), zoom = 10, 
                     maptype = "toner-lite")
ggmap(map) + 
    geom_polygon(aes(x, y, fill = mobility_rate, group = group), data = dt[!is.na(mobility_rate)], 
                 color = 'white', size = 0.1, alpha = 1) +
    geom_point(aes(d.x, d.y), alpha = 0.01, data = evictionsDt, color = cpalPalette[3]) +
    theme_void() +
    theme(plot.title = element_text(size = 20),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)) +
    scale_fill_gradient(low = cpalPalette[2], high = cpalPalette[1]) +
    labs(title = "Mobility rate by area", fill = "Mobility rate (%)")

# ggsave(paste0("figures/mobility_rate_vs_all_evictions.png"),
#        width = 15, height = 9, units = 'cm')

# Basic map (2017): Mobility rate information with evictions as points on top of it, from geoDB data
# Makes sense to include only 2017 evictions since mobility information corresponds to that year
ggmap(map) + 
    geom_polygon(aes(x, y, fill = mobility_rate, group = group), data = dt[!is.na(mobility_rate)], 
                 color = 'white', size = 0.1, alpha = 1) +
    scale_fill_gradient(low = cpalPalette[2], high = cpalPalette[1]) +
    geom_point(aes(d.x, d.y), alpha = 0.01, data = evictionsDt[year.filed == 17], color = cpalPalette[3]) +
    theme_void() +
    theme(plot.title = element_text(size = 20),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)) +
    
    labs(title = "Mobility rate by area (2016-2017)", fill = "Mobility rate (%)")

# ggsave(paste0("figures/mobility_rate_vs_2017_evictions.png"),
#        width = 15, height = 9, units = 'cm')

# Heat map: Plot together mobility rate and evictions' density
ggmap(map) + 
    geom_polygon(aes(x, y, fill = mobility_rate, group = group), data = dt, 
                 color = 'white', size = 0.1, alpha = 1) +
    scale_fill_gradient(low = cpalPalette[2], high = cpalPalette[1], name = "Mobility rate") +
    new_scale("fill") +
    stat_density_2d(aes(d.x, d.y, fill = stat(level)), contour = T, contour_var = "density",
                    data = evictionsDt[year.filed == 17],
                    geom = "polygon",
                    alpha = .07, binwidth = 0.5) +
    scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"), name = "Eviction filings \nper unit area") +
    theme_void() +
    theme(plot.title = element_text(size = 20),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)) +
    labs(title = "", fill = "Mobility rate (%)")

# ggsave(paste0("figures/mobility_rate_with_gaps_vs_2017_evictions_heatmap.png"),
#        width = 15, height = 11, units = 'cm')


# We aimed at going at a thinner granularity than school boundaries: tract level.
# However, this job is incomplete and could be retaken.

# # Tract level: Incomplete! ----------------------------------------------------------------------------------------
# 
# tractLayer <- readOGR("data/tl_2019_48_tract", "tl_2019_48_tract")
# tractLayer = spTransform(tractLayer, crs(boundaries))
# merged <- over(tractLayer, boundaries)
# 
# merged$tractid = tractLayer$GEOID
# merged = merged[!is.na(merged$objectid), ]
# 
# write.csv2(merged, "data/clean/mobility_tract_level.csv", row.names = F)
# 
# 
# # Compare mobility vs eviction rate -------------------------------------------------------------------------------
# 
# evictionsByTract <- fread("data/clean/evictions_panel.csv")
# evictionsByTract[, ":="(evictionRate = evictions / total_pop * 100)]
# 
# mobilityDt <- fread("data/clean/mobility_tract_level.csv")
# mobilityDt[, ":="(tract = tractid)]
# 
# data <- merge(evictionsByTract, mobilityDt, by = "tract")
# 
# ggmap(map) + 
#     geom_polygon(aes(x, y, fill = mobility_rate), data = data, 
#                  color = 'white', size = 0.1, alpha = 1) +
#     theme_void() +
#     theme(plot.title = element_text(size = 20),
#           legend.title = element_text(size = 16),
#           legend.text = element_text(size = 12)) +
#     scale_fill_gradient(low = cpalPalette[2], high = cpalPalette[1]) +
#     labs(title = "Mobility rate by area (2016-2017)", fill = "Mobility rate (%)")
