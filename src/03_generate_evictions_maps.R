# This script aims at generating some insightful maps on eviction distribution and density
# Last section includes density maps over the years

# Settings --------------------------------------------------------------------------------------------------------
source("utils/utils.R")
library(ggmap)
library(RColorBrewer)
library(stringi)
setwd("..")
dataPath = "data/"


# First map -------------------------------------------------------------------------------------------------------

geo = readRDS(paste0(dataPath, "EvictionCases_geocodefirstpass.RDS"))
geo$anyJudgement = geo$amount != 0

map = get_stamenmap(bbox = c(left = -97.25, bottom = 32.5, right = -96.5, top = 33), 
                    zoom = 10, maptype = "toner")
ggmap(map) + 
    geom_point(aes(cxy_lon, cxy_lat), alpha = 0.01, data = geo, color = "red") +
    facet_grid(cols = vars(anyJudgement)) +
    theme_void()


# Basic map -------------------------------------------------------------------------------------------------------

evictionsDt <- fread(paste0(dataPath, "clean/Evictions_wtract.csv"))

map <- get_stamenmap(bbox = c(left = -97, bottom = 32.5, right = -96.5, top = 33), zoom = 10, 
                     maptype = "toner-lite")
ggmap(map) + 
    geom_point(aes(D_x, D_y), alpha = 0.01, data = evictionsDt[Year_Filed == 17], color = cpalPalette[3]) +
    theme_void() +
    theme(plot.title = element_text(size = 20),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)) +
    labs(title = "Eviction records (2019)")

# ggsave(paste0("figures/2019_evictions.png"),
#        width = 15, height = 9, units = 'cm')


# New 2020 map ----------------------------------------------------------------------------------------------------

evictionsDt <- fread(paste0(dataPath, "clean/eviction_clean.csv"))

map <- get_stamenmap(bbox = c(left = -97, bottom = 32.5, right = -96.5, top = 33), zoom = 10, 
                     maptype = "toner-lite")
ggmap(map) + 
    geom_point(aes(d.x, d.y), alpha = 0.01, data = evictionsDt[year.filed == 20], color = cpalPalette[3]) +
    theme_void() +
    theme(plot.title = element_text(size = 20),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12)) +
    labs(title = "Eviction records (2020)")

# ggsave(paste0("figures/2020_evictions.png"),
#        width = 15, height = 9, units = 'cm')


# Evictions heat map ----------------------------------------------------------------------------------------------

evictionsDt <- fread(paste0(dataPath, "clean/eviction_clean.csv"))

map <- get_stamenmap(bbox = c(left = -97, bottom = 32.5, right = -96.5, top = 33), 
                    zoom = 10, maptype = "toner-lite")

for (currYear in c(17, 18, 19)) {
    print(currYear)
    
    ggmap(map) + 
        stat_density_2d(aes(d.x, d.y, fill = stat(level)), contour = T, contour_var = "density",
                        data = evictionsDt[year.filed == currYear],
                        geom = "polygon",
                        alpha = .07, binwidth = 0.5) + 
        scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"), name = "Eviction filings \nper unit area") +
        theme_void() +
        theme(plot.title = element_text(size = 20),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 12)) +
        labs(title = paste0("Eviction records (20", currYear, ")"))
    
    # ggsave(paste0("figures/20", currYear, "_evictions_heatmap.png"),
    #        width = 15, height = 9, units = 'cm')
}
