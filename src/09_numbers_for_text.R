setwd("..")
source("src/utils/utils.R")
dataPath <- 'data/'

# ----------------------------------------------------------------------------------------
#  Load in necessary data
# ----------------------------------------------------------------------------------------

tract_evictions = fread("data/clean/evictions_panel.csv")

tract = readOGR("data/DSSG.gdb", "DallasCounty_Tracts_EvictionLab2015")
tract_geo = fortify(tract, region="GEOID") 
tract_evictions$tract = as.character(tract_evictions$tract)
data = merge(tract_geo, tract_evictions[tract_evictions$year == 2017, ], 
             by.x="id", by.y="tract", all.x=T)
data = data[order(data$group, data$piece, data$order), ]

# ----------------------------------------------------------------------------------------
#  Get table of tracts with highest evictions
# ----------------------------------------------------------------------------------------

aggregated = tract_evictions %>%
    group_by(tract)  %>%
    summarise_at(vars(evictions:amount,  median_income:perc_rental), mean,  na.rm=T)
cutoff = mean(aggregated$evictions_pc) + sd(aggregated$evictions_pc)*2
top = aggregated[aggregated$evictions_pc >= cutoff,  
           c("tract", "evictions", "evictions_pc", "amount", "median_income")] %>%
    mutate(top = 2, 
           tract = as.character(tract))

cutoff2 = mean(aggregated$evictions_pc)
second = aggregated[aggregated$evictions_pc >= cutoff2 & aggregated$evictions_pu < cutoff ,  
                 c("tract", "evictions", "evictions_pc", "amount", "median_income")] %>%
    mutate(top = 1, 
           tract = as.character(tract))

top = rbind(top, second)

temp_data = merge(data, top,  by.x="id", by.y="tract", all.x=T)
temp_data = temp_data[order(temp_data$group, temp_data$piece, temp_data$order), ]

ggplot(temp_data, aes(long,lat,group=group, fill=top)) + # the data
    geom_polygon() + # make polygons
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(), 
          plot.title=element_blank()) + 
    coord_map("bonne", mean(data$lat)) + labs(fill="") + 
    ggtitle("evictions per unit") + guides(fill=F) 
ggsave("figures/tracts_above_mean_evictions_pu.png", width=5, height=5, units="in")
