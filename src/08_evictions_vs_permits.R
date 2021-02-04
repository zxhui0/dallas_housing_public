library(data.table)
library(ggplot2)
library(rgdal)
library(dplyr)
library(BAMMtools)
library(RColorBrewer)

permits = read_csv("data/permits/permits_data_from_2016_to_202005.csv")
permits =  permits %>% 
    dplyr::group_by(tract, PermitYear)  %>% 
    dplyr::summarise(n =  n())

evictions = as.data.frame(fread("data/clean/evictions_panel.csv"))

tract = readOGR("data/DSSG.gdb", "DallasCounty_Tracts_EvictionLab2015")
tract_geo = fortify(tract, region="GEOID") 
evictions$tract = as.character(evictions$tract)
data = merge(tract_geo, evictions[evictions$year == 2017, ], 
             by.x="id", by.y="tract", all.x=T)
data = merge(data, permits[permits$PermitYear == 2017, ], 
             by.x="id", by.y="tract", all.x=T)
data =  data[order(data$id, data$piece, data$order),  ]

cols = brewer.pal(9, "YlGn")
diff = getJenksBreaks(permits$n[permits$PermitYear == 2017], 8)
ggplot(data, aes(long,lat,group=group, fill=n)) + # the data
    geom_polygon() + # make polygons
    scale_fill_gradientn(limits=c(min(diff), max(diff)), colors=cols, 
                         values=scales::rescale(diff)) + 
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank()) + 
    coord_map("bonne", mean(data$lat)) + labs(fill="# of Permits") 
ggsave("figures/permits_2017.png", width=6, height=5, dpi=200, device="png")


cols = rev(brewer.pal(11, "RdBu"))
diff = getJenksBreaks(evictions$evictions[evictions$year == 2017], 10)
ggplot(data, aes(long,lat,group=group, fill=evictions)) + # the data
    geom_polygon() + # make polygons
    scale_fill_gradientn(limits=c(min(diff), max(diff)), colors=cols, 
                         values=scales::rescale(diff)) + 
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank()) + 
    coord_map("bonne", mean(data$lat)) + labs(fill="# of Eviction \nFilings") 
ggsave("figures/evictions_2017.png", width=6, height=5, dpi=200, device="png")


d = merge(evictions,  permits, by.x=c("tract", "year"), 
          by.y=c("tract", "PermitYear"), all.x=T)
ggplot(d, aes(n, evictions)) + 
    geom_point(alpha=0.15) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    theme_anne(size=14) + 
    xlab("Number of Permits") + ylab("Number of Eviction Filings")
ggsave("figures/permits_vs_evictions_2017.png", width=6, height=5, dpi=200, device="png")
