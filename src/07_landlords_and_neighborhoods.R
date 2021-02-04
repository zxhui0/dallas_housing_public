# learning about neighborhood level info

# Settings -------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(purrr)
library(stringr)
library(ggmap)
library(raster)
setwd("..")
source("src/utils/utils.R")

dataPath <- 'data/'
evictions = readRDS(paste0(dataPath, 'clean/eviction_clean.RDS'))

landlords = as.data.frame(table(evictions$p.clean.name))

perc_owned = function(x) {
    x = quantile(landlords$Freq, 1-x)
    x = sum(landlords$Freq[landlords$Freq > x])/sum(landlords$Freq)
    return(x)
}

# ----------------------------------------------------------------------------------------
# Landlord level plots
# ----------------------------------------------------------------------------------------

# ARE THE MAJORITY OF EVICTIONS COMING FROM A SMALL % OF LANDLORDS?

perc_owned(.05) #the top 5% of landlords are responsible for 55% of evictions
perc_owned(.01) #the top 1% of landlords are responsible for 25% of evictions


# DO THEY EVICT AT DIFFERENT RATES OR IS IT JUST THAT OWNERSHIP FOLLOWS THAT DIST?

# ARE THE "SMALL" LANDLORDS DIFFERENTLY LOCATED THAN THE "BIG" LANDLORDS?

map = get_stamenmap( bbox = c(left=-97.25, bottom=32.5, right=-96.5, top=33), zoom=10, maptype="toner")
rows = landlords$Var1[landlords$Freq > quantile(landlords$Freq, 0.99)]
rows = evictions$p.clean.name %in% rows
ggmap(map) + 
    geom_point(aes(d.x, d.y), color="red", alpha=0.05, data=evictions@data[rows, ]) +
    theme_void()

rows = landlords$Var1[landlords$Freq < quantile(landlords$Freq, 0.99)]
rows = evictions$p.clean.name %in% rows
ggmap(map) + 
    geom_point(aes(d.x, d.y), color="blue", alpha=0.007, data=evictions@data[rows, ]) +
    theme_void()

# ARE THE "SMALL" LANDLORDS DIFFERENTLY LOCATED THAN THE "BIG" LANDLORDS?


# ----------------------------------------------------------------------------------------
#  Create tract level plots
# ----------------------------------------------------------------------------------------

tract_evictions <- fread("data/clean/evictions_panel.csv")

# Percent of units that are rental VS evictions PC---------------------------------------
ggplot(tract_evictions, aes(perc_rental, evictions_pc)) + 
    geom_point(alpha=0.2, color=cpalPalette[1]) + 
    geom_smooth(se=F, method="lm", color=cpalPalette[3], lwd=0.8) + 
    xlab("Percentage of units that are rental") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave("figures/acs_percrental_vs_evictionspc.png", width=5, height=4, dpi=200, device="png")


# Tract population VS evictions PC-------------------------------------------------------
ggplot(tract_evictions, aes(total_pop, evictions_pc)) + 
    geom_point(alpha=0.2, color=cpalPalette[1]) + 
    geom_smooth(se=F, method="lm", color=cpalPalette[3], lwd=0.8) + 
    xlab("Population of tract") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2)
ggsave("figures/acs_tractpop_vs_evictionspc.png", width=5, height=4, dpi=200, device="png")

# Tract rent VS evictions PC-------------------------------------------------------------
ggplot(tract_evictions, aes(median_rent, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F,  method="lm") + 
    xlab("Median rent") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave(paste0("figures/acs_median_rent_vs_evictionspc.png"), width=5, height=4, device="png")

# Tract income VS evictions PC-----------------------------------------------------------
ggplot(tract_evictions, aes(median_income, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm") + 
    xlab("Median income") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave(paste0("figures/avg_income_vs_evictionspc.png"), width=5, height=4, device="png")

# rental vacancy rate VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(rental_vacancy, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("Rental Vacancy Rate") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2)
ggsave("figures/acs_rentalvacancy_vs_evictionspc.png", width=5, height=4, device="png")

# rental cost as % of income VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(perc_rent_gt_35, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("% spending >35% of income on rent") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave("figures/acs_rentalover35_vs_evictionspc.png", width=5, height=4, device="png")

# single fathers % VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(single_fathers, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("% Single Fathers") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) + xlim(0, 36)
ggsave("figures/acs_singlefathers_vs_evictionspc.png", width=5, height=4, device="png")

# single mothers % VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(single_mothers, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("% Single Mothers") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) + xlim(0, 36)
ggsave("figures/acs_singlemothers_vs_evictionspc.png", width=5, height=4, device="png")


# unemployment rate VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(unemployment_rate, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("Unemployment Rate") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave("figures/acs_unemploymentrate_vs_evictionspc.png", width=5, height=4, device="png")

# under pov line VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(perc_poverty_line, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("% under poverty line") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave("figures/acs_povertyline_vs_evictionspc.png", width=5, height=4, device="png")

# SNAP VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(perc_snap_benefits, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("Percentage with SNAP benefits") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave("figures/acs_SNAP_vs_evictionspc.png", width=5, height=4, device="png")

# % insured VS evictions PC----------------------------------------------------
ggplot(tract_evictions, aes(perc_insured, evictions_pc)) + 
    geom_point(alpha=0.2, color = cpalPalette[1]) + 
    geom_smooth(color = cpalPalette[3], se=F, method="lm", lwd=0.8) + 
    xlab("Percentage insured") + ylab("Evictions Per Capita") + 
    theme_anne(size=14) + ylim(0, 0.2) 
ggsave("figures/acs_insured_vs_evictionspc.png", width=5, height=4, device="png")


# ----------------------------------------------------------------------------------------
#  Create maps for these data
# ----------------------------------------------------------------------------------------

tract = readOGR("data/DSSG.gdb", "DallasCounty_Tracts_EvictionLab2015")
tract_geo = fortify(tract, region="GEOID") 
tract_evictions$tract = as.character(tract_evictions$tract)
data = merge(tract_geo, tract_evictions[tract_evictions$year == 2017, ], 
             by.x="id", by.y="tract", all.x=T)
data = data[order(data$group, data$piece, data$order), ]

cols = rev(brewer.pal(11, "RdBu"))

# Map of evictions PU --------------------------------------------------------------------
diff = c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.2, 0.4, 0.6)
ggplot(data, aes(long,lat,group=group, fill=evictions_pc)) + # the data
    geom_polygon() + # make polygons
    scale_fill_gradientn(limits=c(min(diff), max(diff)), colors=cols, 
                         values=scales::rescale(diff)) + 
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank()) + 
    coord_map("bonne", mean(data$lat)) + labs(fill="") + 
    ggtitle("Evictions Per Capita")


# Map of judgement amount ----------------------------------------------------------------
diff = c(quantile(tract_evictions$amount, seq(0, 1, length.out=11), na.rm=T))
ggplot(data, aes(long,lat,group=group, fill=amount)) + # the data
    geom_polygon() + # make polygons
    scale_fill_gradientn(limits=c(min(diff), max(diff)), colors=cols, 
                         values=scales::rescale(diff)) + 
    theme(line = element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank()) + 
    coord_map("bonne", mean(data$lat)) + labs(fill="") + 
    ggtitle("mean amount of judgement")

# Map of % evictions filed resulting in judgements ---------------------------------------
diff = c(quantile(tract_evictions$judgements/tract_evictions$evictions, 
                  seq(0, 1, length.out=11), na.rm=T))
ggplot(data, aes(long,lat,group=group, fill=judgements/evictions)) + # the data
    geom_polygon() + # make polygons
    scale_fill_gradientn(limits=c(min(diff), max(diff)), colors=cols, 
                         values=scales::rescale(diff)) + 
    theme(text=element_text(size=20),
          line=element_blank(),  # remove the background, tickmarks, etc
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.background = element_blank()) + 
    coord_map("bonne", mean(data$lat)) + labs(fill="") + ggtitle("% of evictions granted")


# Look at the tracts where there are way more evictions than units -----------------------
# Are there a bunch of motels there? -----------------------------------------------------

tracts_high = unique(tract_evictions$tract[tract_evictions$evictions_pc > 0.5])

evictions$fips = paste0(evictions$state_fips, evictions$county, evictions$tract)
evictions_high = evictions[evictions$fips %in% tracts_high, ]


# ----------------------------------------------------------------------------------------
#  Get demographic tables for the neighborhoods that have low or high eviction
# ----------------------------------------------------------------------------------------

tract_evictions = fread("data/clean/evictions_panel.csv")

aggregated = tract_evictions %>%
    group_by(tract)  %>%
    summarise_at(vars(evictions:perc_rental), mean,  na.rm=T)

cutoff_high = quantile(aggregated$evictions_pc,  0.85)
cutoff_low = quantile(aggregated$evictions_pc,  0.15)

aggregated = aggregated %>%
    mutate(low = ifelse(evictions_pc < cutoff_low, 1, 0),  
           high = ifelse(evictions_pc > cutoff_high, 2, 0), 
           low = ifelse(high != 0, high, low)) %>%
    group_by(low) %>%
    summarise_at(vars(amount:perc_rental), mean,  na.rm=T) %>%
    select(-evicted_white, -evicted_black, -evicted_hisp, -evicted_asian, -evicted_other)

