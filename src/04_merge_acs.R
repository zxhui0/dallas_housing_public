# Merge in ACS

# Settings -------------------------------------------------------------------------------
library(rgdal)
library(raster)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidycensus)
source("utils/utils.R")
source("utils/secure_data.R")
setwd("..")


# ----------------------------------------------------------------------------------------
#  Read in the geocoded data
# ----------------------------------------------------------------------------------------

evictions = readRDS('data/clean/eviction_clean.RDS')


# ----------------------------------------------------------------------------------------
#  Read ACS shapefiles and find cross
# ----------------------------------------------------------------------------------------


get_acs_vars = function(year, api_key, survey="acs5") {
    
    if (year > 2016) {
        vars = c(#pop by race variables
            total_pop="DP05_0033E", white_pop="DP05_0037E", #race vars
            black_pop="DP05_0038E", native_pop="DP05_0039E", 
            asian_pop="DP05_0044E", pacific_pop="DP05_0052PE", 
            other_pop="DP05_0057E", two_race_pop="DP05_0058E", 
            
            #housing related variables 
            tot_housing="DP04_0001E", rental_vacancy="DP04_0005E", 
            median_rent="DP04_0134E", owner_occ="DP04_0046E", 
            renter_occ="DP04_0047E", perc_rent_gt_35="DP04_0142PE", 
            
            #other demograaphic variables
            single_fathers="DP02_0007PE", single_mothers="DP02_0009PE", 
            perc_snap_benefits="DP03_0074PE", perc_poverty_line="DP03_0119PE",
            unemployment_rate="DP03_0009PE", perc_insured="DP03_0096PE",
            
            #income variable
            median_income="DP03_0062E")
    } else {
        vars = c(#pop by race variables
            total_pop="DP05_0028E", white_pop="DP05_0032E", #race vars
            black_pop="DP05_0033E", native_pop="DP05_0034E", 
            asian_pop="DP05_0039E", pacific_pop="DP05_0047E", 
            other_pop="DP05_0052E", two_race_pop="DP05_0053E", 
            
            #housing related variables 
            tot_housing="DP04_0001E", rental_vacancy="DP04_0005E", 
            median_rent="DP04_0134E", owner_occ="DP04_0046E", 
            renter_occ="DP04_0047E", perc_rent_gt_35="DP04_0142PE", 
            
            #other demograaphic variables
            single_fathers="DP02_0007PE", single_mothers="DP02_0009PE", 
            perc_snap_benefits="DP03_0074PE", perc_poverty_line="DP03_0119E",
            unemployment_rate="DP03_0009PE", perc_insured="DP03_0096PE",
            
            #income variable
            median_income="DP03_0062E")
    }

    
    y = get_acs(geography="tract", 
                variables=vars, 
                year=year, output="wide", state="48", county="113", key=api_key, 
                survey=survey)
    
    y = y %>% 
        as.data.frame() %>%
        dplyr::select(GEOID, 
               total_pop, white_pop, black_pop, native_pop, asian_pop, pacific_pop, 
               other_pop, two_race_pop, 
               tot_housing, rental_vacancy, median_rent, owner_occ, renter_occ, 
               perc_rent_gt_35, 
               single_fathers, single_mothers, perc_snap_benefits, perc_poverty_line, 
               unemployment_rate, perc_insured, median_income) %>%
        mutate(year=year)
    return(y)
}

X2015 = get_acs_vars(2015, api_key, "acs5")
X2016 = get_acs_vars(2016, api_key, "acs5")
X2017 = get_acs_vars(2017, api_key, "acs5")
X2018 = get_acs_vars(2018, api_key, "acs5")

acs = rbind(X2015, X2016, X2017, X2018)
acs$year = acs$year + 2


tract_evictions = evictions@data
tract_evictions$year = paste0(20, tract_evictions$year.filed)
tract_evictions = tract_evictions %>% 
    mutate(tract = paste0(state_fips, county, tract)) %>%
    group_by(tract, year) %>% 
    dplyr::summarise(judgements = sum(judgement1!=0), # TODO: are judgements right
                     evictions = n(), 
                     amount = mean(judgement.amt[judgement.amt!=0]), 
                     evicted_white = sum(d.white.perc), 
                     evicted_black = sum(d.black.perc), 
                     evicted_hisp = sum(d.hisp.perc), 
                     evicted_asian = sum(d.asian.perc), 
                     evicted_other = sum(d.other.perc))

#combine eviction data with ACS data
tract_evictions = merge(tract_evictions, acs, by.x=c("tract", "year"), 
                        by.y=c("GEOID", "year"), all.x=T)

#convert columns to numeric
tract_evictions[3:ncol(tract_evictions)] = sapply(tract_evictions[3:ncol(tract_evictions)],
                                                  function(x) {as.numeric(as.character(x))})
tract_evictions = tract_evictions %>%
    mutate(evictions_pc = evictions/total_pop, 
           evictions_pu = evictions/renter_occ, 
           perc_rental = renter_occ/(renter_occ+owner_occ))

write.csv(tract_evictions, "data/clean/evictions_panel.csv")

ggplot(tract_evictions) +
    geom_abline(intercept=0, slope=1, color="black", lwd=0.1) +
    geom_point(aes(white_pop/total_pop,
        evicted_white/(evicted_white+evicted_black+evicted_asian+evicted_other), 
        size = evictions), alpha = 0.35) + 
    xlab("Percent population: white") + 
    ylab("Percent of evictions: white") + theme_anne()
ggsave("figures/acs_percrental_vs_percevict_white.png", width=5, height=4)

ggplot(tract_evictions) +
    geom_abline(intercept=0, slope=1, color="black", lwd=0.1) +
    geom_point(aes(black_pop/total_pop,
        evicted_black/(evicted_white+evicted_black+evicted_asian+evicted_other), 
        size = evictions), alpha = 0.35) + 
    xlab("Percent population: black") + 
    ylab("Percent of evictions: black") + theme_anne()
ggsave("figures/acs_percrental_vs_percevict_black.png", width=5, height=4)

ggplot(tract_evictions) +
    geom_abline(intercept=0, slope=1, color="black", lwd=0.1) +
    geom_point(aes(other_pop/total_pop,
        evicted_other/(evicted_white+evicted_black+evicted_asian+evicted_other), 
        size = evictions), alpha = 0.35) + 
    xlim(0, 1) + ylim(0, 1) + 
    xlab("Percent population: other") + 
    ylab("Percent of evictions: other") + theme_anne()
ggsave("figures/acs_percrental_vs_percevict_others.png", width=5, height=4)
  