# Address normalization

# Settings --------------------------------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(ggplot2)
library(purrr)
library(rapportools)
library(readxl)
library(stringdist)
library(stringr)
library(raster)
library(wru)
library(stats)
library(readr)
library(censusxy)
library(stringi)
setwd("..")
source("src/utils/utils.R")
source("src/utils/secure_data.R") # just contains census api key

dataPath <- 'data/'

# Data retrieval -------------------------------------------------------------------------

fgdb = "data/DSSG.gdb"
evictions = readOGR(fgdb, "EvictionRecords_D", stringsAsFactors = F)
dt = as.data.table(evictions@data)

dt2020 = as.data.frame(read_csv("data/PIAOrduna010120072420.csv"))
dt2020 = dt2020 %>%
    mutate(Year_Filed = substr(`FILED DATE`, 1, 2), 
           Filed_mont = substr(`FILED DATE`, 3, 4),
           Day_Filed = substr(`FILED DATE`, 5, 6), 
           Judgemen_1 = substr(`JUDGMENT DATE`, 1, 2),
           P_NameGrou = `PLAINTIFF NAME`,
           D_x = NA, D_y = NA, P_x = NA, P_y = NA) %>%
    dplyr::select(`CASE NUMBER`, COURT, `FILED DATE`, Year_Filed, Filed_mont, Day_Filed, 
           `PLAINTIFF NAME`, P_NameGrou, `PL ADDRESS`, `PL CITY`, `PL STATE`, `PL ZIP`, 
           `PL PHONE`,
           `DEFENDANT NAME`, `DF ADDRESS`, `DF CITY`, `DF STATE`, `DF ZIP`, `DF PHONE`,
           `JUDGMENT DATE`,  Judgemen_1, AMOUNT, D_x, D_y, P_x, P_y)

dt = rbind(dt, dt2020, use.names=F)


# Fixing cities --------------------------------------------------------------------------

dt$D_City = tolower(dt$D_City)
uniqueCities = c("addison", "allen", "arlington", "austin", "balch springs", "carrollton",
                 "cedar hill", "chicago", "cockrell hill", "coppell", "dallas", "de soto",
                 "duncanville", "farmers branch", "garland", "glenn heights", 
                 "grand prairie", "hutchins", "irving", "lancaster", "mesquite", 
                 "richardson", "rowlett", "sachse", "seagoville", "sunnyvale", 
                 "university park", "wilmer", "wylie")

# Normalizing city names
for (city in uniqueCities) {  # TODO: Optimize this process
    cat("Fixing city", city, "\n")
    dt[, ":="(D_City = unlist(map_chr(D_City, ChangeWordIfSimilar, city)))]    
}

# Quick check of most common cities
#dt[, .(count = .N), by = 'D_City'][order(-count)]

# Fixing defendant address ---------------------------------------------------------------

# Normalizing addresses
regex <- "^(\\d+)*\\s+([a-z\\s\\d]+)*\\s*(#\\s*[a-z\\d]+)*"  
# TODO:
# - Missing when after "#", there is a character, not a number
# - Deal with apartment numbers if no "#" found

abbDict <- c(
    "\\b(av|ave)\\b" = "avenue",
    "\\bblvd\\b" = "boulevard",
    "\\bct\\b" = "court",
    "\\bdr\\b" = "drive",
    "\\bhwy\\b" = "highway",
    "\\bln\\b" = "lane",
    "\\bpkwy\\b" = "parkway",
    "\\brd\\b" = "road", 
    "\\bst\\b" = "street",
    "\\btr\\b" = "trail",
    
    "\\bw\\b" = "west",
    "\\bn\\b" = "north",
    "\\bs\\b" = "south",
    "\\be\\b" = "east",
    "\\bne\\b" = "north east",
    "\\bnw\\b" = "north west",
    "\\bsw\\b" = "south west",
    "\\bse\\b" = "south east"
)

dt$D_Address = tolower(dt$D_Address)
addresses <- copy(dt[, D_Address])

addressesDt <- addresses %>% 
    str_replace_all("[.,;:-]", " ") %>% 
    trimws() %>% 
    str_replace_all(abbDict) %>% 
    str_replace_all("[\\s]+", " ") %>% 
    str_replace_all("\\bapt\\b", "") %>% 
    str_match(regex) %>% 
    data.table()

setnames(addressesDt, names(addressesDt), c("originalDfAddress", "dfStreetNumber", 
                                            "dfStreetName", "dfApartmentNumber"))

addressesDt[, ":="(treatedDfAddress = paste0(addressesDt[,dfStreetNumber], " ", 
                                             addressesDt[,dfStreetName]))]

output <- cbind(dt, addressesDt)

# Fixing plaintiff addresses -------------------------------------------------------------

output$P_Address = tolower(output$P_Address)
addresses <- copy(output[, P_Address])

addressesDt <- addresses %>% 
    str_replace_all("[.,;:-]", " ") %>% 
    trimws() %>% 
    str_replace_all(abbDict) %>% 
    str_replace_all("[\\s]+", " ") %>% 
    str_replace_all("\\bapt\\b", "") %>% 
    str_match(regex) %>% 
    data.table()

setnames(addressesDt, names(addressesDt), c("originalPAddress", "pStreetNumber", 
                                            "pStreetName", "pApartmentNumber"))

addressesDt[, ":="(treatedpAddress = paste0(addressesDt[,pStreetNumber], " ", 
                                             addressesDt[,pStreetName]))]

output <- cbind(output, addressesDt)


# Fixing plaintiff names -----------------------------------------------------------------

abbDict <- c(abbDict, "llc"="", "inc"="", "ltd"="", "llp"="", "dba"="", "(dha)"="", 
             "lp"="")
output$P_Name = output$P_Name %>%
    tolower %>%
    str_replace_all(abbDict) %>% 
    str_replace_all("-", "") %>%
    str_replace_all("[()/.,]", "") %>%
    str_replace_all("[\\s]+", " ") %>%
    trim.space()

freq = table(output$P_Name)
freq = freq[order(freq)]

for (i in 1:length(freq)) {
    if (names(freq)[i] == "")  {next}
    dist = stringdist(names(freq)[i], names(freq))
    dist = data.frame(dist = dist, freq = freq)
    diff = ceiling(nchar(names(freq)[i])*.15)
    dist = dist[dist$dist<diff, ]
    dist = dist[dist$freq.Freq == max(dist$freq.Freq),]
    fuzz_match = dist$freq.Var1[dist$dist == min(dist$dist)]
    output$P_Name[output$P_Name == names(freq)[i]] = fuzz_match
    if (names(freq)[i] != fuzz_match) {
        print(names(freq)[i])
        print(fuzz_match)
        print("------------------------")
    }
    if (i%%1000==0) {print(i)}
}

# Cleaning columns generally -------------------------------------------------------------

output = output %>% 
    dplyr::select(Case, Court, Filed_Date, Year_Filed, Filed_mont, Day_Filed, 
                  
                  P_x, P_y, P_Name, P_NameGrou, pStreetNumber, pStreetName,
                  pApartmentNumber, treatedpAddress, P_City, P_State, P_ZIP,
                  
                  D_x, D_y, D_Name, dfStreetNumber, dfStreetName, dfApartmentNumber, 
                  treatedDfAddress, D_City, D_State, D_Zip,
                  
                  Judgement_, Judgemen_1, Amount)

names(output) = c("case", "court", "date.filed", "year.filed", "month.filed", "day.filed", 
                  "p.x", "p.y", "p.name", "p.namegroup", "p.num", "p.street", "p.apt", 
                  "p.address", "p.city", "p.state", "p.zip", "d.x", "d.y", "d.name", 
                  "d.num", "d.street", "d.apt", "d.address", "d.city", "d.state", "d.zip", 
                  "judgement1", "judgement2", "judgement.amt")

output$d.name = sapply(output$d.name, 
                       function(y) {y = strsplit(y, " ")[[1]]; y = y[y!=""]; 
                       y = paste(y[2], y[1])})
trim = c("name", "num", "street", "apt", "address", "city", "state")
trim = c(paste0("p.", trim), paste0("d.", trim))
output = data.frame(output)
output[, trim] = apply(output[, trim], 2, str_trim)
num = c("d.num", "p.num", "d.x", "d.y", "p.x", "p.y")
for (i in 1:length(num)) {output[, num[i]] = as.numeric(output[, num[i]])}


# Geocode the judgements in 2020 ---------------------------------------------------------

geo = output[is.na(output$d.x), 
             c("d.num", "d.street", "d.address", "d.city", "d.state", "d.zip")]
geo = unique(geo)

geo = cxy_geocode(geo, street="d.address", city="d.city", state="d.state", zip="d.zip", 
                  return="locations", class="dataframe", parallel=4)

nas = which(is.na(geo$cxy_lat))
nas = unique(geo[nas, c("d.address", "d.city", "d.state", "d.zip")])

# slow, about 20 minutes
nas = tidygeocoder::geocode(nas, street=d.address, city=d.city, state=d.state, 
                            postalcode=d.zip, method="census", verbose=T) #1.5 minutes
nas[is.na(nas$lat),] = tidygeocoder::geocode(nas[is.na(nas$lat), 1:4], street=d.address, 
                                             city=d.city, state=d.state, postalcode=d.zip, 
                                             method="osm", verbose=F, lat=lat) #17 minutes
nas = nas[, c("d.address", "d.city", "d.state", "d.zip", "long", "lat")]
geo = geo[, 3:8]
names(nas) = names(geo)

geo = rbind(geo[!is.na(geo$cxy_lon), ], nas, use.names=F)

output = merge(output, geo, by=c("d.address", "d.city", "d.state", "d.zip"), all.x=T)
output$d.x[is.na(output$d.x)] = output$cxy_lon[is.na(output$d.x)] 
output$d.y[is.na(output$d.y)] = output$cxy_lat[is.na(output$d.y)] 
output = output %>% 
    dplyr::select(-cxy_lon, -cxy_lat)


# Cleaning names of plaintiff ------------------------------------------------------------

p.names = unique(output$p.namegroup)
p.dist = as.dist(stringdistmatrix(p.names, p.names)) #5 minutes
hc = hclust(p.dist) 
x = cutree(hc, h = 2.9)
p.names = data.frame(name=p.names, group=x)

p.names.unique = p.names[!duplicated(p.names$group), ]
p.names.unique$p.clean.name = gsub("\\s+", " ", p.names.unique$name) 
p.names = merge(p.names, p.names.unique[, c("group", "p.clean.name")], by="group")
output = merge(output, p.names[, c("name", "p.clean.name")], by.x="p.name", by.y="name")
rm("p.names", "p.names.unique", "p.dist"); gc()


# Save data ------------------------------------------------------------------------------

saveRDS(output, paste0(dataPath, 'clean/eviction_clean_table.RDS'))


# Geolocate to tract level ---------------------------------------------------------------

output = output %>% filter(!is.na(d.x))
points = SpatialPoints(output[, c("d.x", "d.y")]) #convert to spatial
points = SpatialPointsDataFrame(points, output)

fgdb = "data/DSSG.gdb"
tract = readOGR(fgdb, "Tracts_DallasCounty")

crs(points) = crs(tract)
o = over(points, tract)
points$tract = o$TRACTCE
points$county = o$COUNTYFP
points$state_fips = o$STATEFP
points = points[!is.na(points$tract), ]

# Use name to impute race of defendent  --------------------------------------------------

names = points[, c("d.name", "d.state", "county", "tract")]
names$state = "tx"
names$surname = sapply(names$d.name, function(x){tolower(strsplit(x, " ")[[1]][2])})

abbDict = c("\\b(st)\\b" = "", "\\b(unlimited)\\b" = "", "\\b(ltd)\\b" = "")
names$surname = names$surname %>% 
    str_replace_all("[.,;:0-9]", " ") %>% 
    str_replace_all(abbDict) %>% 
    trimws()

imputed = predict_race(names, census.geo="tract", census.key=api_key, retry=2) #8 minutes
points = points[order(points$d.name), ]
imputed = imputed[order(imputed$d.name), ]
points$d.white.perc = imputed$pred.whi
points$d.black.perc = imputed$pred.bla
points$d.hisp.perc = imputed$pred.his
points$d.asian.perc = imputed$pred.asi
points$d.other.perc = imputed$pred.oth


# Save data ------------------------------------------------------------------------------

saveRDS(points, paste0(dataPath, 'clean/eviction_clean.RDS'))
