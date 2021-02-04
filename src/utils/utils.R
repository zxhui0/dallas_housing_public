# Useful, generic functions


# Packages --------------------------------------------------------------------------------------------------------
library(data.table)
library(jsonlite)
library(rgdal)
library(rvest)
library(sf)
library(textreadr)
library(yaml)

# for plotting
library(ggplot2)
library(ggthemes)


# Parameters ------------------------------------------------------------------------------------------------------
cpalPalette <- c("#04879E", "#EDCF4B", "#ED3994", "#ABCF38")


# Functions -------------------------------------------------------------------------------------------------------
CalculateDistanceWords <- function(wordA, wordB) {
    return(stringdist(wordA, wordB))
}

ChangeWordIfSimilar <- function(candidateName, properName, percAccep = 0.1) {
    
    if (is.na(candidateName)) {
        return(candidateName)
    }
    
    nCharsAccept <- ceiling(nchar(properName) * percAccep)
    
    if (CalculateDistanceWords(candidateName, properName) <= nCharsAccept) {
        # cat("Word", candidateName, "changed by", properName, "\n")
        return(properName)
    } 
    
    return(candidateName)
}

LoadConfigFile <- function(env='dev') {
    return(OpenYamlFile('config.yaml')[[env]])
}

OpenYamlFile <- function(file) {
    return(read_yaml(file))
}

prepareForGeocode = function(string) {
    string = stri_replace(string, replacement="", regex="#.*[0-9]*.*$") # remove apt num
    string = stri_replace(str = string, replacement = "", regex = "(-[0-9]+\\s)")
    string = stri_replace(str = string, replacement = "", regex = "[,//.]") # remove , .
    string = sub("\\s+$", "", string) # remove trailing whitespace
    string = str_replace_all(string = string, pattern = "\\s|,", replacement = "+")
    return(string)
}

# Sourced and moderately adapted from 
# https://towardsdatascience.com/geocoding-tableau-and-r-integration-c5b32dc0eda6
geocode <- function(request){
    # API url
    src_url <- "https://nominatim.openstreetmap.org/search?q="
    request_final = paste0(src_url, request, "&format=geojson")

    # Query the API + transform JSON to list 
    response = tryCatch( 
        {read_html(request_final) %>%
                html_node("p") %>%
                html_text() %>%
                fromJSON()},
        error = function(e) {
            read_html(request_final) %>% 
                fromJSON()
        })
    
    # get the lat/lon
    lon = response$features$geometry$coordinates[[1]][1]
    lat = response$features$geometry$coordinates[[1]][2]
    type = response$features$properties$type
        
    if(is.null(lon)) {lon = NA}
    if(is.null(lat)) {lat = NA}
        
    # CREATE A COORDINATES DATAFRAME
    loc <- tibble(address = str_replace_all(request, "%2C", ","),
                          latitude = lat, longitude = lon, type = type)
    return(loc)
}


theme_anne = function(font="Avenir", size=10) {
    theme_tufte(base_size=size, base_family=font) %+replace% 
        theme(
            panel.background  = element_blank(),
            plot.background = element_rect(fill="transparent", colour=NA), 
            axis.line.x = element_line(color="black", size = .2), 
            axis.line.y = element_line(color="black", size = .2), 
            plot.title = element_text(hjust = 0.5)
        )
}


LoadLayer <- function(dataSource, layerName, layerType='geometry') {
    
    if (layerType == 'dataset') {
        data <- sf::st_read(dsn = geoFile, layer = layerName)
        data <- setDT(data)
    } else {
        data <- readOGR(dsn = geoFile, layer = layerName)   
    }
    
    return(data)
}


SubstringRight <- function(s, n, from='right'){
    substr(s, nchar(s) - n + 1, nchar(s))
}
