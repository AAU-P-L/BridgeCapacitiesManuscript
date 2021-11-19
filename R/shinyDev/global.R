library(dplyr)
library(fontawesome)
library(leaflet)
library(sf)
library(sfnetworks)


source("shortestPath.R")
network <- st_read("data/osm_road_network_carinthia.gpkg")

sites <- data.frame(longitude = c(13.94441, 14.44728), latitude = c(46.62183,46.84006))
sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")

site.source <- sites[1,]
site.dest <- sites[2,]


# http://leaflet-extras.github.io/leaflet-providers/preview/index.html
icoLst <- awesomeIconList(
  start = makeAwesomeIcon(text = fa("play"), markerColor = "green", squareMarker = FALSE),
  finish = makeAwesomeIcon(text = fa("flag-checkered"), markerColor = "red", squareMarker = FALSE)
)



route<- getShortestPath(network, site.source, site.dest)
