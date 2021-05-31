library(tidyverse)
library(sf)
library(ggmap) # downloading raster maps from a variety of sources
library(ggspatial) # map backgrounds and annotations for ggplot
library(tmap) # static/interactive map library with ggplot-like syntax
library(osmdata) # obtaining OpenStreetMap vector data
library(units) # working with units
library(mapview) # interactive geometry viewing
library(ggiraph)
library(ggplot2)
library(leaflet)



setwd("C:\\Git\\BridgeCapacities\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\")
fname<- "Strassennetz.dbf"
sn <- st_read(fname, layer = 'Strassennetz')

fname<- "Verkehrslagesegmente.dbf"
segmente <- st_read(fname)

fname<- "EVIS_Fokusstreckennetz.dbf"
focus <- st_read(fname)


setwd("C:\\Git\\BridgeCapacities\\Data\\Brücken\\")
fname<- "Bruecken_Point_14042021.dbf"
bridgesP <- st_read(fname)









# Filter only active bridges

z = st_join(sn, st_zm(bridgesP), st_is_within_distance, dist = units::set_units(10, m),  suffix = c(".road", ".bridge"),left = TRUE) 
z %>% select(FEATURENAM,OBJECTID.road, BR_NAME, OBJECTID.bridge, MAXLAST) %>%filter(!is.na(OBJECTID.bridge))
bide<-z%>%select(OBJECTID.bridge) %>%filter(!is.na(OBJECTID.bridge)) %>% unique()
bridgeIds<-unique(bide$OBJECTID.bridge)
BridgesP_AS <- bridgesP %>% filter(OBJECTID  %in% bridgeIds)



# plot with ggplot()

ggplot()+
  geom_sf(data = st_transform(sn, crs=3857)[1], color="red")+
  geom_sf(data = segmente, color="green")+
  geom_sf(data = focus, color="yellow")+
geom_sf(data=BridgesP_AS, color="blue", size=1, shape=1)


# Plot with leaflet


leaflet() %>% 
  addTiles() %>%
  # addProviderTiles("Stamen.Toner") %>% 
  addPolylines(data = st_transform(sn, crs = 4326), fill = NA, color = "red") %>% 
  addPolylines(data = st_transform(segmente, crs = 4326), fill = NA, color = "green") %>% 
  addPolylines(data = st_transform(focus, crs = 4326), fill = NA, color = "blue") %>% 
  addCircleMarkers(data = st_transform(BridgesP_AS,crs = 4326), color = "yellow", radius = 3, opacity=1) %>%
addCircleMarkers(data = st_transform(bridgesP,crs = 4326), color = "orange", radius = 3, opacity=1)


# http://postgis.net/workshops/postgis-intro/spatial_relationships.html

