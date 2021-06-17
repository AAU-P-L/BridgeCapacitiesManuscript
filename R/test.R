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


DISTANCE= units::set_units(10, m)
#Set working directory to location of git
setwd("C:\\Git\\BridgeCapacities\\")

# Relative File Paths
roadnetwork_file <- ".\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\Strassennetz.dbf"
bridges_file <- ".\\Data\\Brücken\\Bruecken_Point_14042021.dbf"
focus_file <- ".\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\EVIS_Fokusstreckennetz.dbf"
segmente_file <- ".\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\Verkehrslagesegmente.dbf"


# Load Files
roadnetwork <- st_read(roadnetwork_file, layer = "Strassennetz")
focus <- st_read(focus_file)
segmente <- st_read(segmente_file)
bridgesP <- st_read(bridges_file)


#filter Bridges next to "roadnetwork"
z <- st_join(roadnetwork, st_zm(bridgesP), st_is_within_distance, dist = DISTANCE , suffix = c(".road", ".bridge"), left = TRUE)
z %>%
  select(FEATURENAM, OBJECTID.road, BR_NAME, OBJECTID.bridge, MAXLAST) %>%
  filter(!is.na(OBJECTID.bridge))
bide <- z %>%
  select(OBJECTID.bridge) %>%
  filter(!is.na(OBJECTID.bridge)) %>%
  unique()
bridgeIds <- unique(bide$OBJECTID.bridge)
BridgesP_AS <- bridgesP %>% filter(OBJECTID %in% bridgeIds)



# plot with ggplot()
ggplot() +
  geom_sf(data = st_transform(roadnetwork, crs = 3857)[1], color = "red") +
  geom_sf(data = segmente, color = "green") +
  geom_sf(data = focus, color = "yellow") +
  geom_sf(data = BridgesP_AS, color = "blue", size = 1, shape = 1)


# Plot with leaflet
leaflet() %>%
  # addProviderTiles("BasemapAT.grau") %>%
  # addProviderTiles("Stamen.TerrainBackground") %>%
  addProviderTiles("Stamen.TonerBackground") %>%
      # addTiles() %>%
  setView(14.3122, 46.636, zoom = 9) %>%
  addMeasure() %>%
  addScaleBar() %>%
      addPolylines(data = st_transform(roadnetwork, crs = 4326), fill = NA, color = "red") %>%
      addPolylines(data = st_transform(segmente, crs = 4326), fill = NA, color = "green") %>%
      addPolylines(data = st_transform(focus, crs = 4326), fill = NA, color = "blue") %>%
      addCircleMarkers(data = st_transform(BridgesP_AS, crs = 4326), color = "yellow", radius = 3, opacity = 1) %>%
      addCircleMarkers(
        data =  st_transform(bridgesP, crs = 4326), color = "orange", radius = 3, opacity = 0.8, label = ~ as.character(MAXLAST),
        popup =paste0(bridgesP$BR_NAME, ": ", as.character(bridgesP$MAXLAST)),
        labelOptions = labelOptions(noHide = TRUE, offset = c(0, -12), textOnly = TRUE)
      )


# http://postgis.net/workshops/postgis-intro/spatial_relationships.html
# https://stackoverflow.com/questions/43463150/print-label-on-circle-markers-in-leaflet-in-rshiny
# http://leaflet-extras.github.io/leaflet-providers/preview/
