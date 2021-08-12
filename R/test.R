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
library(htmlwidgets)


DISTANCE <- units::set_units(10, m)
# Set working directory to location of git
setwd("C:\\Git\\BridgeCapacities\\")

source(".\\R\\helperFunctions.R")

# Relative File Paths
roadnetwork_file <- ".\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\Strassennetz.dbf"
bridges_file <- ".\\Data\\Brücken\\Bruecken_Point_14042021.dbf"
focus_file <- ".\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\EVIS_Fokusstreckennetz.dbf"
segmente_file <- ".\\Data\\Verkehrsnetz Kärnten\\Verkehrsnetz inkl. Autobahnen\\Verkehrslagesegmente.dbf"
asfinag_file <- ".\\Data\\Brücken\\Bruecken_Asfinag.csv"



# Boundary

bound.land_kaernten <- getbb("Kärnten, Austria",
                                     featuretype = "state",
                                     format_out = "sf_polygon"
)[1, 1]

bound.veneto <- getbb("Veneto, Italy",
                             featuretype = "state",
                             format_out = "sf_polygon"
)[1, 1]


bound.fruili <- getbb("Friuli, Italy",
                      featuretype = "state",
                      format_out = "sf_polygon"
)

bound.texas <- getbb("McLennan County, Texas, USA",
                      featuretype = "settelement",
                      format_out = "sf_polygon"
)


### Roads from OSM

highway.motorway<- query(key="highway", value="motorway", bbox=bound.land_kaernten)
highway.trunk<- query(key="highway", value="trunk", bbox=bound.land_kaernten)

highway.primary<- query(key="highway", value="primary", bbox=bound.land_kaernten)
highway.secondary<- query(key="highway", value="secondary", bbox=bound.land_kaernten)
highway.tertiary<- query(key="highway", value="tertiary", bbox=bound.land_kaernten)

# tunnel<- query(key="tunnel", value=available_tags("tunnel"), bbox=bound.land_kaernten)
# 
# bridges.Veneto<- query(key="bridge", value=available_tags("bridge"), bbox=bound.veneto)
# bridges.Veneto.filtered <- bridges.Veneto$osm_lines %>% select(osm_id, name, highway, maxweight, maxweight.signed, maxweight.source, maxweightrating)
# bridges.Veneto.filtered %>%filter(!is.na(maxweight))  %>%filter(highway=="motorway")

View(bridges.Veneto.filtered %>%filter(!is.na(maxweight)) )
bridges.all- query(key="bridge", value=available_tags("bridge"), bbox=bound.land_kaernten)

bridges.limited.osm<- bridges.all$osm_lines %>%  filter(!is.na(maxweight))

# Load Files
roadnetwork <- st_read(roadnetwork_file, layer = "Strassennetz")
focus <- st_read(focus_file)
segmente <- st_read(segmente_file)
bridgesP <- st_read(bridges_file)


bridges.limited.land<- bridgesP %>%  filter(!is.na(MAXLAST))


bridgesAsfinag <- as_tibble(read.csv2(asfinag_file)) %>%
  rename(longitude = WGS_X) %>%
  rename(latitude = WGS_Y)


# filter Bridges next to "roadnetwork"
z <- st_join(roadnetwork, st_zm(bridgesP), st_is_within_distance, dist = DISTANCE, suffix = c(".road", ".bridge"), left = TRUE)
z %>%
  select(FEATURENAM, OBJECTID.road, BR_NAME, OBJECTID.bridge, MAXLAST) %>%
  filter(!is.na(OBJECTID.bridge))
bide <- z %>%
  select(OBJECTID.bridge) %>%
  filter(!is.na(OBJECTID.bridge)) %>%
  unique()
bridgeIds <- unique(bide$OBJECTID.bridge)
BridgesP_AS <- bridgesP %>% filter(OBJECTID %in% bridgeIds)

nrow(bridgesP %>% filter(!is.na(MAXLAST)))


# plot with ggplot()
ggplot() +
  geom_sf(data = st_transform(roadnetwork, crs = 3857)[1], color = "red") +
  geom_sf(data = segmente, color = "green") +
  geom_sf(data = focus, color = "yellow") +
  geom_sf(data = BridgesP_AS, color = "blue", size = 1, shape = 1)


# Plot with leaflet
f<- leaflet() %>%
  # addProviderTiles("BasemapAT.grau") %>%
  # addProviderTiles("Stamen.TerrainBackground") %>%
  # addProviderTiles("Stamen.TonerBackground") %>%
  addTiles() %>%
  setView(14.3122, 46.636, zoom = 9) %>%
  addMeasure() %>%
  addScaleBar() %>%
  addPolygons(data = bound.land_kaernten, fillOpacity = 0.3, weight = 1.2, color = "#444444", smoothFactor = 0.5) %>%
  addPolylines(data = st_transform(roadnetwork, crs = 4326), fill = NA, color = "red") %>%
  addPolylines(data = st_transform(segmente, crs = 4326), fill = NA, color = "green") %>%
  addPolylines(data = st_transform(focus, crs = 4326), fill = NA, color = "blue") %>%
  addCircleMarkers(data = st_transform(BridgesP_AS, crs = 4326), color = "yellow", radius = 3, opacity = 1) %>%
  addCircleMarkers(
    data = st_transform(bridgesP, crs = 4326), color = "orange", radius = 3, opacity = 0.8, 
    popup = paste0(bridgesP$BR_NAME, ": ", as.character(bridgesP$MAXLAST)),
    labelOptions = labelOptions(noHide = TRUE, offset = c(0, -12), textOnly = TRUE)
  ) %>%
  addCircleMarkers(
    data = bridgesAsfinag, color = "green", radius = 3, opacity = 0.8,
    popup = paste0(bridgesAsfinag$Objektbezeichnung, ": ", as.character(bridgesAsfinag$Lastbeschraenkung_qualitativ)),
    labelOptions = labelOptions(noHide = TRUE, offset = c(0, -12), textOnly = TRUE)
  )




bridges
#


# Plot with leaflet
roads<- leaflet() %>%
  # addProviderTiles("BasemapAT.grau") %>%
  # addProviderTiles("Stamen.TerrainBackground") %>%
  # addProviderTiles("Stamen.TonerBackground") %>%
  addTiles() %>%
  setView(14.3122, 46.636, zoom = 9) %>%
  addMeasure() %>%
  addScaleBar() %>%
  addPolygons(data = bound.land_kaernten, fillOpacity = 0.3, weight = 1.2, color = "#444444", smoothFactor = 0.5) %>%
  addPolylines(data = highway.motorway$osm_lines, fill = NA, color = "gray", opacity=0.9) %>%
  addPolylines(data = highway.trunk$osm_lines, fill = NA, color = "gray", opacity=0.9) %>%
  addPolylines(data = highway.primary$osm_lines, fill = NA, color = "gray", opacity=0.9) %>%
  addPolylines(data = highway.secondary$osm_lines, fill = NA, color = "gray", opacity=0.9)  %>%
  addPolylines(data = bridges.all$osm_lines, fill = NA, color = "blue", opacity=0.9)  %>%
  addPolylines(data=bridges.limited.osm, color="red", fill="red",opacity=0.9, label=as.character(bridges.limited.osm$maxweight)) %>%
  addCircleMarkers(data= bridges.limited.land, color="red", label=as.character(bridges.limited.land$MAXLAST))





saveWidget(roads,  "roads.html",  selfcontained = TRUE)




# http://postgis.net/workshops/postgis-intro/spatial_relationships.html
# https://stackoverflow.com/questions/43463150/print-label-on-circle-markers-in-leaflet-in-rshiny
# http://leaflet-extras.github.io/leaflet-providers/preview/
