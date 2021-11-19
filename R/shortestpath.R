library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(TSP)
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
library(tidyverse)

# https://rallydatajunkie.com/visualising-rally-stages/working-with-route-data.html
# http://switchfromshapefile.org/

bbox <- getbb("Kärnten, Austria",
  featuretype = "state",
  format_out = "sf_polygon"
)[1, 1]

bbox.veneto <- getbb("Veneto, Italy",
              featuretype = "state",
              format_out = "sf_polygon"
)[1, 1]
bbox.friuli<- getbb("Friuli-Venezia Giulia, Italy",
                     featuretype = "state",
                     format_out = "sf_polygon"
)

poly <- leaflet() %>%
  addTiles()%>%
  addPolygons(data=bbox) %>%
  addPolygons(data=bbox.veneto) %>%
  addPolygons(data=bbox.friuli$multipolygon) 
  
  
# 
# 
# 
# ########### 1.  Retrieve Road Network, clean, and store locally ###################
# bbox_to_string(bbox)
# # https://wiki.openstreetmap.org/wiki/Key:highway
# # dput(available_tags("highway"))
# tags <- c(
#   "motorway", "trunk", "primary", "secondary", "tertiary", "unclassified",
#   "motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link"
# )
# 
# # st_read
# # st_write
# ########## 1. Get road network
# query <- opq(bbox = bbox, timeout = 500) %>%
#   add_osm_feature(key = "highway", value = tags)
# features <- osmdata_sf(query)
# features<- trim_osmdata(features, bbox)
# 
# ## Select a
# not_all_na <- function(x) any(!is.na(x))
# not_any_na <- function(x) all(!is.na(x))
# network<- features$osm_lines %>% select(where(not_all_na))


# Write to file
# st_write(network, "osm_road_network_carinthia.gpkg", driver = "gpkg")



########### 2. Load Network from file, Convert to Graph represenation ###################



network<- st_read("osm_road_network_carinthia.gpkg")

net <- as_sfnetwork(network, directed = TRUE) 
  # st_transform(3035) %>%

### Set weights  
net <- net %>%   activate("edges") %>%  mutate(weight = edge_length())

## Check lengths
net %>%   activate("edges") %>% select(weight)

node.source=1
node.destination=12334

paths <- st_network_paths(net, from = node.source, to = node.destination,   type = "shortest")



#
# paths %>%
#   slice(1) %>%
#   pull(node_paths) %>%
#   unlist()
#
#
# paths %>%
#   slice(1) %>%
#   pull(edge_paths) %>%
# unlist()




##########
selected_edges <-
  paths %>%
  pull(edge_paths) %>%
  unlist()


selected_route <- net %>%
  activate("edges") %>%
  st_as_sf() %>%
  slice(selected_edges)


length <- st_length(selected_route) %>% sum()

#######
nodes.source.geom <- net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(node.source)

nodes.destination.geom <- net %>%
  activate("nodes") %>%
  st_as_sf() %>%
  slice(node.destination)
#########




## Show on Map

# http://leaflet-extras.github.io/leaflet-providers/preview/index.html
icoLst <- awesomeIconList(
  start = makeAwesomeIcon(text = fa("play"), markerColor = "green", squareMarker = FALSE),
  finish = makeAwesomeIcon(text = fa("flag-checkered"), markerColor = "red", squareMarker = FALSE)
)

roads <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner, group = "Stamen") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "StamenLite") %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Gray") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satelite") %>%
    addTiles(group = "OSM") %>%
  addTiles() %>%
  addMeasure() %>%
  addScaleBar() %>%
  addPolylines(data = network %>% filter(highway %in% c("motorway", "trunk", "motorway_link", "trunk_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "motorway") %>%
  addPolylines(data = network %>% filter(highway %in% c("secondary", "secondary_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "secondary") %>%
  addPolylines(data = network %>% filter(highway %in% c("primary", "primary_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "primary") %>%
  addPolylines(data = network %>% filter(highway %in% c("tertiary", "tertiary_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "tertiary") %>%
  addPolylines(data = network %>% filter(highway %in% c("unclassified")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "unclassified") %>%
  addPolylines(data = network, fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "network") %>%
  
  addPolylines(data = selected_route, fillOpacity = 0.9, weight = 5, color = "red", smoothFactor = 0.5, group = "path") %>%
  addAwesomeMarkers(data = nodes.source.geom, icon = icoLst$start, group = "start/end") %>%
  addAwesomeMarkers(data = nodes.destination.geom, icon = icoLst$finish, group = "start/end") %>%
  addLayersControl(
    baseGroups = c("OSM", "Stamen", "StamenLite" ,"Gray", "Satelite"),
    overlayGroups = c("motorway", "primary", "secondary", "tertiary", "unclassified",  "path", "start/end"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup("motorway") %>%
hideGroup("primary") %>%
hideGroup("secondary") %>%
hideGroup("tertiary") %>%
hideGroup("unclassified")


saveWidget(roads, "path.html", selfcontained = TRUE)




# https://cran.r-project.org/web/packages/sfnetworks/vignettes/routing.html

