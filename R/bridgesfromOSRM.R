install.packages('Rcpp')
install.packages("osmdata")
library(Rcpp)
library(osmdata)
library(leaflet)
library(sf)
library(tmap)


q2 <- opq('Carinthia, Austria') %>%
  add_osm_feature(key = 'bridge', value = 'yes')
bridge <- osmdata_sf(q2)
osm_lines <- bridge$osm_lines
names(osm_lines$geometry) <- NULL
leaflet(osm_lines) %>%
  addTiles %>%
  addPolylines()




bounding_polygon <- getbb("Carinthia, Austria",
                      featuretype = "state",
                      format_out = "polygon")



leaflet(bounding_polygon) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5)
			  
			  
leaflet() %>%
  addTiles %>%
  addPolygons(data=bounding_polygon, fillOpacity = 0.7,weight = 1.2, color = "#444444", smoothFactor = 0.5) %>%
  addMeasure() %>%
  addScaleBar() 