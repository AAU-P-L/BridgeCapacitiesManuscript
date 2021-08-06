library(Rcpp)
library(osmdata)
library(leaflet)
library(sf)
library(tmap)
library(dplyr)

### Functions
query <- function(key, value, bbox) {
  query <- opq(bbox = bbox) %>%
    add_osm_feature(key = key, value = value)
  features <- osmdata_sf(query)
  features <- trim_osmdata(features, bb_poly = bb <- as(bbox, "Spatial"))
}


convert_poly_to_points <- function(sf_polygons) {
  polys_as_points <-
    sf_polygons %>%
    st_centroid()
  # st_geometry()
  return(polys_as_points)
}


as_points_only <- function(features) {
  points <- features$osm_points
  
  polys <- features$osm_poly
  if(nrow(polys) >0){
    convertToPoints <- convert_poly_to_points(polys)
    
    return(bind_rows(points, convertToPoints))
  }
  
  return(points)
}

filter_NAs <- function(features, colName) {
  if (nrow(features) > 0) {
    features <- features %>% filter(!is.na(eval(as.name(colName))))
  }
  
  return(features)
}

addToLeafLet <- function(f, key, filterTags) {
  feature <- filter_NAs(as_points_only(query(key = key, value = available_tags(key), bbox = bbox)), colName = key)
  if (nrow(feature) > 0) {
    feature<- feature %>% filter(!eval(as.name(key)) %in% filterTags) 
  }
  if (nrow(feature) > 0) {
    f <- f %>% addMarkers(data = feature, popup = ~ as.character(osm_id), label = ~ as.character(eval(as.name(key))))
  }
  return(f)
}
