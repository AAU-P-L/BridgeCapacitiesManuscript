bound.texas <- getbb("McLennan County, Texas, USA",
                     featuretype = "settelement",
                     format_out = "sf_polygon"
)


highway.motorway<- query(key="highway", value="motorway", bbox=bound.texas)
highway.trunk<- query(key="highway", value="trunk", bbox=bound.texas)

highway.primary<- query(key="highway", value="primary", bbox=bound.texas)
highway.secondary<- query(key="highway", value="secondary", bbox=bound.texas)
highway.tertiary<- query(key="highway", value="tertiary", bbox=bound.texas)

bridges<- query(key="bridge", value=available_tags("bridge"), bbox=bound.texas)
bridges.filtered <- bridges$osm_lines %>% select(osm_id, name, highway, maxspeed, maxweight, maxweight.signed, maxweight.source, maxweightrating)

bridges$osm_lines %>% filter(highway=="motorway")


# Plot with leaflet
roads<- leaflet() %>%
  # addProviderTiles("BasemapAT.grau") %>%
  # addProviderTiles("Stamen.TerrainBackground") %>%
  # addProviderTiles("Stamen.TonerBackground") %>%
  addTiles() %>%
  # setView(14.3122, 46.636, zoom = 9) %>%
  addMeasure() %>%
  addScaleBar() %>%
  addPolygons(data = bound.texas, fillOpacity = 0.3, weight = 1.2, color = "#444444", smoothFactor = 0.5) %>%
  addPolylines(data = highway.motorway$osm_lines, fill = NA, color = "red") %>%
  addPolylines(data = highway.trunk$osm_lines, fill = NA, color = "blue") %>%
  addPolylines(data = highway.primary$osm_lines, fill = NA, color = "orange") %>%
  addPolylines(data = highway.secondary$osm_lines, fill = NA, color = "yellow") %>%
addPolylines(data = highway.tertiary$osm_lines, fill = NA, color = "green") %>%
addPolylines(data = bridges$osm_lines, fill = NA, color = "black") 

saveWidget(roads,  "waco.html",  selfcontained = TRUE)

