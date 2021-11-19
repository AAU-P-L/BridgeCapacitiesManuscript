library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

library(sf)
library(fontawesome)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
# zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]





function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "TilesStamenLite") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "TilesSatelite") %>%
      addProviderTiles(providers$Stamen.Toner, group = "TilesStamen") %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "TilesEsriGray") %>%
      addTiles(group = "TilesOSM") %>%
      addPolylines(data = network %>% filter(highway %in% c("motorway", "trunk", "motorway_link", "trunk_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "roads_motorway") %>%
      addPolylines(data = network %>% filter(highway %in% c("primary", "primary_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "roads_primary") %>%
      addPolylines(data = network %>% filter(highway %in% c("secondary", "secondary_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "roads_secondary") %>%
      addPolylines(data = network %>% filter(highway %in% c("tertiary", "tertiary_link")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "roads_tertiary") %>%
      addPolylines(data = network %>% filter(highway %in% c("unclassified")), fillOpacity = 0.3, weight = 2, color = "#444444", smoothFactor = 0.5, group = "roads_unclassified") %>%
      addAwesomeMarkers(data = site.source, icon = icoLst$start) %>%
      addAwesomeMarkers(data = site.dest, icon = icoLst$finish) %>%
      addPolylines(data = route$route, fillOpacity = 0.9, weight = 5, color = "red", smoothFactor = 0.5, group = "path") 
      

  })

  # route$length
  output$outLength <- renderText(paste("Length [m]",round(as.double(route$length),1)))
  
  
  observe({
    show <- input$motorwayShowID

    if (show == TRUE) {
      leafletProxy("map") %>% showGroup("roads_motorway")
    }
    else {
      leafletProxy("map") %>% hideGroup("roads_motorway")
    }
  })


  observe({
    show <- input$primaryShowID

    if (show == TRUE) {
      leafletProxy("map") %>% showGroup("roads_primary")
    }
    else {
      leafletProxy("map") %>% hideGroup("roads_primary")
    }
  })

  observe({
    show <- input$secondaryShowID

    if (show == TRUE) {
      leafletProxy("map") %>% showGroup("roads_secondary")
    }
    else {
      leafletProxy("map") %>% hideGroup("roads_secondary")
    }
  })



  observe({
    show <- input$tertiaryShowID

    if (show == TRUE) {
      leafletProxy("map") %>% showGroup("roads_tertiary")
    }
    else {
      leafletProxy("map") %>% hideGroup("roads_tertiary")
    }
  })



  observe({
    show <- input$unclassifiedShowID

    if (show == TRUE) {
      leafletProxy("map") %>% showGroup("roads_unclassified")
    }
    else {
      leafletProxy("map") %>% hideGroup("roads_unclassified")
    }
  })





  observe({
    if (input$mapTypeID == "OSM") {
      leafletProxy("map") %>%
        showGroup("TilesOSM") %>%
        hideGroup("TilesSatelite") %>%
        hideGroup("TilesStamenLite") %>%
        hideGroup("TilesStamen") %>%
        hideGroup("TilesEsriGray")
    }
    if (input$mapTypeID == "Satelite") {
      leafletProxy("map") %>%
        showGroup("TilesSatelite") %>%
        hideGroup("TilesOSM") %>%
        hideGroup("TilesStamenLite") %>%
        hideGroup("TilesStamen") %>%
        hideGroup("TilesEsriGray")
    }

    if (input$mapTypeID == "Stamen.TonerLite") {
      leafletProxy("map") %>%
        showGroup("TilesStamenLite") %>%
        hideGroup("TilesSatelite") %>%
        hideGroup("TilesOSM") %>%
        hideGroup("TilesStamen") %>%
        hideGroup("TilesEsriGray")
    }

    if (input$mapTypeID == "Stamen.Toner") {
      leafletProxy("map") %>%
        showGroup("TilesStamen") %>%
        hideGroup("TilesSatelite") %>%
        hideGroup("TilesOSM") %>%
        hideGroup("TilesStamenLite") %>%
        hideGroup("TilesEsriGray")
    }
    if (input$mapTypeID == "EsriGray") {
      leafletProxy("map") %>%
        showGroup("TilesEsriGray") %>%
        hideGroup("TilesSatelite") %>%
        hideGroup("TilesOSM") %>%
        hideGroup("TilesStamenLite") %>%
        hideGroup("TilesStamen")
    }






    # sizeBy <- input$size
    #
    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("viridis", colorData)
    # } else {
    #   colorData <- zipdata[[colorBy]]
    #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    # }
    #
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }
    #
    # leafletProxy("map", data = zipdata) %>%
    #   clearShapes() %>%
    #   addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
    #              stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #   addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #             layerId="colorLegend")
  })



  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # observe({
  #   colorBy <- input$color
  #   sizeBy <- input$size
  #
  #   if (colorBy == "superzip") {
  #     # Color and palette are treated specially in the "superzip" case, because
  #     # the values are categorical instead of continuous.
  #     colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
  #     pal <- colorFactor("viridis", colorData)
  #   } else {
  #     colorData <- zipdata[[colorBy]]
  #     pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  #   }
  #
  #   if (sizeBy == "superzip") {
  #     # Radius is treated specially in the "superzip" case.
  #     radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  #   } else {
  #     radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  #   }
  #
  #   leafletProxy("map", data = zipdata) %>%
  #     clearShapes() %>%
  #     addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
  #       stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
  #     addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
  #       layerId="colorLegend")
  # })

  # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }

  # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })
}
