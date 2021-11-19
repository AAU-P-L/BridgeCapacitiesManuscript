library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage("OHC Planner",
  id = "nav",
  tabPanel(
    "Interactive map",
    div(
      class = "outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width = "100%", height = "100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "tripControls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Trip Configuration"),
        selectInput(
          "tripID",
          "Trip",
          c("trip1", "trip2", "trip3"),
          selected = "trip1"
        ),
        sliderInput(
          "weigthSlider",
          "Truck Weight [metric tons]",
          10,
          90,
          60,
          step = 1,
          round = FALSE,
          ticks = 5
        ),
        sliderInput(
          "SlopeSlider",
          "Maximal Slope [%]",
          5,
          37.5,
          25,
          step = 1,
          round = FALSE,
          ticks = 5
        )
      ),
      
      
      absolutePanel(
        id = "mapControls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 440, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Map Config"),
        selectInput("mapTypeID", "MapType", c("OSM", "Satelite", "Stamen.Toner", "Stamen.TonerLite", "EsriGray")),
        checkboxInput("motorwayShowID", "Motorways", value = TRUE, width = NULL),
        checkboxInput("primaryShowID", "Primary Roads", value = TRUE, width = NULL),
        checkboxInput("secondaryShowID", "Secondary Roads", value = TRUE, width = NULL),
        checkboxInput("tertiaryShowID", "Tertiary Roads", value = TRUE, width = NULL),
        checkboxInput("unclassifiedShowID", "Unclassified Roads", value = TRUE, width = NULL),
      ),
      absolutePanel(
        id = "Summary", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 800, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Summary"),
        textOutput("outLength")
        
      ),
 
      
      
      tags$div(
        id = "cite",
        "OHC Transport Planer; by Christian Truden (2021)."
      )
    )
  ),
  conditionalPanel("false", icon("crosshair"))
)
