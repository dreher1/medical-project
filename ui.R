library(shiny)
library(leaflet)

fluidPage(
  leafletOutput("map", width = "100%", height = 600)
)

