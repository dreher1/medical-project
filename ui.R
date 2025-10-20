# library(shiny)
# library(leaflet)
# 
# fluidPage(
#   leafletOutput("map", width = "100%", height = 600)
# )
library(shiny)
library(markdown)
library(leaflet)

shinyUI(
  navbarPage(
    title = "Example NavBar App",
    
    tabPanel(
      "Home",
      fluidPage(
        h2("Home"),
        includeMarkdown("home.md"),  # You can include a markdown file here if desired
        textOutput("home_text")
      )
    ),
    
    tabPanel(
      "Visualizations",
      fluidPage(
        h2("Visualizations"),
        p("Placeholder for visual outputs."),
        plotOutput("viz_plot"),
        uiOutput("viz_note"),
        
        # ---- INSERT LEAFLET MAP (UI) HERE ----
        h3("Map Placeholder"),
        leafletOutput("map", height = "400px")
        # --------------------------------------
      )
    ),
    
    tabPanel(
      "Data Explorer",
      fluidPage(
        h2("Data Explorer"),
        p("Placeholder for data summary or text output."),
        textOutput("data_summary")
      )
    )
  )
)