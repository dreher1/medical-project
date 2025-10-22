# library(shiny)
# library(leaflet)
# 
# fluidPage(
#   leafletOutput("map", width = "100%", height = 600)
# )
library(shiny)
library(markdown)
library(leaflet)
library(reactable)

# Define custom royal blue theme colors
royal_blue <- "#4169E1"   # main royal blue
light_blue <- "#E8F0FF"   # very light background tone
dark_blue  <- "#1E3A8A"   # darker navy accent


#state dropdown choices (includes DC & PR)
state_choices <- c("All states (USA)", sort(c(state.name, "District of Columbia")))


shinyUI(
  navbarPage(
    title = "BIOL-185 Project - Melanoma Case Studies",
    id = "main_navbar",
    inverse = TRUE,  # makes navbar dark-themed by default
    
    # Apply a royal blue background color to the navbar
    header = tags$style(HTML(paste0("
      .navbar {
        background-color: ", royal_blue, ";
        border-color: ", royal_blue, ";
      }
      .navbar-brand, .navbar-nav li a {
        color: white !important;
        font-weight: 500;
      }
      .navbar-nav li a:hover {
        background-color: ", dark_blue, " !important;
      }
      body {
        background-color: ", light_blue, ";
        color: #0A0A0A;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      h2, h3 {
        color: ", dark_blue, ";
      }
      .well, .panel {
        background-color: white !important;
        border: 1px solid ", royal_blue, ";
        border-radius: 10px;
        box-shadow: 0 0 8px rgba(0,0,0,0.05);
      }
    "))),
    
    # ---- Home Tab ----
    tabPanel(
      "Home",
      fluidPage(
        h2("Home"),
        includeMarkdown("home.md"),
        textOutput("home_text")
      )
    ),
    
    # ---- Visualizations Tab ----
    tabPanel(
      "Visualizations",
      fluidPage(
        h2("Visualizations"),
        
        # Interactive Map
        h3("Interactive Map"),
        #ADDED: dropdown under Visualizations to focus the map
        selectInput(
          inputId = "state_select",
          label   = "Focus state:",
          choices = state_choices,
          selected = "All states (USA)",
          width = "300px"
        ),
        leafletOutput("map", height = "400px")
      )
    ),
    
    # ---- Data Explorer Tab ----
    tabPanel(
      "Data Explorer",
      fluidPage(
        h2("Data Explorer"),
        p("Explore the melanoma_table interactively:"),
        reactableOutput("data_table", height = 500),
        br(),
        textOutput("data_summary")
      )
    )
  )
)