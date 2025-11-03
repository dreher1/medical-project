library(shiny)
library(markdown)
library(leaflet)
library(reactable)

# Define custom colors taken from an online template
royal_blue <- "#4169E1" 
light_blue <- "#E8F0FF" 
dark_blue  <- "#1E3A8A"


#state dropdown choices (includes DC)
state_choices <- c("All states (USA)", sort(c(state.name, "District of Columbia")))

#create the user interface establishment
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
    
    # home tab
    tabPanel(
      "Home",
      fluidPage(
        h2("Home"),
        includeMarkdown("home.md"),
        textOutput("home_text")
      )
    ),
    
    # Visualizations tab
    tabPanel(
      "Visualizations",
      fluidPage(
        h2("Visualizations"),
        h3("Interactive Map"),
        
        # State dropdown
        selectInput(
          inputId = "state_select",
          label   = "Focus state:",
          choices = state_choices,
          selected = "All states (USA)",
          width = "300px"
        ),
        
        #check boxes
        checkboxGroupInput(
          inputId = "viz_options",
          label   = "Show data layers:",
          choices = c(
            "Melanoma by County",
            "UV Measurement (wmh2)",
            "UV vs Melanoma correlation"
          ),
          selected = character(0)   
        ),
        
        leafletOutput("map", height = "400px")
      )
    ),
    
    # Data Explorer tab
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